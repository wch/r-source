#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TOOLS_DIR="$ROOT_DIR/tools/tracing-demo"
RUN_ID="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="${1:-$ROOT_DIR/tracing_runs/$RUN_ID}"

mkdir -p "$RUN_DIR"

PLAN_MD="$RUN_DIR/implementation-plan.md"
PLAN_PDF="$RUN_DIR/implementation-plan.pdf"
PROBE_LIST="$RUN_DIR/rtrace-probe-list.txt"
DEMO_LOG="$RUN_DIR/demo.log"
RTRACE_LOG="$RUN_DIR/rtrace.log"
SCHED_LOG="$RUN_DIR/sched.log"
RTRACE_ERR="$RUN_DIR/rtrace.err"
SCHED_ERR="$RUN_DIR/sched.err"
RUN_META="$RUN_DIR/run-meta.txt"

R_BIN="$ROOT_DIR/bin/R"

if [[ ! -x "$R_BIN" ]]; then
  echo "ERROR: built R interpreter not found at $R_BIN"
  exit 1
fi

if ! command -v dtrace >/dev/null 2>&1; then
  echo "ERROR: dtrace is not available on this system."
  exit 1
fi

cat > "$PLAN_MD" <<'EOF'
# Implementation Plan: R Demo Tracing Session

1. Generate implementation plan PDF artifact first.
2. Preflight built interpreter and trace capabilities.
3. Run core demo workload with visible DEMO_START and DEMO_END markers.
4. Attach DTrace rtrace probe capture to the active R PID.
5. Attach scheduler/off-CPU capture in parallel for contention analysis.
6. Store raw logs in a timestamped run directory.
7. Correlate demo windows with probe and contention events.
8. Produce an annotated PDF report with findings and metrics.
EOF

"$R_BIN" --vanilla -q -f "$TOOLS_DIR/render_plan_pdf.R" --args "$PLAN_MD" "$PLAN_PDF" >/dev/null

echo "[INFO] Plan PDF generated: $PLAN_PDF"

if ! sudo -n true >/dev/null 2>&1; then
  echo "[WARN] sudo cache not confirmed; proceeding and relying on collector command errors if privileges are missing."
fi

echo "[INFO] Capturing probe listing..."
if ! sudo -n dtrace -q -l -P rtrace -c "$R_BIN --vanilla -q -e 'Sys.sleep(0.5); q()'" > "$PROBE_LIST" 2>"$RUN_DIR/probe-list.err"; then
  echo "[WARN] Probe listing command failed. Continuing with runtime capture."
fi

{
  echo "run_id=$RUN_ID"
  echo "run_dir=$RUN_DIR"
  echo "started_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "r_bin=$R_BIN"
  echo "hostname=$(hostname)"
} > "$RUN_META"

PIPE_PATH="$RUN_DIR/demo.pipe"
mkfifo "$PIPE_PATH"

tee "$DEMO_LOG" < "$PIPE_PATH" &
TEE_PID=$!

"$R_BIN" --vanilla -q -f "$TOOLS_DIR/demo_trace_runner.R" > "$PIPE_PATH" 2>&1 &
R_PID=$!

echo "[INFO] Demo runner PID: $R_PID"
echo "[INFO] Starting DTrace collectors..."

sudo -n dtrace -q -s "$TOOLS_DIR/rtrace_capture.d" -p "$R_PID" > "$RTRACE_LOG" 2>"$RTRACE_ERR" &
DTRACE_R_PID=$!

sudo -n dtrace -q -s "$TOOLS_DIR/sched_capture.d" -p "$R_PID" > "$SCHED_LOG" 2>"$SCHED_ERR" &
DTRACE_S_PID=$!

wait "$R_PID"
R_EXIT=$?

wait "$DTRACE_R_PID" || true
wait "$DTRACE_S_PID" || true

rm -f "$PIPE_PATH"
wait "$TEE_PID" || true

echo "[INFO] Demo runner exit code: $R_EXIT"

echo "[INFO] Building annotated PDF report..."
"$R_BIN" --vanilla -q -f "$TOOLS_DIR/build_trace_report.R" --args "$RUN_DIR" >/dev/null

echo "finished_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$RUN_META"
echo "demo_exit_code=$R_EXIT" >> "$RUN_META"

ls -1 "$RUN_DIR" > "$RUN_DIR/artifacts.txt"

echo "[INFO] Done. Artifacts in: $RUN_DIR"
