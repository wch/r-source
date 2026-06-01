#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-}"
RUN_DIR="${2:-}"

if [[ -z "$ROOT_DIR" || -z "$RUN_DIR" ]]; then
  echo "usage: $0 <root_dir> <run_dir>"
  exit 2
fi

TOOLS_DIR="$ROOT_DIR/tools/tracing-demo"
LINUX_TOOLS_DIR="$TOOLS_DIR/linux"
BUILD_DIR="${R_BUILD_DIR:-$ROOT_DIR/.linux-ebpf-build}"
INSTALL_RECOMMENDED="${INSTALL_RECOMMENDED:-1}"
R_LAUNCH="$BUILD_DIR/bin/R"
R_USDT_TARGET="$BUILD_DIR/bin/exec/R"
RUN_ID="$(basename "$RUN_DIR")"
USDT_ENABLEMENT_MODE="prebuilt-ebpf"
USDT_ENABLEMENT_NOTE="reused-existing-build"

ebpf_usdt_enabled() {
  [[ -f "$BUILD_DIR/src/main/Makefile" ]] && \
    grep -q '^EBPF_CPPFLAGS = -DHAVE_EBPF_USDT' "$BUILD_DIR/src/main/Makefile"
}

BPFTRACE_R_PID=""
BPFTRACE_S_PID=""

cleanup_collectors() {
  if [[ -n "$BPFTRACE_R_PID" || -n "$BPFTRACE_S_PID" ]]; then
    kill -INT ${BPFTRACE_R_PID:-} ${BPFTRACE_S_PID:-} >/dev/null 2>&1 || true
  fi
}

validate_run_meta_provenance() {
  local run_meta_file="$1"

  if [[ ! -f "$run_meta_file" ]]; then
    echo "ERROR: run metadata file is missing: $run_meta_file" >&2
    return 1
  fi

  if ! grep -Eq '^usdt_enablement_mode=.+' "$run_meta_file"; then
    echo "ERROR: missing or empty usdt_enablement_mode in $run_meta_file" >&2
    return 1
  fi

  if ! grep -Eq '^usdt_enablement_note=.+' "$run_meta_file"; then
    echo "ERROR: missing or empty usdt_enablement_note in $run_meta_file" >&2
    return 1
  fi
}

trap cleanup_collectors EXIT INT TERM

mkdir -p "$RUN_DIR"

PLAN_MD="$RUN_DIR/implementation-plan.md"
PLAN_PDF="$RUN_DIR/implementation-plan.pdf"
DEMO_LOG="$RUN_DIR/demo.log"
RTRACE_LOG="$RUN_DIR/rtrace.log"
SCHED_LOG="$RUN_DIR/sched.log"
RTRACE_ERR="$RUN_DIR/rtrace.err"
SCHED_ERR="$RUN_DIR/sched.err"
PROBE_LIST="$RUN_DIR/rtrace-probe-list.txt"
RUN_META="$RUN_DIR/run-meta.txt"
SUMMARY_CSV="$RUN_DIR/summary.csv"
TRACE_REPORT_PDF="$RUN_DIR/trace-report-annotated.pdf"
HEATMAP_PDF="$RUN_DIR/trace-heatmap.pdf"

cat > "$PLAN_MD" <<'EOF'
# Implementation Plan: Linux VM eBPF Tracing Session

1. Validate kernel and userspace tracing dependencies.
2. Ensure R is configured for Linux USDT probes (`--with-ebpf`).
3. Build R if needed.
4. Start selected `demo()` workload with marker output.
5. Attach bpftrace to `rtrace` USDT probes for the demo process.
6. Capture profile-based on-CPU proxy samples in parallel.
7. Stop tracing collectors after the workload completes.
8. Emit summary CSV and metadata artifacts.
EOF

"$R_LAUNCH" --vanilla -q -f "$TOOLS_DIR/render_plan_pdf.R" --args "$PLAN_MD" "$PLAN_PDF" >/dev/null

echo "[INFO] Plan PDF generated: $PLAN_PDF"

if [[ ! -x "$R_LAUNCH" || ! -x "$R_USDT_TARGET" ]] || \
   ! "$R_LAUNCH" --version >/dev/null 2>&1 || \
   ! ebpf_usdt_enabled; then
  if [[ -x "$R_LAUNCH" && -x "$R_USDT_TARGET" ]] && ! ebpf_usdt_enabled; then
    echo "[INFO] Existing Linux build found, but eBPF USDT is disabled; reconfiguring at $BUILD_DIR..."
  else
    echo "[INFO] No usable Linux R build found; configuring out-of-tree build at $BUILD_DIR..."
  fi
  mkdir -p "$BUILD_DIR"
  if [[ ! -f "$ROOT_DIR/SVNINFO" ]]; then
    {
      echo "Revision: 0"
      echo "Last Changed Date: $(date -u '+%Y-%m-%d %H:%M:%S +0000')"
    } > "$ROOT_DIR/SVNINFO"
  fi
  # Stale host build outputs in the source tree can confuse VPATH builds.
  find "$ROOT_DIR/src" -type f \( -name '*.o' -o -name '*.d' -o -name '*.so' -o -name '*.a' \) -delete
  (
    cd "$BUILD_DIR"
    rm -f config.cache
    MULTIARCH_INCLUDE="/usr/include/$(dpkg-architecture -qDEB_HOST_MULTIARCH)"
    CPPFLAGS="-I${MULTIARCH_INCLUDE}" "$ROOT_DIR/configure" --enable-tracing --with-ebpf --without-recommended-packages
    USDT_ENABLEMENT_MODE="configure-ebpf"
    USDT_ENABLEMENT_NOTE="configure-enabled-ebpf"
    if ! grep -q '^EBPF_CPPFLAGS = -DHAVE_EBPF_USDT' "$BUILD_DIR/src/main/Makefile"; then
      if printf '#include <sys/sdt.h>\nint main(void){return 0;}\n' | \
        ${CC:-gcc} -I"${MULTIARCH_INCLUDE}" -x c - -o /tmp/r-ebpf-usdt-check >/dev/null 2>&1; then
        echo "[WARN] configure did not enable Linux eBPF USDT; forcing EBPF_CPPFLAGS after successful sys/sdt.h probe"
        sed -i.bak 's/^#EBPF_CPPFLAGS = -DHAVE_EBPF_USDT$/EBPF_CPPFLAGS = -DHAVE_EBPF_USDT/' "$BUILD_DIR/src/main/Makefile"
        USDT_ENABLEMENT_MODE="forced-ebpf-cppflags"
        USDT_ENABLEMENT_NOTE="configure-missed-sys-sdt-h"
      else
        echo "ERROR: Linux eBPF USDT support was not enabled during configure" >&2
        exit 1
      fi
    fi
    if ! grep -q '^EBPF_CPPFLAGS = -DHAVE_EBPF_USDT' "$BUILD_DIR/src/main/Makefile"; then
      echo "ERROR: failed to force-enable Linux eBPF USDT build flags" >&2
      exit 1
    fi
    make -C "$BUILD_DIR/src/main" clean >/dev/null
    if ! make -j"$(nproc)"; then
      echo "[WARN] Parallel make failed; retrying serial build for stability..."
      make -j1
    fi
    printf '%s\n' "$USDT_ENABLEMENT_MODE" > "$BUILD_DIR/.usdt-enable-mode"
    printf '%s\n' "$USDT_ENABLEMENT_NOTE" > "$BUILD_DIR/.usdt-enable-note"
  )
fi

if [[ ! -x "$R_LAUNCH" || ! -x "$R_USDT_TARGET" ]]; then
  echo "ERROR: built R interpreter not found under $BUILD_DIR/bin" >&2
  exit 1
fi

if [[ -f "$BUILD_DIR/.usdt-enable-mode" ]]; then
  USDT_ENABLEMENT_MODE="$(head -n 1 "$BUILD_DIR/.usdt-enable-mode")"
fi
if [[ -f "$BUILD_DIR/.usdt-enable-note" ]]; then
  USDT_ENABLEMENT_NOTE="$(head -n 1 "$BUILD_DIR/.usdt-enable-note")"
fi

if [[ "$INSTALL_RECOMMENDED" == "1" ]]; then
  export R_BUILD_LIBRARY="$BUILD_DIR/library"
  export R_RECOMMENDED_PACKAGES="boot class cluster codetools foreign KernSmooth lattice MASS Matrix mgcv nlme nnet rpart spatial survival"
  "$R_LAUNCH" --vanilla -q <<'EOF'
build_lib <- Sys.getenv("R_BUILD_LIBRARY")
pkgs <- strsplit(Sys.getenv("R_RECOMMENDED_PACKAGES"), " +")[[1]]
installed <- rownames(installed.packages(lib.loc = build_lib))
missing <- setdiff(pkgs, installed)
if (length(missing) > 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(missing, lib = build_lib, dependencies = FALSE)
}
EOF
fi

if ! command -v bpftrace >/dev/null 2>&1; then
  echo "ERROR: bpftrace is not available in the guest" >&2
  exit 1
fi

{
  echo "run_id=$RUN_ID"
  echo "run_dir=$RUN_DIR"
  echo "started_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "r_launch=$R_LAUNCH"
  echo "r_usdt_target=$R_USDT_TARGET"
  echo "usdt_enablement_mode=$USDT_ENABLEMENT_MODE"
  echo "usdt_enablement_note=$USDT_ENABLEMENT_NOTE"
  echo "kernel=$(uname -r)"
  echo "hostname=$(hostname)"
  echo "bpftrace_version=$(bpftrace --version | head -n 1)"
} > "$RUN_META"

sudo bpftrace -l "usdt:$R_USDT_TARGET:rtrace:*" > "$PROBE_LIST" 2>"$RUN_DIR/probe-list.err" || true

"$R_LAUNCH" --vanilla -q -f "$TOOLS_DIR/demo_trace_runner.R" > "$DEMO_LOG" 2>&1 &
R_PID=$!

echo "[INFO] Demo runner PID: $R_PID"

RTRACE_BT="$RUN_DIR/rtrace_capture.bt"
SCHED_BT="$RUN_DIR/sched_capture.bt"

sed \
  -e "s|__TARGET_PID__|$R_PID|g" \
  -e "s|__R_BIN__|$R_USDT_TARGET|g" \
  "$LINUX_TOOLS_DIR/rtrace_capture.bt" > "$RTRACE_BT"

sed -e "s|__TARGET_PID__|$R_PID|g" "$LINUX_TOOLS_DIR/sched_capture.bt" > "$SCHED_BT"

sudo bpftrace "$RTRACE_BT" > "$RTRACE_LOG" 2> "$RTRACE_ERR" &
BPFTRACE_R_PID=$!

sudo bpftrace "$SCHED_BT" > "$SCHED_LOG" 2> "$SCHED_ERR" &
BPFTRACE_S_PID=$!

set +e
wait "$R_PID"
R_EXIT=$?
set -e

kill -INT "$BPFTRACE_R_PID" "$BPFTRACE_S_PID" >/dev/null 2>&1 || true
wait "$BPFTRACE_R_PID" || true
wait "$BPFTRACE_S_PID" || true

RTRACE_LINES="$(wc -l < "$RTRACE_LOG" | tr -d ' ')"
SCHED_LINES="$(wc -l < "$SCHED_LOG" | tr -d ' ')"
ONCPU_SAMPLES="$(awk -F'|' '$2=="oncpu-sample"{c++} END{print c+0}' "$SCHED_LOG" 2>/dev/null || echo 0)"

awk -F'|' '
  BEGIN { prev = 0; n = 0 }
  $2 == "oncpu-sample" {
    ts = $1
    if (prev > 0) {
      n++
      gap[n] = ts - prev
    }
    prev = ts
  }
  END {
    if (n == 0) {
      print 0
      exit
    }
    asort(gap)
    idx = int((n * 0.95) + 0.5)
    if (idx < 1) idx = 1
    if (idx > n) idx = n
    print int(gap[idx] / 1000)
  }
' "$SCHED_LOG" > "$RUN_DIR/oncpu_gap_p95_us.tmp"

ONCPU_GAP_P95_US="$(cat "$RUN_DIR/oncpu_gap_p95_us.tmp")"
rm -f "$RUN_DIR/oncpu_gap_p95_us.tmp"

{
  echo "metric,value"
  echo "run_id,$RUN_ID"
  echo "demo_exit_code,$R_EXIT"
  echo "rtrace_line_count,$RTRACE_LINES"
  echo "sched_line_count,$SCHED_LINES"
  echo "oncpu_sample_count,$ONCPU_SAMPLES"
  echo "oncpu_sample_gap_p95_us,$ONCPU_GAP_P95_US"
} > "$SUMMARY_CSV"

"$R_LAUNCH" --vanilla -q -f "$TOOLS_DIR/build_trace_report.R" --args "$RUN_DIR" >/dev/null

if [[ ! -f "$TRACE_REPORT_PDF" ]]; then
  echo "ERROR: expected trace report PDF was not generated: $TRACE_REPORT_PDF" >&2
  exit 1
fi

if [[ ! -f "$HEATMAP_PDF" ]]; then
  echo "ERROR: expected heatmap PDF was not generated: $HEATMAP_PDF" >&2
  exit 1
fi

echo "[INFO] Trace report PDF generated: $TRACE_REPORT_PDF"
echo "[INFO] Heatmap PDF generated: $HEATMAP_PDF"

echo "finished_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$RUN_META"
echo "demo_exit_code=$R_EXIT" >> "$RUN_META"

validate_run_meta_provenance "$RUN_META"

ls -1 "$RUN_DIR" > "$RUN_DIR/artifacts.txt"

echo "[INFO] Linux trace run complete: $RUN_DIR"

if [[ "$R_EXIT" -ne 0 ]]; then
  echo "ERROR: demo runner exited with code $R_EXIT" >&2
  exit "$R_EXIT"
fi

trap - EXIT INT TERM