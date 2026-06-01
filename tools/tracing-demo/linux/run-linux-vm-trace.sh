#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
TOOLS_DIR="$ROOT_DIR/tools/tracing-demo"
LINUX_TOOLS_DIR="$TOOLS_DIR/linux"
GUEST_HOME_MOUNT="/mnt/hosthome"

INSTANCE_NAME="${INSTANCE_NAME:-rtrace-linux-arm64}"
VM_MEMORY="${VM_MEMORY:-16GiB}"
VM_DISK="${VM_DISK:-30GiB}"
FORCE_RECREATE="${FORCE_RECREATE:-0}"
MIN_FREE_GIB="${MIN_FREE_GIB:-8}"
RUN_ID="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="${1:-$ROOT_DIR/tracing_runs/$RUN_ID-linux-arm64}"

host_to_guest_path() {
  local host_path="$1"
  local host_home
  host_home="$(cd "$HOME" && pwd)"

  if [[ "$host_path" == "$host_home" ]]; then
    echo "$GUEST_HOME_MOUNT"
    return 0
  fi

  if [[ "$host_path" == "$host_home"/* ]]; then
    echo "$GUEST_HOME_MOUNT/${host_path#"$host_home"/}"
    return 0
  fi

  return 1
}

mkdir -p "$RUN_DIR"

if ! command -v limactl >/dev/null 2>&1; then
  echo "ERROR: limactl is not installed. Install host prerequisites from $TOOLS_DIR/Brewfile.lima-linux.fragment" >&2
  exit 1
fi

if ! command -v qemu-system-aarch64 >/dev/null 2>&1; then
  echo "ERROR: qemu-system-aarch64 is not installed. Install host prerequisites from $TOOLS_DIR/Brewfile.lima-linux.fragment" >&2
  exit 1
fi

HOME_AVAIL_KB="$(df -Pk "$HOME" | awk 'NR==2 {print $4}')"
HOME_AVAIL_GIB=$((HOME_AVAIL_KB / 1024 / 1024))
if [[ "$HOME_AVAIL_GIB" -lt "$MIN_FREE_GIB" ]]; then
  echo "ERROR: insufficient free disk on host ($HOME_AVAIL_GIB GiB available; need >= $MIN_FREE_GIB GiB)." >&2
  echo "       Free space under $HOME or lower MIN_FREE_GIB if you intentionally want a smaller safety margin." >&2
  exit 1
fi

if ! GUEST_ROOT_DIR="$(host_to_guest_path "$ROOT_DIR")"; then
  echo "ERROR: workspace path must be under $HOME for Lima mount mapping: $ROOT_DIR" >&2
  exit 1
fi

if ! GUEST_RUN_DIR="$(host_to_guest_path "$RUN_DIR")"; then
  echo "ERROR: run directory must be under $HOME for Lima mount mapping: $RUN_DIR" >&2
  exit 1
fi

HOST_CPUS="$(sysctl -n hw.logicalcpu 2>/dev/null || sysctl -n hw.ncpu)"
VM_CPUS=$((HOST_CPUS - 2))
if [[ "$VM_CPUS" -lt 1 ]]; then
  VM_CPUS=1
fi

if [[ ! -f "$LINUX_TOOLS_DIR/lima-rtrace.yaml" ]]; then
  echo "ERROR: missing Lima template: $LINUX_TOOLS_DIR/lima-rtrace.yaml" >&2
  exit 1
fi

LIMA_RENDERED_CFG="$RUN_DIR/lima-rtrace.rendered.yaml"
sed \
  -e "s/__CPUS__/$VM_CPUS/g" \
  -e "s/__MEMORY__/$VM_MEMORY/g" \
  -e "s/__DISK__/$VM_DISK/g" \
  "$LINUX_TOOLS_DIR/lima-rtrace.yaml" > "$LIMA_RENDERED_CFG"

if limactl list | awk 'NR>1 {print $1}' | grep -qx "$INSTANCE_NAME"; then
  if [[ "$FORCE_RECREATE" == "1" ]]; then
    echo "[INFO] FORCE_RECREATE=1 set; deleting existing Lima instance: $INSTANCE_NAME"
    limactl delete -f "$INSTANCE_NAME" >/dev/null
  fi
fi

if limactl list | awk 'NR>1 {print $1}' | grep -qx "$INSTANCE_NAME"; then
  echo "[INFO] Reusing existing Lima instance: $INSTANCE_NAME"
  limactl start "$INSTANCE_NAME" >/dev/null
else
  echo "[INFO] Creating Lima instance: $INSTANCE_NAME"
  limactl start --name "$INSTANCE_NAME" "$LIMA_RENDERED_CFG"
fi

if ! limactl shell "$INSTANCE_NAME" -- test -d "$GUEST_ROOT_DIR"; then
  echo "ERROR: workspace path is not visible in guest: $GUEST_ROOT_DIR" >&2
  echo "       Recreate the instance with FORCE_RECREATE=1 to apply updated mount settings." >&2
  exit 1
fi

limactl shell "$INSTANCE_NAME" -- mkdir -p "$GUEST_RUN_DIR"
if ! limactl shell "$INSTANCE_NAME" -- test -d "$GUEST_RUN_DIR"; then
  echo "ERROR: run directory is not writable/visible in guest: $GUEST_RUN_DIR" >&2
  exit 1
fi

chmod +x \
  "$LINUX_TOOLS_DIR/guest-bootstrap.sh" \
  "$LINUX_TOOLS_DIR/guest-run-trace.sh"

echo "[INFO] Running guest bootstrap..."
limactl shell "$INSTANCE_NAME" -- \
  bash "$GUEST_ROOT_DIR/tools/tracing-demo/linux/guest-bootstrap.sh" "$GUEST_ROOT_DIR" "$GUEST_RUN_DIR"

echo "[INFO] Running guest trace session..."
limactl shell "$INSTANCE_NAME" -- \
  bash "$GUEST_ROOT_DIR/tools/tracing-demo/linux/guest-run-trace.sh" "$GUEST_ROOT_DIR" "$GUEST_RUN_DIR"

REQUIRED_ARTIFACTS=(
  "$RUN_DIR/implementation-plan.pdf"
  "$RUN_DIR/trace-report-annotated.pdf"
  "$RUN_DIR/trace-heatmap.pdf"
)

for artifact in "${REQUIRED_ARTIFACTS[@]}"; do
  if [[ ! -f "$artifact" ]]; then
    echo "ERROR: required artifact missing after Linux trace run: $artifact" >&2
    exit 1
  fi
done

{
  echo "host_instance_name=$INSTANCE_NAME"
  echo "host_cpu_total=$HOST_CPUS"
  echo "host_cpu_assigned=$VM_CPUS"
  echo "host_memory_assigned=$VM_MEMORY"
  echo "host_disk_assigned=$VM_DISK"
  echo "host_run_dir=$RUN_DIR"
  echo "host_finished_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$RUN_DIR/linux-host-run-meta.txt"

echo "[INFO] Done. Artifacts in: $RUN_DIR"