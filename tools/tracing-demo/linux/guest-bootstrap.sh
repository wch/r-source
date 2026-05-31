#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-}"
RUN_DIR="${2:-}"

if [[ -z "$ROOT_DIR" || -z "$RUN_DIR" ]]; then
  echo "usage: $0 <root_dir> <run_dir>"
  exit 2
fi

if [[ ! -d "$ROOT_DIR" ]]; then
  echo "ERROR: root directory not found in guest: $ROOT_DIR"
  exit 1
fi

mkdir -p "$RUN_DIR"

KERNEL_REL="$(uname -r)"
KERNEL_MAJOR="${KERNEL_REL%%.*}"
KERNEL_MINOR="$(echo "$KERNEL_REL" | cut -d. -f2)"

if [[ "$KERNEL_MAJOR" -lt 5 ]] || [[ "$KERNEL_MAJOR" -eq 5 && "$KERNEL_MINOR" -lt 15 ]]; then
  echo "ERROR: kernel $KERNEL_REL is below required minimum 5.15 for this harness" >&2
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive
sudo apt-get update
sudo apt-get install -y --no-install-recommends \
  bpftrace \
  bison \
  build-essential \
  ca-certificates \
  curl \
  flex \
  gfortran \
  git \
  libcairo2-dev \
  libcurl4-openssl-dev \
  libicu-dev \
  libjpeg-dev \
  liblapack-dev \
  libpcre2-dev \
  libpng-dev \
  libreadline-dev \
  libtiff5-dev \
  libx11-dev \
  libxml2-dev \
  libxt-dev \
  pkg-config \
  systemtap-sdt-dev \
  tcl-dev \
  tk-dev \
  zlib1g-dev

if ! printf '#include <sys/sdt.h>\n' | gcc -E - >/dev/null 2>&1; then
  echo "ERROR: <sys/sdt.h> is not resolvable by the compiler include path" >&2
  exit 1
fi

SDT_HEADER_PATH="$(dpkg -L systemtap-sdt-dev | awk '/\/sys\/sdt\.h$/ {print; exit}')"

if ! command -v bpftrace >/dev/null 2>&1; then
  echo "ERROR: bpftrace not installed" >&2
  exit 1
fi

{
  echo "guest_kernel=$KERNEL_REL"
  echo "guest_bpftrace=$(bpftrace --version | head -n 1)"
  echo "guest_sdt_header=${SDT_HEADER_PATH:-unknown}"
  echo "guest_bootstrap_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "guest_user=$(id -un)"
} > "$RUN_DIR/linux-guest-bootstrap-meta.txt"

echo "[INFO] guest bootstrap complete"