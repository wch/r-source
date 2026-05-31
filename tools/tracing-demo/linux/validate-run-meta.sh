#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
usage: tools/tracing-demo/linux/validate-run-meta.sh [run_dir_or_run_meta]

Validates Linux tracing run metadata provenance fields:
  - usdt_enablement_mode
  - usdt_enablement_note

If no argument is provided, the script validates the latest run under:
  tracing_runs/*-linux-arm64/run-meta.txt
EOF
}

resolve_run_meta_path() {
  local input_path="${1:-}"

  if [[ -z "$input_path" ]]; then
    local latest_dir
    latest_dir="$(ls -td tracing_runs/*-linux-arm64 2>/dev/null | head -n 1 || true)"
    if [[ -z "$latest_dir" ]]; then
      echo "ERROR: no Linux tracing runs found under tracing_runs/*-linux-arm64" >&2
      return 1
    fi
    echo "$latest_dir/run-meta.txt"
    return 0
  fi

  if [[ -d "$input_path" ]]; then
    echo "$input_path/run-meta.txt"
    return 0
  fi

  echo "$input_path"
}

validate_run_meta() {
  local run_meta_file="$1"

  if [[ ! -f "$run_meta_file" ]]; then
    echo "ERROR: run metadata file not found: $run_meta_file" >&2
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

  echo "OK: Linux run metadata provenance fields are present in $run_meta_file"
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

RUN_META_PATH="$(resolve_run_meta_path "${1:-}")"
validate_run_meta "$RUN_META_PATH"
