# Linux VM Tracing Harness (Lima + Ubuntu + bpftrace)

This harness extends the macOS DTrace flow to an Ubuntu Linux VM on macOS hosts.
It provisions an `arm64` Ubuntu guest with Lima, validates a recent kernel, installs
`bpftrace`, builds R with Linux USDT probes, and runs the same demo workload used by
`tools/tracing-demo/run-demo-trace.sh`.

## How This Relates to macOS DTrace Harness

The Linux harness is the Linux backend of the same tracing model, not a separate
instrumentation design.

- macOS backend: DTrace collectors read `rtrace` provider events from host-built R.
- Linux backend: bpftrace collectors read the same `rtrace` USDT events from
   guest-built R (`--with-ebpf`).
- Shared reporting: both backends are normalized into the same `summary.csv`,
   `trace-report-annotated.pdf`, and `trace-heatmap.pdf` outputs.

This gives platform-equivalent report artifacts while preserving backend-specific
collector implementation.

## Backend Differences at a Glance

| Topic | macOS path | Linux path |
| --- | --- | --- |
| Collector | DTrace scripts | bpftrace scripts |
| Runtime constraints | SIP may limit scheduler/syscall probes | requires guest kernel support and bpftrace |
| Build/probe integration | host build path in this tree | `./configure --with-ebpf` + `sys/sdt.h` |
| Execution location | host tracing | Lima Ubuntu guest tracing |
| Artifact contract | `summary.csv` + annotated PDF + heatmap PDF | same artifact contract |

## How rtrace Encapsulates Both

`rtrace` is the common probe contract across both backends.

- R emits the same semantic probe events.
- DTrace and bpftrace capture those events through different collector syntax.
- The report builder normalizes backend formatting differences before metric
   computation, so platform reports remain directly comparable.

## Host Prerequisites

Install host dependencies via Homebrew Bundle fragment:

- `brew bundle --file tools/tracing-demo/Brewfile.lima-linux.fragment`

Required commands:

- `limactl`
- `qemu-system-aarch64`

Recommended host free disk space:

- at least `8GiB` available under your home volume

## What This Harness Produces

For each run, `run-linux-vm-trace.sh` creates a run directory (default:
`tracing_runs/<timestamp>-linux-arm64`) containing:

- `implementation-plan.md` and `implementation-plan.pdf`
- `demo.log`
- `rtrace.log`
- `sched.log`
- `trace-report-annotated.pdf`
- `trace-heatmap.pdf`
- `summary.csv`
- `rtrace-probe-list.txt`
- `run-meta.txt`
- `linux-guest-bootstrap-meta.txt`
- `linux-host-run-meta.txt`
- collector stderr files (`*.err`)

The host launcher also validates that all expected PDF artifacts exist at the end of each
run and exits non-zero if any are missing.

## Quick Start

1. Install host prerequisites:
   - `brew bundle --file tools/tracing-demo/Brewfile.lima-linux.fragment`

2. Run the Linux harness:
   - `tools/tracing-demo/linux/run-linux-vm-trace.sh`

3. Inspect latest Linux run directory:
   - `ls -td tracing_runs/*-linux-arm64 | head -n 1`

4. Validate provenance metadata (CI-friendly):
   - `tools/tracing-demo/linux/validate-run-meta.sh`
   - `tools/tracing-demo/linux/validate-run-meta.sh tracing_runs/<run-id>-linux-arm64`

## Resource Policy

Defaults in host launcher:

- memory: `16GiB`
- vCPUs: `host_cpu_count - 2` (minimum `1`)

Override memory with:

- `VM_MEMORY=20GiB tools/tracing-demo/linux/run-linux-vm-trace.sh`

Override disk size with:

- `VM_DISK=40GiB tools/tracing-demo/linux/run-linux-vm-trace.sh`

Override free-space safety threshold with:

- `MIN_FREE_GIB=4 tools/tracing-demo/linux/run-linux-vm-trace.sh`

Override instance name with:

- `INSTANCE_NAME=my-rtrace-vm tools/tracing-demo/linux/run-linux-vm-trace.sh`

Skip recommended package installation with:

- `INSTALL_RECOMMENDED=0 tools/tracing-demo/linux/run-linux-vm-trace.sh`

## Linux Probe Build Contract

On Linux, this tree uses eBPF USDT integration (`sys/sdt.h`) instead of `dtrace`.
The guest runner configures R with:

- `./configure --with-ebpf --without-recommended-packages`
- then installs recommended packages from CRAN into the guest build library by default

Bootstrap validates:

- kernel >= `5.15`
- `#include <sys/sdt.h>` resolves via compiler include paths
- `bpftrace` installed

## Notes

- The harness assumes the repo path is visible inside Lima guest mounts.
- First run may be lengthy due to package installation and R build.
- Linux guest build output is written to `.linux-ebpf-build/` at the repo root.
- If a Lima instance already exists with the configured name, it is reused.
- If you need to rebuild the instance after template changes, use `FORCE_RECREATE=1 tools/tracing-demo/linux/run-linux-vm-trace.sh`.
