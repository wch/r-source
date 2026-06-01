# Tracing Demo Harness (R + DTrace)

This directory contains a reproducible demo harness for running a selected set of R `demo()` workloads while collecting probe data and generating a PDF report.

## What This Harness Produces

For each run, `run-demo-trace.sh` creates a timestamped directory under `tracing_runs/` with:

- `implementation-plan.md` and `implementation-plan.pdf`
- `demo.log` (live demo transcript + markers)
- `rtrace.log` (R provider probe stream)
- `sched.log` (scheduler proxy stream)
- `summary.csv` (machine-readable metrics)
- `trace-report-annotated.pdf` (human-readable report)
- `trace-heatmap.pdf` (time-binned activity heatmaps)

## Linux VM Harness (Lima + bpftrace)

For a Linux analogue of this workflow on macOS hosts, use:

- `tools/tracing-demo/linux/run-linux-vm-trace.sh`

This path provisions an Ubuntu `arm64` Lima VM with 16GiB memory and host CPU
count minus two, installs Linux tracing/build dependencies, builds R with Linux
USDT probes (`--with-ebpf`), and captures `rtrace` probe events via `bpftrace`.

Install host prerequisites with the bundle fragment:

- `brew bundle --file tools/tracing-demo/Brewfile.lima-linux.fragment`

See Linux-specific docs:

- `tools/tracing-demo/linux/README.md`
- `*.err` files for each collector and probe list
- `run-meta.txt` and `artifacts.txt`

## Cross-Platform Architecture

The harness has one instrumentation contract and two backend collectors.

- Contract layer: `rtrace` USDT probe names and payload shape emitted by R.
- Backend layer (macOS): DTrace scripts consume `rtrace` provider events.
- Backend layer (Linux): bpftrace scripts consume the same `rtrace` USDT events.
- Reporting layer: `tools/tracing-demo/build_trace_report.R` normalizes backend-specific
   log details and produces one common summary/report format.

In practice, this means the semantic event stream is shared, while probe plumbing and
privilege/runtime constraints differ by platform.

## Scale Guidance: Local Analysis vs Observability Backend

Use the same `rtrace` probe contract for both lanes, but choose the lane based on scale.

- Small scale (developer workstation, targeted experiments): local log files plus
   R-native reports (`summary.csv`, heatmaps, annotated PDF) are fast to iterate on
   and excellent for deep analysis.
- Larger scale (many runs, many hosts, longer retention windows): stream probe
   events to a dedicated telemetry backend (for example OpenTelemetry collector +
   Jaeger UI) instead of relying only on local disk logs.

Recommended operating model:

- Keep R report generation as the canonical reproducible analysis artifact.
- Add OTel export for interactive tracing and fleet-scale observability.
- Treat Jaeger and R as complementary tools: Jaeger for live trace navigation,
   R for cross-run statistical analysis and publication-quality visualizations.

## DTrace vs bpftrace Differences

| Topic | macOS harness | Linux harness |
| --- | --- | --- |
| Collector technology | DTrace (`rtrace_capture.d`, scheduler probes/fallback) | bpftrace (`rtrace_capture.bt`, `sched_capture.bt`) |
| Probe source | `rtrace` provider probes from R | Linux USDT probes from same `rtrace` points |
| Build mode | standard macOS flow in this tree | `./configure --with-ebpf` with `sys/sdt.h` |
| Scheduler view | prefers off-cpu if available; otherwise SIP-safe on-cpu sampling proxy | scheduler events from bpftrace script in guest kernel context |
| Privilege constraints | SIP can block scheduler/syscall probes; requires `sudo -n` collector attach | relies on guest kernel support (>= 5.15) and bpftrace availability |
| Run topology | local host process tracing | Lima Ubuntu guest tracing of guest-built R |
| Output contract | normalized to common `summary.csv` + annotated PDF | normalized to the same `summary.csv` + annotated PDF |

## How rtrace Encapsulates Both Backends

`rtrace` is the stable observability surface. The harness does not depend on backend-
specific probe names for domain semantics; it depends on `rtrace` events and then
maps platform capture output into a shared schema.

- Shared semantics: probe names (for example eval/native/gc events), pid/tid/cpu,
   and wall-clock ordering.
- Backend-specific capture formats: DTrace and bpftrace emit different raw text
   layouts and may differ in timestamp column naming.
- Normalization step: `build_trace_report.R` aligns those differences (for example
   timestamp column normalization) before computing metrics and rendering charts.

This is why report artifacts are now comparable across macOS and Linux even though
collection mechanisms differ.

## Quick Start

1. Build and prepare local runtime (if not already prepared):
   - `make -C src/modules -j4`
   - `make -C src/library -j4`
   - Ensure helper paths exist:
     - `bin/R` (from `src/scripts/R.sh`)
     - `bin/mkinstalldirs` executable
     - `include -> src/include` symlink

2. Refresh sudo credentials for DTrace collectors:
   - `sudo -v`

3. Run the harness:
   - `tools/tracing-demo/run-demo-trace.sh`

4. Open newest report:
   - `open "$(ls -td tracing_runs/* | head -n 1)/trace-report-annotated.pdf"`

## Data Sources and Semantics

### `demo.log`

`demo_trace_runner.R` emits markers:

- `TRACE_RUN_START`
- `DEMO_START`
- `DEMO_END`
- `TRACE_RUN_END`

Markers include UTC wall-clock timestamps, package, demo topic, status, and elapsed seconds. These windows are used for report correlation.

### `rtrace.log`

Captured from `rtrace` provider probes (`rtrace_capture.d`). Current stream includes high-volume eval probes and selected native/gc probes.

Fields:

- `wall_ns` (wall timestamp in ns)
- `probe`
- `pid`, `tid`, `cpu`
- `arg0`, `arg1`, `arg2`

### `sched.log` (SIP-safe proxy mode)

On this macOS host, true scheduler `off-cpu` probes are unavailable due SIP. The collector falls back to profile sampling (`profile-199`) and emits:

- `event=oncpu-sample`
- `value_us=0` (reserved for future proxy types)

The report computes proxy metrics from sample timing:

- `oncpu_sample_count`
- `oncpu_sample_rate_hz`
- `oncpu_sample_gap_p95_us`

## Report Interpretation

`build_trace_report.R` generates:

1. Summary page
2. Demo timeline
3. Probe count bar chart
4. Off-CPU or fallback On-CPU gap proxy chart
5. Demo-mapped contention/proxy summary page

If off-CPU data exists, it is preferred. Otherwise, the fallback proxy path is used.

## Known Limitations

### macOS SIP restrictions

Common limitations under SIP-enabled systems:

- `sched:::off-cpu` and related scheduler probes may not be available.
- `syscall:::*` probes may be unavailable.
- `dtrace -l -P rtrace -c <cmd>` may fail with `Operation not permitted` in probe-list preflight.

Implications:

- `offcpu_*` metrics in `summary.csv` remain `NA`/0.
- `sched.log` may contain only profile-derived proxy events.

### Privilege model

Collectors are started with `sudo -n`. If credentials are not cached:

- collectors can fail with `sudo: a password is required`
- run can still finish demo execution, but trace data may be partial/empty

### Demo set behavior

Some demo topics intentionally/commonly fail in this harness context (for example missing demo topics or object-specific assumptions). Failures are recorded in `DEMO_END status` and do not abort the overall run.

### Timezone warnings

Some demo code paths may emit timezone warnings (for example unknown timezone names in this local environment). These are expected noise unless they halt execution.

## Reproducibility Checklist

Before sharing results externally, include:

- OS version and SIP status
- `dtrace -V` output
- whether `sudo -v` was refreshed immediately before run
- full `run-meta.txt`
- `summary.csv`
- collector stderr files (`rtrace.err`, `sched.err`, `probe-list.err`)
- exact run directory path and timestamp

## Troubleshooting

### Empty `rtrace.log`

- Confirm sudo cache: `sudo -v`
- Confirm provider visibility in this runtime context
- Check `rtrace.err` and `probe-list.err`

### Empty `sched.log`

- Check `sched.err` for SIP probe restrictions
- If SIP blocks scheduler probes, fallback should still produce `oncpu-sample`
- If still empty, verify collector actually attached to live R PID

### Report missing or stale

- Rebuild report manually:
  - `./bin/R --vanilla -q -f tools/tracing-demo/build_trace_report.R --args <run_dir>`

## Extending for Wider Use

Recommended next steps for portability:

- Add a `--mode` flag (`strict-offcpu`, `proxy-only`, `auto`) to `run-demo-trace.sh`
- Extend Linux/eBPF backend to support x86_64 emulated guests and native Linux hosts
- Add probe capability detection and explicit mode banner at run start
- Add a compact JSON report alongside `summary.csv`
- Add CI smoke check that validates parser/report behavior with fixture logs
