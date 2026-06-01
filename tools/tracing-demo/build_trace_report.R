args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript build_trace_report.R <run_dir>")
}

run_dir <- args[[1]]
demo_log <- file.path(run_dir, "demo.log")
rtrace_log <- file.path(run_dir, "rtrace.log")
sched_log <- file.path(run_dir, "sched.log")
report_pdf <- file.path(run_dir, "trace-report-annotated.pdf")
heatmap_pdf <- file.path(run_dir, "trace-heatmap.pdf")
summary_csv <- file.path(run_dir, "summary.csv")

stopifnot(file.exists(demo_log))

parse_time <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

lines <- readLines(demo_log, warn = FALSE)
marker_lines <- grep("^DEMO_(START|END)\\t", lines, value = TRUE)
parts <- strsplit(marker_lines, "\\t", fixed = FALSE)
parts <- parts[vapply(parts, length, integer(1)) >= 6]

markers <- data.frame(
  kind = vapply(parts, `[[`, character(1), 1),
  tstr = vapply(parts, `[[`, character(1), 2),
  pkg = vapply(parts, `[[`, character(1), 3),
  demo = vapply(parts, `[[`, character(1), 4),
  status = vapply(parts, `[[`, character(1), 5),
  elapsed = suppressWarnings(as.numeric(vapply(parts, `[[`, character(1), 6))),
  stringsAsFactors = FALSE
)
markers$ts <- parse_time(markers$tstr)

starts <- markers[markers$kind == "DEMO_START", c("pkg", "demo", "ts")]
ends <- markers[markers$kind == "DEMO_END", c("pkg", "demo", "ts", "status", "elapsed")]
names(starts)[3] <- "start_ts"
names(ends)[3] <- "end_ts"

intervals <- merge(starts, ends, by = c("pkg", "demo"), all = TRUE)
intervals <- intervals[order(intervals$start_ts), ]
if (nrow(intervals) > 0) {
  intervals$idx <- seq_len(nrow(intervals))
}

read_event_table <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) {
    return(data.frame())
  }

  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) {
    return(data.frame())
  }

  # Linux bpftrace emits a preamble line like "Attaching N probes...".
  lines <- lines[!grepl("^Attaching [0-9]+ probes\\.\\.\\.$", lines)]
  if (!length(lines)) {
    return(data.frame())
  }

  header <- lines[[1]]
  sep <- if (grepl("\\|", header, fixed = FALSE)) "|" else "\t"

  out <- tryCatch(
    read.delim(
      textConnection(lines),
      sep = sep,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) data.frame()
  )

  out
}

rtrace <- read_event_table(rtrace_log)
sched <- read_event_table(sched_log)

if (nrow(sched) > 0 && "nsecs" %in% names(sched) && !"wall_ns" %in% names(sched)) {
  names(sched)[names(sched) == "nsecs"] <- "wall_ns"
}

probe_counts <- if (nrow(rtrace) > 0) sort(table(rtrace$probe), decreasing = TRUE) else integer(0)
offcpu <- if (nrow(sched) > 0) sched[sched$event == "offcpu", , drop = FALSE] else data.frame()
oncpu <- if (nrow(sched) > 0) sched[sched$event == "oncpu-sample", , drop = FALSE] else data.frame()
syscall_us <- if (nrow(sched) > 0) sched[sched$event == "syscall-us", , drop = FALSE] else data.frame()

if (nrow(sched) > 0 && "offcpu_us" %in% names(sched) && !"value_us" %in% names(sched)) {
  names(sched)[names(sched) == "offcpu_us"] <- "value_us"
}
if (nrow(offcpu) > 0 && "offcpu_us" %in% names(offcpu) && !"value_us" %in% names(offcpu)) {
  names(offcpu)[names(offcpu) == "offcpu_us"] <- "value_us"
}
if (nrow(oncpu) > 0 && "offcpu_us" %in% names(oncpu) && !"value_us" %in% names(oncpu)) {
  names(oncpu)[names(oncpu) == "offcpu_us"] <- "value_us"
}
if (nrow(syscall_us) > 0 && "offcpu_us" %in% names(syscall_us) && !"value_us" %in% names(syscall_us)) {
  names(syscall_us)[names(syscall_us) == "offcpu_us"] <- "value_us"
}

offcpu_stats <- c(count = 0, p50_us = NA_real_, p95_us = NA_real_, max_us = NA_real_)
if (nrow(offcpu) > 0) {
  offcpu_stats[["count"]] <- nrow(offcpu)
  offcpu_stats[["p50_us"]] <- as.numeric(quantile(offcpu$value_us, 0.50, na.rm = TRUE))
  offcpu_stats[["p95_us"]] <- as.numeric(quantile(offcpu$value_us, 0.95, na.rm = TRUE))
  offcpu_stats[["max_us"]] <- max(offcpu$value_us, na.rm = TRUE)
}

run_span_sec <- NA_real_
if (nrow(intervals) > 0) {
  run_start <- min(intervals$start_ts, na.rm = TRUE)
  run_end <- max(intervals$end_ts, na.rm = TRUE)
  run_span_sec <- as.numeric(difftime(run_end, run_start, units = "secs"))
}

proxy_stats <- c(
  oncpu_sample_count = 0,
  oncpu_sample_rate_hz = NA_real_,
  oncpu_sample_gap_p95_us = NA_real_,
  syscall_count = 0,
  syscall_p95_us = NA_real_,
  blocking_syscall_count = 0
)

if (nrow(oncpu) > 1) {
  oncpu <- oncpu[order(oncpu$wall_ns), ]
  gaps_us <- diff(oncpu$wall_ns) / 1000
  proxy_stats[["oncpu_sample_gap_p95_us"]] <- as.numeric(quantile(gaps_us, 0.95, na.rm = TRUE))
}
if (nrow(oncpu) > 0) {
  proxy_stats[["oncpu_sample_count"]] <- nrow(oncpu)
  if (!is.na(run_span_sec) && run_span_sec > 0) {
    proxy_stats[["oncpu_sample_rate_hz"]] <- nrow(oncpu) / run_span_sec
  }
}
if (nrow(syscall_us) > 0) {
  proxy_stats[["syscall_count"]] <- nrow(syscall_us)
  proxy_stats[["syscall_p95_us"]] <- as.numeric(quantile(syscall_us$value_us, 0.95, na.rm = TRUE))
  proxy_stats[["blocking_syscall_count"]] <- sum(syscall_us$value_us >= 1000, na.rm = TRUE)
}

map_events_to_demo <- function(event_ns, intervals_df) {
  if (!nrow(intervals_df)) return(rep(NA_character_, length(event_ns)))
  event_ts <- as.POSIXct(event_ns / 1e9, origin = "1970-01-01", tz = "UTC")
  demo_name <- rep(NA_character_, length(event_ts))
  for (i in seq_len(nrow(intervals_df))) {
    ok <- !is.na(intervals_df$start_ts[i]) & !is.na(intervals_df$end_ts[i])
    if (!ok) next
    hit <- event_ts >= intervals_df$start_ts[i] & event_ts <= intervals_df$end_ts[i]
    demo_name[hit] <- paste(intervals_df$pkg[i], intervals_df$demo[i], sep = "::")
  }
  demo_name
}

normalize_event_ns <- function(event_ns, intervals_df) {
  if (!length(event_ns) || !nrow(intervals_df)) {
    return(list(ns = event_ns, mode = "none"))
  }

  clean <- is.finite(event_ns)
  if (!any(clean)) {
    return(list(ns = event_ns, mode = "none"))
  }

  out <- event_ns
  run_start_ns <- as.numeric(min(intervals_df$start_ts, na.rm = TRUE)) * 1e9
  run_end_ns <- as.numeric(max(intervals_df$end_ts, na.rm = TRUE)) * 1e9
  run_span_ns <- max(1, run_end_ns - run_start_ns)

  direct_hits <- sum(!is.na(map_events_to_demo(event_ns[clean], intervals_df)))
  if (direct_hits > 0) {
    return(list(ns = out, mode = "epoch"))
  }

  min_ns <- min(event_ns[clean], na.rm = TRUE)
  max_ns <- max(event_ns[clean], na.rm = TRUE)
  span_ns <- max(1, max_ns - min_ns)
  scaled <- run_start_ns + ((event_ns - min_ns) / span_ns) * run_span_ns

  scaled_hits <- sum(!is.na(map_events_to_demo(scaled[clean], intervals_df)))
  if (scaled_hits > direct_hits) {
    out <- scaled
    return(list(ns = out, mode = "scaled-monotonic"))
  }

  list(ns = out, mode = "epoch")
}

normalized_sched <- normalize_event_ns(if (nrow(sched) > 0) sched$wall_ns else numeric(0), intervals)
if (nrow(sched) > 0) {
  sched$wall_ns_demo <- normalized_sched$ns
}

if (nrow(offcpu) > 0) {
  offcpu$wall_ns_demo <- sched$wall_ns_demo[sched$event == "offcpu"]
}

if (nrow(oncpu) > 0) {
  oncpu$wall_ns_demo <- sched$wall_ns_demo[sched$event == "oncpu-sample"]
}

if (nrow(syscall_us) > 0) {
  syscall_us$wall_ns_demo <- sched$wall_ns_demo[sched$event == "syscall-us"]
}

if (nrow(offcpu) > 0) {
  offcpu$demo <- map_events_to_demo(offcpu$wall_ns_demo, intervals)
}

if (nrow(oncpu) > 0) {
  oncpu$demo <- map_events_to_demo(oncpu$wall_ns_demo, intervals)
}

if (nrow(syscall_us) > 0) {
  syscall_us$demo <- map_events_to_demo(syscall_us$wall_ns_demo, intervals)
}

demo_levels <- if (nrow(intervals) > 0) paste(intervals$pkg, intervals$demo, sep = "::") else character(0)
bin_count <- if (is.na(run_span_sec) || run_span_sec <= 0) 40L else as.integer(max(20L, min(160L, ceiling(run_span_sec * 4))))
run_start_ns <- if (nrow(intervals) > 0) min(as.numeric(intervals$start_ts) * 1e9, na.rm = TRUE) else NA_real_
run_end_ns <- if (nrow(intervals) > 0) max(as.numeric(intervals$end_ts) * 1e9, na.rm = TRUE) else NA_real_

make_heatmap_matrix <- function(event_ns, event_weight, event_demo, demo_names, bins, start_ns, end_ns) {
  if (!length(demo_names) || !is.finite(start_ns) || !is.finite(end_ns)) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }

  out <- matrix(0, nrow = length(demo_names), ncol = bins)
  if (!length(event_ns) || !length(event_demo) || !length(event_weight)) {
    return(out)
  }

  span_ns <- max(1, end_ns - start_ns)
  row_idx <- match(event_demo, demo_names)
  keep <- !is.na(row_idx) & is.finite(event_ns) & is.finite(event_weight)
  keep <- keep & event_ns >= start_ns & event_ns <= end_ns
  if (!any(keep)) {
    return(out)
  }

  row_idx <- row_idx[keep]
  ns <- event_ns[keep]
  w <- pmax(event_weight[keep], 0)
  col_idx <- floor((ns - start_ns) / span_ns * bins) + 1L
  col_idx[col_idx < 1L] <- 1L
  col_idx[col_idx > bins] <- bins

  accum <- aggregate(w, by = list(row = row_idx, col = col_idx), FUN = sum)
  out[cbind(accum$row, accum$col)] <- accum$x
  out
}

plot_heatmap_panel <- function(mat, title, demo_names, span_seconds, palette) {
  plot.new()
  title(main = title)

  if (!nrow(mat) || !ncol(mat) || !any(mat > 0)) {
    text(0.5, 0.5, "No data available for this heatmap", cex = 1.1)
    return(invisible(NULL))
  }

  z <- log10(mat + 1)
  image(
    x = seq_len(ncol(z)),
    y = seq_len(nrow(z)),
    z = t(z),
    col = palette,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(2, at = seq_len(nrow(z)), labels = demo_names, las = 2, cex.axis = 0.65)

  tick_pos <- pretty(seq_len(ncol(z)), n = 7)
  tick_pos <- tick_pos[tick_pos >= 1 & tick_pos <= ncol(z)]
  tick_sec <- ((tick_pos - 1) / max(1, ncol(z) - 1)) * span_seconds
  axis(1, at = tick_pos, labels = sprintf("%.1fs", tick_sec), cex.axis = 0.8)
  mtext("Run Timeline", side = 1, line = 2.3)
  box()
}

offcpu_mat <- make_heatmap_matrix(
  event_ns = if (nrow(offcpu) > 0) offcpu$wall_ns_demo else numeric(0),
  event_weight = if (nrow(offcpu) > 0) offcpu$value_us else numeric(0),
  event_demo = if (nrow(offcpu) > 0) offcpu$demo else character(0),
  demo_names = demo_levels,
  bins = bin_count,
  start_ns = run_start_ns,
  end_ns = run_end_ns
)

oncpu_mat <- make_heatmap_matrix(
  event_ns = if (nrow(oncpu) > 0) oncpu$wall_ns_demo else numeric(0),
  event_weight = if (nrow(oncpu) > 0) rep(1, nrow(oncpu)) else numeric(0),
  event_demo = if (nrow(oncpu) > 0) oncpu$demo else character(0),
  demo_names = demo_levels,
  bins = bin_count,
  start_ns = run_start_ns,
  end_ns = run_end_ns
)

syscall_mat <- make_heatmap_matrix(
  event_ns = if (nrow(syscall_us) > 0) syscall_us$wall_ns_demo else numeric(0),
  event_weight = if (nrow(syscall_us) > 0) syscall_us$value_us else numeric(0),
  event_demo = if (nrow(syscall_us) > 0) syscall_us$demo else character(0),
  demo_names = demo_levels,
  bins = bin_count,
  start_ns = run_start_ns,
  end_ns = run_end_ns
)

combined_mat <- log1p(offcpu_mat) + 0.6 * log1p(oncpu_mat) + 0.8 * log1p(syscall_mat)

if (nrow(combined_mat) > 0) {
  heat_cols <- colorRampPalette(c("#f7fbff", "#9ecae1", "#3182bd", "#08519c"))(128)
  pdf(heatmap_pdf, width = 11, height = 8.5, paper = "special")
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(2, 2), mar = c(4.5, 10, 3, 1.5))
  plot_heatmap_panel(combined_mat, "Combined Activity Heatmap", demo_levels, run_span_sec, heat_cols)
  plot_heatmap_panel(offcpu_mat, "Off-CPU Duration Heatmap (us)", demo_levels, run_span_sec, heat_cols)
  plot_heatmap_panel(oncpu_mat, "On-CPU Sample Density Heatmap", demo_levels, run_span_sec, heat_cols)
  plot_heatmap_panel(syscall_mat, "Syscall Duration Heatmap (us)", demo_levels, run_span_sec, heat_cols)
  par(old_par)
  dev.off()
}

top_contention <- data.frame(demo = character(0), p95_us = numeric(0), count = integer(0))
if (nrow(offcpu) > 0) {
  split_by_demo <- split(offcpu$value_us, offcpu$demo)
  split_by_demo <- split_by_demo[!is.na(names(split_by_demo))]
  if (length(split_by_demo)) {
    top_contention <- data.frame(
      demo = names(split_by_demo),
      p95_us = vapply(split_by_demo, function(x) as.numeric(quantile(x, 0.95, na.rm = TRUE)), numeric(1)),
      count = vapply(split_by_demo, length, integer(1)),
      stringsAsFactors = FALSE
    )
    top_contention <- top_contention[order(top_contention$p95_us, decreasing = TRUE), ]
    top_contention <- head(top_contention, 8)
  }
}

top_proxy <- data.frame(demo = character(0), sample_count = integer(0), syscall_p95_us = numeric(0))
if (nrow(oncpu) > 0 || nrow(syscall_us) > 0) {
  demos <- sort(unique(c(na.omit(oncpu$demo), na.omit(syscall_us$demo))))
  if (length(demos)) {
    sample_counts <- vapply(demos, function(d) sum(oncpu$demo == d, na.rm = TRUE), integer(1))
    syscall_p95 <- vapply(
      demos,
      function(d) {
        x <- syscall_us$value_us[syscall_us$demo == d]
        if (!length(x)) return(NA_real_)
        as.numeric(quantile(x, 0.95, na.rm = TRUE))
      },
      numeric(1)
    )
    top_proxy <- data.frame(
      demo = demos,
      sample_count = sample_counts,
      syscall_p95_us = syscall_p95,
      stringsAsFactors = FALSE
    )
    top_proxy <- top_proxy[order(top_proxy$sample_count, decreasing = TRUE), ]
    top_proxy <- head(top_proxy, 8)
  }
}

summary_rows <- data.frame(
  metric = c(
    "demos_executed",
    "rtrace_event_count",
    "offcpu_event_count",
    "offcpu_p50_us",
    "offcpu_p95_us",
    "offcpu_max_us",
    "oncpu_sample_count",
    "oncpu_sample_rate_hz",
    "oncpu_sample_gap_p95_us",
    "syscall_count",
    "syscall_p95_us",
    "blocking_syscall_count"
  ),
  value = c(
    nrow(intervals),
    if (nrow(rtrace)) nrow(rtrace) else 0,
    offcpu_stats[["count"]],
    offcpu_stats[["p50_us"]],
    offcpu_stats[["p95_us"]],
    offcpu_stats[["max_us"]],
    proxy_stats[["oncpu_sample_count"]],
    proxy_stats[["oncpu_sample_rate_hz"]],
    proxy_stats[["oncpu_sample_gap_p95_us"]],
    proxy_stats[["syscall_count"]],
    proxy_stats[["syscall_p95_us"]],
    proxy_stats[["blocking_syscall_count"]]
  ),
  stringsAsFactors = FALSE
)
write.csv(summary_rows, summary_csv, row.names = FALSE)

pdf(report_pdf, width = 11, height = 8.5, paper = "special")

plot.new()
title("R Demo Trace Report")
summary_lines <- c(
  sprintf("Run directory: %s", run_dir),
  sprintf("Demos executed: %d", nrow(intervals)),
  sprintf("rtrace events: %d", if (nrow(rtrace)) nrow(rtrace) else 0),
  sprintf("offcpu events: %d", offcpu_stats[["count"]]),
  sprintf("offcpu p50: %s us", ifelse(is.na(offcpu_stats[["p50_us"]]), "NA", format(round(offcpu_stats[["p50_us"]], 2), nsmall = 2))),
  sprintf("offcpu p95: %s us", ifelse(is.na(offcpu_stats[["p95_us"]]), "NA", format(round(offcpu_stats[["p95_us"]], 2), nsmall = 2))),
  sprintf("oncpu samples: %d", proxy_stats[["oncpu_sample_count"]]),
  sprintf("oncpu sample rate: %s Hz", ifelse(is.na(proxy_stats[["oncpu_sample_rate_hz"]]), "NA", format(round(proxy_stats[["oncpu_sample_rate_hz"]], 2), nsmall = 2))),
  sprintf("oncpu gap p95: %s us", ifelse(is.na(proxy_stats[["oncpu_sample_gap_p95_us"]]), "NA", format(round(proxy_stats[["oncpu_sample_gap_p95_us"]], 2), nsmall = 2))),
  sprintf("syscall p95: %s us", ifelse(is.na(proxy_stats[["syscall_p95_us"]]), "NA", format(round(proxy_stats[["syscall_p95_us"]], 2), nsmall = 2))),
  "",
  "Interpretation:",
  if (nrow(rtrace) > 0) "- Probes fired with non-empty event stream." else "- No probe events captured.",
  if (offcpu_stats[["count"]] > 0) "- Scheduler off-CPU intervals indicate measurable contention/wait." else "- No off-CPU intervals captured.",
  if (proxy_stats[["oncpu_sample_count"]] > 0) "- On-CPU samples and syscall durations are being used as contention proxies." else "- No proxy scheduler data captured."
)
text(0.05, 0.92, labels = paste(summary_lines, collapse = "\n"), adj = c(0, 1), cex = 0.95)

if (nrow(intervals) > 0) {
  plot.new()
  title("Demo Timeline")
  y <- seq_len(nrow(intervals))
  xs <- as.numeric(intervals$start_ts)
  xe <- as.numeric(intervals$end_ts)
  xlim <- range(c(xs, xe), na.rm = TRUE)
  plot.window(xlim = xlim, ylim = c(0.5, nrow(intervals) + 0.5))
  axis(1)
  axis(2, at = y, labels = paste(intervals$pkg, intervals$demo, sep = "::"), las = 2, cex.axis = 0.7)
  for (i in seq_len(nrow(intervals))) {
    segments(xs[i], y[i], xe[i], y[i], lwd = 4, col = "steelblue")
  }
  box()
  mtext("Unix Time", side = 1, line = 2)
}

if (length(probe_counts) > 0) {
  barplot(probe_counts, las = 2, cex.names = 0.7, main = "Probe Event Counts", ylab = "Count", col = "darkseagreen")
} else {
  plot.new()
  title("Probe Event Counts")
  text(0.5, 0.5, "No probe events found", cex = 1.2)
}

if (nrow(offcpu) > 0) {
  plot(offcpu$value_us, pch = 16, cex = 0.5, col = "firebrick", main = "Off-CPU Durations", xlab = "Event Index", ylab = "offcpu_us")
  abline(h = offcpu_stats[["p95_us"]], col = "navy", lwd = 2, lty = 2)
  legend("topright", legend = sprintf("p95 = %.1f us", offcpu_stats[["p95_us"]]), bty = "n")
} else {
  if (nrow(oncpu) > 1) {
    gaps <- diff(sort(oncpu$wall_ns)) / 1000
    plot(gaps, pch = 16, cex = 0.45, col = "darkorange3", main = "On-CPU Sample Gap Proxy", xlab = "Sample Index", ylab = "gap_us")
    if (!is.na(proxy_stats[["oncpu_sample_gap_p95_us"]])) {
      abline(h = proxy_stats[["oncpu_sample_gap_p95_us"]], col = "navy", lwd = 2, lty = 2)
      legend("topright", legend = sprintf("p95 = %.1f us", proxy_stats[["oncpu_sample_gap_p95_us"]]), bty = "n")
    }
  } else {
    plot.new()
    title("On-CPU Sample Gap Proxy")
    text(0.5, 0.5, "No on-CPU sample-gap data found", cex = 1.2)
  }
}

plot.new()
title("Contention / Proxy Metrics Mapped To Demos")
if (nrow(top_contention) > 0) {
  lines_out <- c("Top demos by off-CPU p95:", "")
  for (i in seq_len(nrow(top_contention))) {
    lines_out <- c(lines_out, sprintf("%s | p95=%.2f us | n=%d", top_contention$demo[i], top_contention$p95_us[i], top_contention$count[i]))
  }
  text(0.05, 0.95, labels = paste(lines_out, collapse = "\n"), adj = c(0, 1), cex = 0.95)
} else {
  if (nrow(top_proxy) > 0) {
    lines_out <- c("Top demos by on-CPU proxy activity:", "")
    for (i in seq_len(nrow(top_proxy))) {
      p95_txt <- ifelse(is.na(top_proxy$syscall_p95_us[i]), "NA", format(round(top_proxy$syscall_p95_us[i], 2), nsmall = 2))
      lines_out <- c(lines_out, sprintf("%s | samples=%d | syscall p95=%s us", top_proxy$demo[i], top_proxy$sample_count[i], p95_txt))
    }
    text(0.05, 0.95, labels = paste(lines_out, collapse = "\n"), adj = c(0, 1), cex = 0.95)
  } else {
    text(0.5, 0.5, "No mapped contention or proxy windows available", cex = 1.1)
  }
}

if (nrow(combined_mat) > 0) {
  old_par <- par(no.readonly = TRUE)
  par(mar = c(4.5, 10, 3, 1.5))
  heat_cols <- colorRampPalette(c("#f7fbff", "#9ecae1", "#3182bd", "#08519c"))(128)
  plot_heatmap_panel(combined_mat, "Combined Activity Heatmap", demo_levels, run_span_sec, heat_cols)
  par(old_par)
}

dev.off()
