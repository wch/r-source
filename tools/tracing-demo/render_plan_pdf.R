args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript render_plan_pdf.R <input_md> <output_pdf>")
}

input_md <- args[[1]]
output_pdf <- args[[2]]

lines <- readLines(input_md, warn = FALSE)
if (!length(lines)) {
  lines <- "(empty plan file)"
}

page_size <- 55L
pages <- split(lines, ceiling(seq_along(lines) / page_size))

pdf(output_pdf, width = 11, height = 8.5, paper = "special")
for (i in seq_along(pages)) {
  plot.new()
  title(main = sprintf("Implementation Plan (%d/%d)", i, length(pages)))
  text(
    x = 0.02,
    y = 0.96,
    labels = paste(pages[[i]], collapse = "\n"),
    adj = c(0, 1),
    family = "mono",
    cex = 0.72
  )
}
dev.off()
