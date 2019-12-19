#  File src/library/stats/R/monthplot.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

monthplot <- function(x, ...) UseMethod("monthplot")

monthplot.StructTS <-
    function (x, labels = NULL, ylab = choice, choice = "sea", ...)
    monthplot(fitted(x)[, choice], labels = labels, ylab = ylab, ...)

monthplot.stl <-
    function (x, labels = NULL, ylab = choice, choice = "seasonal", ...)
    monthplot(x$time.series[, choice], labels = labels, ylab = ylab, ...)

monthplot.ts <-
    function (x, labels = NULL, times = time(x), phase = cycle(x),
              ylab = deparse1(substitute(x)), ...)
{
    if (is.null(labels) & !missing(phase))
        return(monthplot.default(x, times = times, phase = phase,
                                 ylab = ylab, ...))
    if (is.null(labels)) {
        if (missing(phase)) {
            f <- frequency(x)
            if (f == 4) labels <- paste0("Q", 1L:4L)
            else if (f == 12)
                labels <- c("J", "F", "M", "A", "M", "J", "J",
                  "A", "S", "O", "N", "D")
            else labels <- 1L:f
        }
    }
    monthplot.default(x, labels = labels, times = times, phase = phase,
                      ylab = ylab, ...)
}

monthplot.default <-
    function (x, labels = 1L:12L,
              ylab = deparse1(substitute(x)),
              times = seq_along(x),
              phase = (times - 1L)%%length(labels) + 1L, base = mean,
              axes = TRUE, type = c("l", "h"), box = TRUE, add = FALSE,
              col = par("col"), lty = par("lty"), lwd = par("lwd"),
              col.base = col, lty.base = lty, lwd.base = lwd, ...)
{
    dots <- list(...); nmdots <- names(dots)
    type <- match.arg(type)
    if (is.null(labels) || (missing(labels) && !missing(phase))) {
        labels <- unique(phase)
        phase <- match(phase, labels)
    }
    f <- length(labels)
    if (!is.null(base))
        means <- tapply(x, phase, base)
    if (!add) {
        dev.hold(); on.exit(dev.flush())
        Call <- match.call()
        Call[[1L]] <- quote(graphics::plot)
        Call$x <- NA
        Call$y <- NA
        Call$axes <- FALSE
        Call$xlim <- if("xlim" %in% nmdots) dots$xlim else c(0.55, f + 0.45)
        Call$ylim <- if("ylim" %in% nmdots) dots$ylim else range(x, na.rm = TRUE)
        Call$xlab <- if("xlab" %in% nmdots) dots$xlab else ""
        if(box) Call$frame.plot <- TRUE
        Call$labels <- Call$times <- Call$phase <- Call$base <-
            Call$type <- Call$box <- Call$add <- Call$col.base <-
            Call$lty.base <- Call$lwd.base <- NULL
        eval(Call)
        if (axes) {
            axis(1, at = 1L:f, labels = labels, ...)
            axis(2, ...)
        }
        if (!is.null(base)) {
            segments(1L:f - 0.45, means, 1L:f + 0.45, means,
                     col = col.base, lty = lty.base, lwd = lwd.base)
        }
    }
    y <- as.numeric(times)
    scale <- 1 / diff(range(y, na.rm = TRUE)) * 0.9
    for (i in 1L:f) {
        sub <- phase == i
        if (type != "h")
            lines((y[sub] - min(y)) * scale - 0.45 + i, x[sub],
                  type = type, col = col, lty = lty, lwd = lwd, ...)
        else segments((y[sub] - min(y)) * scale - 0.45 + i, means[i],
                      (y[sub] - min(y)) * scale - 0.45 + i, x[sub],
                      col = col, lty = lty, lwd = lwd, ...)
    }
    invisible()
}
