monthplot <- function(x, ...) UseMethod("monthplot")

monthplot.StructTS <-
    function (x, labels = NULL, ylab = choice, choice = "sea", ...)
    monthplot(fitted(x)[, choice], labels = labels, ylab = ylab, ...)

monthplot.stl <-
    function (x, labels = NULL, ylab = choice, choice = "seasonal", ...)
    monthplot(x$time.series[, choice], labels = labels, ylab = ylab, ...)

monthplot.ts <-
    function (x, labels = NULL, times = time(x), phase = cycle(x),
              ylab = deparse(substitute(x)), ...)
{
    if (is.null(labels) & !missing(phase))
        return(monthplot.default(x, times = times, phase = phase,
                                 ylab = ylab, ...))
    if (is.null(labels)) {
        if (missing(phase)) {
            f <- frequency(x)
            if (f == 4) labels <- paste("Q", 1:4, sep = "")
            else if (f == 12)
                labels <- c("J", "F", "M", "A", "M", "J", "J",
                  "A", "S", "O", "N", "D")
            else labels <- 1:f
        }
    }
    monthplot.default(x, labels = labels, times = times, phase = phase,
                      ylab = ylab, ...)
}

monthplot.default <-
    function (x, labels = 1:12,
              times = 1:length(x),
              phase = (times - 1)%%length(labels) + 1,
              base = mean,
              xlim = c(0.55, f + 0.45), ylim = range(x, na.rm = TRUE),
              axes = TRUE, xlab = "", ylab = deparse(substitute(x)),
              type = "l", box = TRUE, add = FALSE, ...)
{
    if (is.null(labels) || (missing(labels) && !missing(phase))) {
        labels <- unique(phase)
        phase <- match(phase, labels)
    }
    f <- length(labels)
    if (!is.null(base))
        means <- tapply(x, phase, base)
    if (!add) {
        plot(NA, NA, axes = FALSE, xlim = xlim, ylim = ylim,
             xlab = xlab, ylab = ylab, ...)
        if (box)
            box()
        if (axes) {
            axis(1, at = 1:f, labels = labels, ...)
            axis(2, ...)
        }
        if (!is.null(base))
            segments(1:f - 0.45, means, 1:f + 0.45, means)
    }
    y <- as.numeric(times)
    scale <- 1 / diff(range(y, na.rm = TRUE)) * 0.9
    for (i in 1:f) {
        sub <- phase == i
        if (type != "h")
            lines((y[sub] - min(y)) * scale - 0.45 + i, x[sub],
                  type = type, ...)
        else segments((y[sub] - min(y)) * scale - 0.45 + i, means[i],
                      (y[sub] - min(y)) * scale - 0.45 + i, x[sub], ...)
    }
}
