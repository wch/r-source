assocplot <- function(x, col = c("black", "red"), space = 0.3,
                      main = NULL, xlab = NULL, ylab = NULL)
{
    if(length(dim(x)) != 2)
        stop("'x' must be a 2-d contingency table")
    if(any(x < 0) || any(is.na(x)))
        stop("all entries of 'x' must be nonnegative and finite")
    if((n <- sum(x)) == 0)
        stop("at least one entry of 'x' must be positive")
    if(length(col) != 2)
        stop("incorrect 'col': must be length 2")

    f <- x[ , rev(1:NCOL(x))]           # rename for convenience;
                                        # f is observed freqs
                                        # reverse to be consistent with
                                        # mosaicplot().
    e <- outer(rowSums(f), colSums(f), "*") / n
                                        # e is expected freqs
    d <- (f - e) / sqrt(e)              # Pearson residuals
    e <- sqrt(e)
    x.w <- apply(e, 1, max)             # the widths of the x columns
    y.h <- apply(d, 2, max) - apply(d, 2, min)
                                        # the heights of the y rows
    x.delta <- mean(x.w) * space
    y.delta <- mean(y.h) * space
    xlim <- c(0, sum(x.w) + NROW(f) * x.delta)
    ylim <- c(0, sum(y.h) + NCOL(f) * y.delta)
    plot.new()
    plot.window(xlim, ylim, log = "")
    x.r <- cumsum(x.w + x.delta)
    x.m <- (c(0, x.r[-NROW(f)]) + x.r) / 2
    y.u <- cumsum(y.h + y.delta)
    y.m <- y.u - apply(pmax(d, 0), 2, max) - y.delta / 2
    z <- expand.grid(x.m, y.m)
    rect(z[, 1] - e / 2, z[, 2],
         z[, 1] + e / 2, z[, 2] + d,
         col = col[1 + (d < 0)])
    axis(1, at = x.m, labels = rownames(f), tick = FALSE)
    axis(2, at = y.m, labels = colnames(f), tick = FALSE)
    abline(h = y.m, lty = 2)
    ndn <- names(dimnames(f))
    if(length(ndn) == 2) {
        if(is.null(xlab))
            xlab <- ndn[1]
        if(is.null(ylab))
            ylab <- ndn[2]
    }
    title(main = main, xlab = xlab, ylab = ylab)
}
