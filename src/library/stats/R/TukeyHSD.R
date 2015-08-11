#  File src/library/stats/R/TukeyHSD.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2000-2001  Douglas M. Bates
#  Copyright (C) 2002-2015  The R Core Team
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

###
###               Tukey multiple comparisons for R
###

TukeyHSD <-
    function(x, which, ordered = FALSE, conf.level = 0.95, ...)
    UseMethod("TukeyHSD")

TukeyHSD.aov <-
    function(x, which = seq_along(tabs), ordered = FALSE,
             conf.level = 0.95, ...)
{
    mm <- model.tables(x, "means")
    if(is.null(mm$n))
        stop("no factors in the fitted model")
    tabs <- mm$tables
    if(names(tabs)[1L] == "Grand mean") tabs <- tabs[-1L]
    tabs <- tabs[which]
    ## mm$n need not be complete -- factors only -- so index by names
    nn <- mm$n[names(tabs)]
    nn_na <- is.na(nn)
    if(all(nn_na))
        stop("'which' specified no factors")
    if(any(nn_na)) {
        warning("'which' specified some non-factors which will be dropped")
        tabs <- tabs[!nn_na]
        nn <- nn[!nn_na]
    }
    out <- setNames(vector("list", length(tabs)), names(tabs))
    MSE <- sum(x$residuals^2)/x$df.residual
    for (nm in names(tabs)) {
        tab <- tabs[[nm]]
        means <- as.vector(tab)
        nms <- if(length(dim(tab)) > 1L) {
            dn <- dimnames(tab)
            apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
        } else names(tab)
        n <- nn[[nm]]
        ## expand n to the correct length if necessary
        if (length(n) < length(means)) n <- rep.int(n, length(means))
        if (as.logical(ordered)) {
            ord <- order(means)
            means <- means[ord]
            n <- n[ord]
            if (!is.null(nms)) nms <- nms[ord]
        }
        center <- outer(means, means, "-")
        keep <- lower.tri(center)
        center <- center[keep]
        width <- qtukey(conf.level, length(means), x$df.residual) *
            sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
        est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
        pvals <- ptukey(abs(est), length(means), x$df.residual,
                        lower.tail = FALSE)
        dnames <- list(NULL, c("diff", "lwr", "upr","p adj"))
        if (!is.null(nms)) dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, center + width,pvals),
                           c(length(width), 4L), dnames)
    }
    class(out) <- c("TukeyHSD", "multicomp") # multicomp is historical
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    out
}

print.TukeyHSD <- function(x, digits = getOption("digits"), ...)
{
    cat("  Tukey multiple comparisons of means\n")
    cat("    ", format(100*attr(x, "conf.level"), 2),
        "% family-wise confidence level\n", sep = "")
    if (attr(x, "ordered"))
        cat("    factor levels have been ordered\n")
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n", sep = "")
    xx <- unclass(x)
    attr(xx, "orig.call") <- attr(xx, "conf.level") <- attr(xx, "ordered") <- NULL
    xx[] <- lapply(xx, function(z, digits)
               {z[, "p adj"] <- round(z[, "p adj"], digits); z},
                   digits = digits)
    print.default(xx, digits, ...)
    invisible(x)
}

plot.TukeyHSD <- function (x, ...)
{
    for (i in seq_along(x)) {
        xi <- x[[i]][, -4L, drop = FALSE] # drop p-values
        yvals <- nrow(xi):1L
        dev.hold(); on.exit(dev.flush())
        ## xlab, main are set below, so block them from ...
        plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L), type = "n",
             axes = FALSE, xlab = "", ylab = "", main = NULL, ...)
        axis(1, ...)
        axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]], srt = 0, ...)
        abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
        abline(v = 0, lty = 2, lwd = 0.5, ...)
        segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
        segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi),
                 rep.int(yvals + 0.1, 3L), ...)
        title(main = paste0(format(100 * attr(x, "conf.level"), digits = 2L),
                            "% family-wise confidence level\n"),
              xlab = paste("Differences in mean levels of", names(x)[i]))
        box()
	dev.flush(); on.exit()
    }
}
