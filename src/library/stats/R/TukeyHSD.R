###
###               Tukey multiple comparisons for R
###
### Copyright 2000-2001  Douglas M. Bates <bates@stat.wisc.edu>
### Modified for base R 2002 B. D. Ripley
###
### This file is made available under the terms of the GNU General
### Public License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

TukeyHSD <-
    function(x, which, ordered = FALSE, conf.level = 0.95, ...)
    UseMethod("TukeyHSD")

TukeyHSD.aov <-
    function(x, which = seq(along = tabs), ordered = FALSE,
             conf.level = 0.95, ...)
{
    mm <- model.tables(x, "means")
    tabs <- mm$tables[-1]
    tabs <- tabs[which]
    nn <- mm$n[which]
    out <- vector("list", length(tabs))
    names(out) <- names(tabs)
    MSE <- sum(resid(x)^2)/x$df.residual
    for (nm in names(tabs)) {
        tab <- tabs[[nm]]
        means <- as.vector(tab)
        nms <- if(length(d <- dim(tab)) > 1) {
            dn <- dimnames(tab)
            apply(do.call("expand.grid", dn), 1, paste, collapse=":")
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
        dnames <- list(NULL, c("diff", "lwr", "upr"))
        if (!is.null(nms)) dnames[[1]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, center + width),
                           c(length(width), 3), dnames)
    }
    class(out) <- c("multicomp", "TukeyHSD")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    out
}

print.TukeyHSD <- function(x, ...)
{
    cat("  Tukey multiple comparisons of means\n")
    cat("    ", format(100*attr(x, "conf.level"), 2),
        "% family-wise confidence level\n", sep="")
    if (attr(x, "ordered"))
        cat("    factor levels have been ordered\n")
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500), "\n\n", sep="")
    attr(x, "orig.call") <- attr(x, "conf.level") <- attr(x, "ordered") <- NULL
    print.default(unclass(x), ...)
}

plot.TukeyHSD <- function (x, ...)
{
    for (i in seq(along = x)) {
        xi <- x[[i]]
        yvals <- nrow(xi):1
        plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), type = "n",
             axes = FALSE, xlab = "", ylab = "", ...)
        axis(1, ...)
        axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1]],
             srt = 0, ...)
        abline(h = yvals, lty = 1, lwd = 0, col = "lightgray")
        abline(v = 0, lty = 2, lwd = 0, ...)
        segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
        segments(as.vector(xi), rep.int(yvals - 0.1, 3), as.vector(xi),
                 rep.int(yvals + 0.1, 3), ...)
        title(main = paste(format(100 * attr(x, "conf.level"),
              2), "% family-wise confidence level\n", sep = ""),
              xlab = paste("Differences in mean levels of", names(x)[i]))
        box()
    }
}
