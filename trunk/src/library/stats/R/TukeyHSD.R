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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA


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
    tabs <- mm$tables[-1]
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
        est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
        pvals <- ptukey(abs(est),length(means),x$df.residual,lower.tail=FALSE)
        dnames <- list(NULL, c("diff", "lwr", "upr","p adj"))
        if (!is.null(nms)) dnames[[1]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, center + width,pvals),
                           c(length(width), 4), dnames)
    }
    class(out) <- c("multicomp", "TukeyHSD")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    out
}

print.TukeyHSD <- function(x, digits=getOption("digits"), ...)
{
    cat("  Tukey multiple comparisons of means\n")
    cat("    ", format(100*attr(x, "conf.level"), 2),
        "% family-wise confidence level\n", sep="")
    if (attr(x, "ordered"))
        cat("    factor levels have been ordered\n")
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500), "\n\n", sep="")
    xx <- unclass(x)
    attr(xx, "orig.call") <- attr(xx, "conf.level") <- attr(xx, "ordered") <- NULL
    xx[] <- lapply(xx, function(z, digits)
               {z[, "p adj"] <- round(z[, "p adj"], digits); z},
                   digits=digits)
    print.default(xx, digits, ...)
    x
}

plot.TukeyHSD <- function (x, ...)
{
    for (i in seq_along(x)) {
        xi <- x[[i]][, -4, drop=FALSE] # drop p-values
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
