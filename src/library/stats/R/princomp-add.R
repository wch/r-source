## copyright (C) 1998 W. N. Venables and B. D. Ripley
##
predict.princomp <- function(object, newdata, ...) {
    if (missing(newdata)) return(object$scores)
    scale(newdata, object$center, object$scale) %*% object$loadings
}

summary.princomp <- function(object, loadings = FALSE, cutoff = 0.1, ...)
{
    object$cutoff <- cutoff
    object$print.loadings <- loadings
    class(object) <- "summary.princomp"
    object
}

print.summary.princomp <-
    function(x, digits = 3, loadings = x$print.loadings, cutoff = x$cutoff,
             ...)
{
    vars <- x$sdev^2
    vars <- vars/sum(vars)
    cat("Importance of components:\n")
    print(rbind("Standard deviation" = x$sdev,
                "Proportion of Variance" = vars,
                "Cumulative Proportion" = cumsum(vars)))
    if(loadings) {
        cat("\nLoadings:\n")
        cx <- format(round(x$loadings, digits = digits))
        cx[abs(x$loadings) < cutoff] <-
            paste(rep(" ", nchar(cx[1,1], type="w")), collapse="")
        print(cx, quote = FALSE, ...)
    }
    invisible(x)
}

plot.princomp <- function(x, main = deparse(substitute(x)), ...)
    screeplot(x, main = main, ...)

screeplot <-
function(x, npcs = min(10, length(x$sdev)),
         type = c("barplot", "lines"),
         main = deparse(substitute(x)), ...)
{
    main
    type <- match.arg(type)
    pcs <- x$sdev^2
    xp <- seq(length=npcs)
    if(type=="barplot")
        barplot(pcs[xp], names = names(pcs[xp]), main = main,
                ylab = "Variances", ...)
    else {
        plot(xp, pcs[xp], type = "b", axes = FALSE, main = main,
             xlab = "", ylab = "Variances", ...)
        axis(2)
        axis(1, at = xp, labels = names(pcs[xp]))
    }
    invisible()
}

loadings <- function(x) x$loadings
