prcomp <- function(x, retx = TRUE, center = TRUE, scale. = FALSE,
                   tol = NULL) {
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scale.)
    s <- svd(x, nu = 0)
    if (!is.null(tol)) {
        rank <- sum(s$d > (s$d[1]*tol))
        if (rank < ncol(x))
            s$v <- s$v[, 1:rank, drop = FALSE]
    }
    s$d <- s$d / sqrt(max(1, nrow(x) - 1))
    dimnames(s$v) <-
        list(colnames(x), paste("PC", seq(len = ncol(s$v)), sep = ""))
    r <- list(sdev = s$d, rotation = s$v)
    if (retx) r$x <- x %*% s$v
    class(r) <- "prcomp"
    r
}

plot.prcomp <- function(x, main = deparse(substitute(x)), ...)
    screeplot(x, main = main, ...)

print.prcomp <- function(x, print.x = FALSE, ...) {
    cat("Standard deviations:\n")
    print(x$sdev, ...)
    cat("\nRotation:\n")
    print(x$rotation, ...)
    if (print.x && length(x$x)) {
        cat("\nRotated variables:\n")
        print(x$x, ...)
    }
    invisible(x)
}

summary.prcomp <- function(object, ...)
{
    vars <- object$sdev^2
    vars <- vars/sum(vars)
    importance <- rbind("Standard deviation" = object$sdev,
                        "Proportion of Variance" = round(vars, 5),
                        "Cumulative Proportion" = round(cumsum(vars), 5))
    colnames(importance) <- colnames(object$rotation)
    object$importance <- importance
    class(object) <- "summary.prcomp"
    object
}

print.summary.prcomp <-
function(x, digits = min(3, getOption("digits")-3), ...) {
    cat("Importance of components:\n")
    print(x$importance, digits = digits)
    invisible(x)
}
