prcomp <- function (x, ...) UseMethod("prcomp")

prcomp.default <-
    function(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...)
{
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scale.)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    s <- svd(x, nu = 0)
    s$d <- s$d / sqrt(max(1, nrow(x) - 1))
    if (!is.null(tol)) {
        ## we get rank at least one even for a 0 matrix.
        rank <- sum(s$d > (s$d[1]*tol))
        if (rank < ncol(x)) {
            s$v <- s$v[, 1:rank, drop = FALSE]
            s$d <- s$d[1:rank]
        }
    }
    dimnames(s$v) <-
        list(colnames(x), paste("PC", seq(len = ncol(s$v)), sep = ""))
    r <- list(sdev = s$d, rotation = s$v,
              center = if(is.null(cen)) FALSE else cen,
              scale = if(is.null(sc)) FALSE else sc)
    if (retx) r$x <- x %*% s$v
    class(r) <- "prcomp"
    r
}

prcomp.formula <- function (formula, data = NULL, subset, na.action, ...)
{
    mt <- terms(formula, data = data)
    if (attr(mt, "response") > 0)
        stop("response not allowed in formula")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$... <- NULL
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    ## this is not a `standard' model-fitting function,
    ## so no need to consider contrasts or levels
    if (any(sapply(mf, function(x) is.factor(x) || !is.numeric(x))))
        stop("PCA applies only to numerical variables")
    na.act <- attr(mf, "na.action")
    mt <- attr(mf, "terms")
    attr(mt, "intercept") <- 0
    x <- model.matrix(mt, mf)
    res <- prcomp.default(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1]] <- as.name("prcomp")
    res$call <- cl
    if (!is.null(na.act)) {
        res$na.action <- na.act
        if (!is.null(sc <- res$x))
            res$x <- napredict(na.act, sc)
    }
    res
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
function(x, digits = min(3, getOption("digits")-3), ...)
{
    cat("Importance of components:\n")
    print(x$importance, digits = digits)
    invisible(x)
}

predict.prcomp <- function(object, newdata, ...)
{
    if (missing(newdata)) {
        if(!is.null(object$x)) return(object$x)
        else stop("no scores are available: refit with 'retx=TRUE'")
    }
    if(length(dim(newdata)) != 2)
        stop("'newdata' must be a matrix or data frame")
    p <- NCOL(object$rotation)
    nm <- rownames(object$rotation)
    if(!is.null(nm)) {
        if(!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching one or more of the original columns")
        newdata <- newdata[, nm]
    } else {
        if(NCOL(newdata) != p)
            stop("'newdata' does not have the correct number of columns")
    }
    ## next line does as.matrix
    scale(newdata, object$center, object$scale) %*% object$rotation
}
