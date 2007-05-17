friedman.test <- function(y, ...) UseMethod("friedman.test")

friedman.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if (is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if (any(is.na(groups)) || any(is.na(blocks)))
            stop("NA's are not allowed in groups or blocks")
        if (any(diff(c(length(y), length(groups), length(blocks)))))
            stop("y, groups and blocks must have the same length")
        DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                       " and ", deparse(substitute(blocks)), sep = "")
        if (any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
        ## Need to ensure consistent order of observations within
        ## blocks.
        o <- order(groups, blocks)
        y <- y[o]
        groups <- groups[o]
        blocks <- blocks[o]
    }

    k <- nlevels(groups)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1, rank))
    TIES <- tapply(r, row(r), table)
    STATISTIC <- ((12 * sum((colSums(r) - n * (k + 1) / 2)^2)) /
                  (n * k * (k + 1)
                   - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                      (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Friedman chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Friedman rank sum test",
                   data.name = DNAME),
              class = "htest")
}

friedman.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula))
        stop("formula missing")
    ## <FIXME>
    ## Maybe put this into an internal rewriteTwoWayFormula() when
    ## adding support for strata()
    if((length(formula) != 3)
       || (length(formula[[3]]) != 3)
       || (formula[[3]][[1]] != as.name("|"))
       || (length(formula[[3]][[2]]) != 1)
       || (length(formula[[3]][[3]]) != 1))
        stop("incorrect specification for 'formula'")
    formula[[3]][[1]] <- as.name("+")
    ## </FIXME>
    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- NULL
    y <- do.call("friedman.test", as.list(mf))
    y$data.name <- DNAME
    y
}
