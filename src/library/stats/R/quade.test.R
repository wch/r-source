quade.test <- function(y, ...) UseMethod("quade.test")

quade.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if(is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if(any(is.na(groups)) || any(is.na(blocks)))
            stop("NA's are not allowed in groups or blocks")
        if(any(diff(c(length(y), length(groups), length(blocks)))))
            stop("y, groups and blocks must have the same length")
        DNAME <- paste(DNAME, ", ",
                       deparse(substitute(groups)), " and ",
                       deparse(substitute(blocks)), sep = "")
        if(any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
    }
    k <- nlevels(groups)
    b <- nlevels(blocks)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
#    n <- nrow(y)
    r <- t(apply(y, 1, rank))
    q <- rank(apply(y, 1, function(u) max(u) - min(u)))
    s <- q * (r - (k+1)/2)
    ## S is a matrix of ranks within blocks (minus the average rank)
    ## multiplied by the ranked ranges of the blocks
    A <- sum(s^2)
    B <- sum(colSums(s)^2) / b
    if(A == B) {
        ## Treat zero denominator case as suggested by Conover (1999),
        ## p.374.
        STATISTIC <- NaN
        PARAMETER <- c(NA, NA)
        PVAL <- (gamma(k+1))^(1-b)
    } else {
        STATISTIC <- (b - 1) * B / (A - B)
        ## The same as 2-way ANOVA on the scores S.
        PARAMETER <- c(k - 1, (b-1) * (k-1))
        PVAL <- pf(STATISTIC, PARAMETER[1], PARAMETER[2], lower = FALSE)
    }
    names(STATISTIC) <- "Quade F"
    names(PARAMETER) <- c("num df", "denom df")

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Quade test",
                   data.name = DNAME),
              class = "htest")
}

quade.test.formula <-
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
    y <- do.call("quade.test", as.list(mf))
    y$data.name <- DNAME
    y
}
