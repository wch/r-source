quade.test <- function(y, groups, blocks) 
{
    DNAME <- deparse(substitute(y))
    if(is.matrix(y)) {
        groups <- as.factor(c(col(y)))
        blocks <- as.factor(c(row(y)))
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
            stop("Not an unreplicated complete block design")
        groups <- as.factor(groups)
        blocks <- as.factor(blocks)
    }
    k <- nlevels(groups)
    b <- nlevels(blocks)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1, rank))
    q <- rank(apply(y, 1, function(u) max(u) - min(u)))
    s <- q * (r - (k+1)/2)
    ## S is a matrix of ranks within blocks (minus the average rank)
    ## multiplied by the ranked ranges of the blocks
    A <- sum(s^2)
    B <- sum(apply(s, 2, sum)^2) / b
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
        PVAL <- 1 - pf(STATISTIC, PARAMETER[1], PARAMETER[2])
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
