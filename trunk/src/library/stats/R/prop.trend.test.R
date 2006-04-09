prop.trend.test <- function (x, n, score = 1:length(x)) 
{
    method <- "Chi-squared Test for Trend in Proportions"
    dname <- paste(deparse(substitute(x)), "out of", deparse(substitute(n)))
    dname <- paste(dname, ",\n using scores:", paste(score, collapse = " "))

    ## Tabular input have caused grief, get rid of
    ## dim() attributes:
    x <- as.vector(x)
    n <- as.vector(n) 
    score <- as.vector(score)

    freq <- x/n
    p <- sum(x)/sum(n)
    w <- n/p/(1 - p)
    a <- anova(lm(freq ~ score, weight = w))
    chisq <- a["score", "Sum Sq"]
    names(chisq) <- "X-squared"
    df <- c(df = 1)
    pval <- pchisq(chisq, 1, lower.tail = FALSE)
    rval <- list(statistic = chisq, parameter = df,
                 p.value = as.numeric(pval),  
                 method = method, data.name = dname)
    class(rval) <- "htest"
    return(rval)
}
