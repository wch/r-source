#### Return the object's value of the Akaike Information Criterion
#### (or "A Inf.. Crit..")

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## AIC for logLik objects
AIC.logLik <- function(object, ..., k = 2)
    -2 * c(object) + k * attr(object, "df")

## AIC for various fitted objects
AIC.lm <- function(object, ..., k = 2)
{
    if(length(list(...))) {
        object <- list(object, ...)
        val <- lapply(object, logLik)
        val <- as.data.frame(t(sapply(val,
                                      function(el)
                                      c(attr(el, "df"), AIC(el, k = k)))))
        names(val) <- c("df", "AIC")
        row.names(val) <- as.character(match.call()[-1])
        val
    } else {
        AIC(logLik(object), k = k)
    }
}
