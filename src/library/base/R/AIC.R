#### Return the object's value of the Akaike Information Criterion
#### (or "An Inf.. Crit..")

AIC <- function(object, ..., k = 2) UseMethod("AIC")

## AIC for logLik objects
AIC.logLik <- function(object, ..., k = 2)
    -2 * c(object) + k * attr(object, "df")

AIC.default <- function(object, ..., k = 2)
{
    ## AIC for various fitted objects --- any for which there's a logLik() method:

    if(length(list(...))) {# several objects: produce data.frame
	object <- list(object, ...)
	val <- lapply(object, logLik)
	val <- as.data.frame(t(sapply(val,
				      function(el)
				      c(attr(el, "df"), AIC(el, k = k)))))
	names(val) <- c("df", "AIC")
        Call <- match.call()
        Call$k <- NULL
	row.names(val) <- as.character(Call[-1])
	val
    } else AIC(logLik(object), k = k)
}
