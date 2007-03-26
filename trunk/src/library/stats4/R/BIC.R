## we need this stats4::logLik to avoid stats namespace issues
setGeneric("AIC", useAsDefault=function(object, ..., k = 2)
           stats::AIC(stats4::logLik(object),  ..., k = k))

setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

setMethod("BIC", signature(object="logLik"),
function(object, ...){
    -2 * c(object) + attr(object, "df") * log(attr(object, "nobs"))
})

setMethod("BIC", signature(object="ANY"),
function(object, ...){
    BIC(object=logLik(object, ...))
})
