setGeneric("AIC")

setGeneric("BIC", function(object, ...) standardGeneric("BIC"))

setMethod("BIC", signature(object="logLik"),
function(object, ...){
    -2 * c(object) + attr(object, "df") * log(attr(object, "nobs"))
})

setMethod("BIC", signature(object="ANY"),
function(object, ...){
    BIC(object=logLik(object, ...))
})          
