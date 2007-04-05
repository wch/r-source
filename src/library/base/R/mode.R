mode <- function(x) {
    if(is.expression(x)) return("expression")
    if(is.call(x))
	return(switch(deparse(x[[1]])[1],
		      "(" = "(",
		      ## otherwise
		      "call"))
    if(is.name(x)) "name" else
    switch(tx <- typeof(x),
	   double=, integer= "numeric",# 'real=' dropped, 2000/Jan/14
	   closure=, builtin=, special= "function",
	   ## otherwise
	   tx)
}

"storage.mode<-" <-function(x, value)
{
    if (storage.mode(x) == value) return(x)
    if(is.factor(x)) stop("invalid to change the storage mode of a factor")
    if(value == "single")
        warning('use of storage.mode(x) <- "single" is deprecated: use mode<- instead', domain=NA, call.=FALSE)
    mde <- paste("as.",value,sep="")
    atr <- attributes(x)
    isSingle <- !is.null(attr(x, "Csingle"))
    setSingle <- value == "single"
    x <- eval(call(mde,x), parent.frame())
    attributes(x) <- atr
    ## this avoids one copy
    if(setSingle != isSingle)
        attr(x, "Csingle") <- if(setSingle) TRUE # else NULL
    x
}

"mode<-" <- function(x, value)
{
    if (storage.mode(x) == value) return(x)
    if(is.factor(x)) stop("invalid to change the storage mode of a factor")
    mde <- paste("as.",value,sep="")
    atr <- attributes(x)
    isSingle <- !is.null(attr(x, "Csingle"))
    setSingle <- value == "single"
    x <- eval(call(mde,x), parent.frame())
    attributes(x) <- atr
    ## this avoids one copy
    if(setSingle != isSingle)
        attr(x, "Csingle") <- if(setSingle) TRUE # else NULL
    x
}
storage.mode <- function(x) {
    x <- typeof(x)
    if (x == "closure" || x == "builtin" || x == "special") return("function")
    x
}
