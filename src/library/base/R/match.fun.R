match.fun <- function (FUN, descend = FALSE)
{
    if (is.function(FUN)) FUN
    else if (is.character(FUN))
	get(FUN, mode = "function")
    else {
	FUN <- deparse(substitute(FUN))
	if (descend)
	    get(FUN, mode = "function")
	else stop(paste("\"", FUN, "\" is not a function", sep = ""))
    }
}
