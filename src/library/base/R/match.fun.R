match.fun <- function (FUN, descend = TRUE)
{
    if (is.function(FUN)) FUN
    else if (is.character(FUN))
	get(FUN, mode = "function")
    else {
	f <- substitute(FUN)
	if(descend && is.name(f))
	    get(as.character(f), mode = "function")
	else stop(paste("\"", f, "\" is not a function", sep = ""))
    }
}
