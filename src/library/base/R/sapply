sapply <- function(X, FUN, ..., simplify = TRUE)
{
	if(is.character(FUN))
		FUN <- get(FUN, mode = "function")
	else if(mode(FUN) != "function") {
		farg <- substitute(FUN)
		if(mode(farg) == "name")
			FUN <- get(farg, mode = "function")
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	answer <- lapply(as.list(X), FUN, ...)
	if(simplify && length(answer) &&
	   length(common.len <- unique(unlist(lapply(answer, length)))) == 1) {
		if(common.len == 1)
			unlist(answer, recursive = FALSE)
		else if(common.len > 1)
			array(unlist(answer, recursive = FALSE),
				dim= c(common.len, length(X)),
				dimnames= list(names(answer[[1]]), names(answer)))
		else answer
	} else answer
}

