warnings <- function(...)
{
    if(!exists("last.warning", envir=.GlobalEnv)) return()
    last.warning <- get("last.warning", envir=.GlobalEnv)
    if(!(n <- length(last.warning))) return()
    names <- names(last.warning)
    cat("Warning message", if(n > 1)"s", ":\n", sep="")
    for(i in 1:n) {
	out <- if(n == 1) names[i] else paste(i,": ", names[i], sep="")
	if(length(last.warning[[i]])) {
	    temp <- deparse(last.warning[[i]])
	    out <- paste(out, "in:", temp[1], if(length(temp) > 1) " ...")
	}
	cat(out, ..., fill = TRUE)
    }
}
