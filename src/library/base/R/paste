paste <- function (..., sep = " ", collapse=NULL) 
{
        args <- list(...)
	if(is.null(args)) ""
	else {
		for (i in 1:length(args)) args[[i]] <- as.character(args[[i]])
		.Internal(paste(args, sep, collapse))
	}
}
