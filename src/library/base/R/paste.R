paste <- function (..., sep = " ", collapse=NULL)
{
        args <- list(...)
	if(is.null(args)) ""
	else {
		for (i in 1:length(args)) args[[i]] <- as.character(args[[i]])
		.Internal(paste(args, sep, collapse))
	}
}
##=== Could we extend  paste(.) to (optionally) accept a
##    2-vector for collapse ?    With the following functionality

##- paste.extra <- function(r, collapse=c(", "," and ")) {
##-         n <- length(r)
##-         if(n <= 1) paste(r)
##-         else
##-           paste(paste(r[-n],collapse=collapse[1]),
##-                 r[n], sep=collapse[min(2,length(collapse))])
##- }


