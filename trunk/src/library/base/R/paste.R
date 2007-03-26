paste <- function (..., sep = " ", collapse = NULL)
{
    args <- list(...)
    if(length(args) == 0)
        if(length(collapse) == 0) character(0) else ""
    else {
	## using as.character() here {not in C} for method dispatch
	.Internal(paste(lapply(args, as.character), sep, collapse))
    }
}

##=== Could we consider a  .Primitive  *fast*
##  paste2 <- function(x,y)  paste(x,y, sep='')

##=== Could we extend  paste(.) to (optionally) accept a
##    2-vector for collapse ?	 With the following functionality

##- paste.extra <- function(r, collapse=c(", "," and ")) {
##-	    n <- length(r)
##-	    if(n <= 1) paste(r)
##-	    else
##-	      paste(paste(r[-n],collapse=collapse[1]),
##-		    r[n], sep=collapse[min(2,length(collapse))])
##- }
