## 'objects <- function(....) ...    --->>> ./attach.R

inherits <- function(x, what, which = FALSE)
	.Internal(inherits(x, what, which))

NextMethod <- function(generic=NULL, object=NULL, ...)
    .Internal(NextMethod(generic, object,...))

methods <- function (generic.function, class)
{
    an <- lapply(seq(along=(sp <- search())), ls)
    names(an) <- sp
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
	name <- paste("^", generic.function, ".", sep = "")
    }
    else if (!missing(class)) {
	if (!is.character(class))
	    class <- paste(deparse(substitute(class)))
	name <- paste(".", class, "$", sep = "")
    }
    else stop("must supply generic.function or class")
    grep(gsub("([.[])", "\\\\\\1", name), unlist(an), value = TRUE)
}

data.class <- function(x) {
    if (length(cl <- class(x)))
	cl[1]
    else {
	l <- length(dim(x))
	if (l == 2)	"matrix"
	else if (l > 0)	"array"
	else mode(x)
    }
}
