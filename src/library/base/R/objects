inherits <- function(x, name)
	any(!is.na(match(name,class(x))))

NextMethod <- function(generic=NULL, object=NULL, ...)
	.Internal(NextMethod(generic, object,...))

methods <- function (generic.function, class) 
{
	allnames <- unique(c(ls(pos=seq(along=search()))))
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
	grep(gsub("\\.", "\\\\.", name), allnames, value = TRUE)
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

