print <- function(x, ...)UseMethod("print")

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <-
function(x,digits=NULL,quote=TRUE,na.print=NULL,print.gap=NULL, ...)
	.Internal(print.default(x,digits,quote,na.print,print.gap))

print.atomic <- function(x,quote=TRUE,...) print.default(x,quote=quote)

print.matrix <- function (x, rowlab = character(0), collab =
                          character(0), quote = TRUE, right = FALSE) {
  x <- as.matrix(x)
  .Internal(print.matrix(x, rowlab, collab, quote, right))
}
prmatrix <- .Alias(print.matrix)

print.tabular <-
function(table, digits = max(3, .Options$digits - 3), na.print = "")
{
	if(!is.null(table$title)) cat("\n", table$title, "\n\n", sep="")
	if(!is.null(table$topnote))
		cat(paste(table$topnote, collapse="\n"), "\n\n", sep="")
	print.default(table$table, digits=digits, na = "", print.gap = 2)
	if(!is.null(table$botnote))
		cat("\n", paste(table$botnote, collapse="\n"), sep="")
	cat("\n")
}

noquote <- function(obj) {
	## constructor for a useful "minor" class
	if(!inherits(obj,"noquote")) class(obj) <- c(class(obj),"noquote")
	obj
}

"[.noquote" <- function (x, ...) {
	attr <- attributes(x,"legend")
	r <- unclass(x)[...]
	attributes(r) <- c(attributes(r),
			   attr[is.na(match(names(attr),c("dim","dimnames")))])
	r
}

print.noquote <- function(obj,...) {
	## method for (character) objects of class 'noquote'
	cl <- class(obj)
	if(!is.null(cl)) class(obj) <- cl[cl != "noquote"]
	NextMethod("print", obj, quote = FALSE, ...)
}
## used for version:
print.simple.list <-
function(x, ...) print(noquote(cbind("_"=unlist(x))), ...)
