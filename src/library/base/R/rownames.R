rownames <- function(x) {
	dn <- dimnames(x)
	if(is.null(dn)) dn else dn[[1]]
}
"rownames<-" <- function(x, value) {
	dn <- dimnames(x)
	if(is.null(dn)) dimnames(x) <- list(value, dn)
	else dimnames(x) <- list(value, dn[[2]])
	x
}
