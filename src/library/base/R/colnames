colnames <- function(x) {
	dn <- dimnames(x)
	if(is.null(dn)) dn else dn[[2]]
}
"colnames<-" <- function(x, value) {
	dn <- dimnames(x)
	if(is.null(dn)) dimnames(x) <- list(dn, value)
	else dimnames(x) <- list(dn[[1]], value)
	x
}
