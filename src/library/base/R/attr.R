"mostattributes<-" <- function(obj, value) {
    if(length(value)) {
	if(!is.list(value)) stop("RHS must be list")
	if(h.nam <- !is.na(inam <- match("names", names(value)))) {
	    n1 <- value[[inam]];	value <- value[-inam] }
	if(h.dim <- !is.na(idin <- match("dim", names(value)))) {
	    d1 <- value[[idin]];	value <- value[-idin] }
	if(h.dmn <- !is.na(idmn <- match("dimnames", names(value)))) {
	    dn1 <- value[[idmn]];	value <- value[-idmn] }
	attributes(obj) <- value
        dm <- dim(obj)
	if(h.nam && is.null(dm) && length(obj) == length(n1))
	    names(obj) <- n1
	if(h.dim && length(obj) == prod(d1))
	    dim(obj) <- dm <- d1
	if(h.dmn && !is.null(dm)) {
            ddn <- sapply(dn1, length)
            if( all((dm == ddn)[ddn > 0]) ) dimnames(obj) <- dn1
        }
    }
    obj
}
