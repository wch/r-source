aperm <- function(a, perm, resize=TRUE) {
    if (missing(perm))
	perm<-(length(dim(a)):1)
    else {
	if(length(perm) != length(dim(a)))
	    stop("perm has incorrect length")
	if(!all(sort(perm)==1:length(perm)))
	    stop("perm is not a permutation")
    }
    r <- .Internal(aperm(a, perm, resize))
    if(!is.null(dn <- dimnames(a))) dimnames(r) <- dn[perm]
    r
}
