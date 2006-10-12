conflicts <- function(where=search(), detail = FALSE)
{
    if(length(where) < 1) stop("argument 'where' of length 0")
    z <- vector(length(where), mode="list")
    names(z) <- where
    for(i in seq_along(where))
	z[[i]] <- objects(pos=i)
    all <- unlist(z, use.names=FALSE)
    dups <- duplicated(all)
    dups <- all[dups]
    if(detail) {
	for(i in where)
	    z[[i]] <- z[[i]][match(dups, z[[i]], 0)]
	z[sapply(z, function(x) length(x)==0)] <- NULL
	z
    } else dups
}
