interaction <-
function(..., drop=FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg == 1 && is.list(args[[1]])) {
	args <- args[[1]]
	narg <- length(args)
    }
    ans <- 0
    lvs <- NULL
    for(i in narg:1) {
        f <- args[[i]]
	if (!is.factor(f))
	    f <- factor(f)
	l <- levels(f)
	ans <- ans * length(l) + as.integer(f) - 1
	if (i == narg) lvs <- l
	else lvs <- as.vector(outer(l, lvs, paste, sep="."))
    }
    ans <- ans + 1
    if (drop) {
	f <- unique(ans[!is.na(ans)])
	ans <- match(ans, f)
	lvs <- lvs[f]
    }
    levels(ans) <- lvs
    class(ans) <- "factor"
    ans
}
