### This is almost like the Primitive ":" for factors
### (that has no "drop = TRUE") --- it's not used anywhere in "standard R"
interaction <- function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg == 1 && is.list(args[[1]])) {
	args <- args[[1]]
	narg <- length(args)
    }
    for(i in narg:1) {
        f <- args[[i]]
	if (!is.factor(f)) f <- factor(f)
	l <- levels(f)
        if1 <- as.integer(f) - 1
        if(i != narg) {
            if(lex.order) {
                ll <- length(lvs)
                ans <- ans + ll * if1
                lvs <- ## as.vector(t(outer(l, lvs, paste, sep=sep)))
                    paste(rep(l, each= ll), rep(lvs, length(l)), sep=sep)
            } else {
                ans <- ans * length(l) + if1
                lvs <- ## as.vector(outer(l, lvs, paste, sep=sep))
                    paste(rep(l, length(lvs)), rep(lvs, each = length(l)), sep=sep)
            }
        } else {
            ans <- if1
            lvs <- l
        }
    }
    ans <- structure(as.integer(ans+1), levels=lvs, class = "factor")
    ans[ , drop=drop]
}
