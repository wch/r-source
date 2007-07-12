apropos <- function (what, where = FALSE, ignore.case = TRUE, mode = "any")
{
    stopifnot(is.character(what))
    x <- character(0)
    check.mode <- mode != "any"
    for (i in seq_along(search())) {
	li <-
	    if(ignore.case)
		grep(what, ls(pos = i, all.names = TRUE),
		     ignore.case = TRUE, value = TRUE)
	    else ls(pos = i, pattern = what, all.names = TRUE)
	if(length(li)) {
	    if(check.mode)
		li <- li[sapply(li, exists, where = i,
				mode = mode, inherits = FALSE)]
	    x <- c(x, if(where) structure(li, names = rep.int(i, length(li))) else li)
	}
    }
    sort(x)
}

find <- function(what, mode = "any", numeric = FALSE, simple.words=TRUE)
{
    stopifnot(is.character(what))
    if(length(what) > 1) {
        warning("elements of 'what' after the first will be ignored")
        what <- what[1]
    }
#   would need to escape at least + * | as well
#     if(simple.words)
# 	what <- gsub("([.[])", "\\\\\\1", paste("^",what,"$", sep=""))
    len.s <- length(sp <- search())
    ind <- logical(len.s)
    check.mode <- mode != "any"
    for (i in 1:len.s) {
        if(simple.words) {
            found <- what %in% ls(pos = i, all.names = TRUE)
            if(found && check.mode)
                found <- exists(what, where = i, mode = mode, inherits=FALSE)
            ind[i] <- found
        } else {
            li <- ls(pos = i, pattern = what, all.names = TRUE)
            ll <- length(li)
            if(ll > 0 && check.mode) {
                mode.ok <- sapply(li, exists, where = i, mode = mode,
                                  inherits = FALSE)
                ll <- sum(mode.ok)
                if(ll >= 2)
                    warning(gettextf("%d occurrences in %s", ll, sp[i]),
                            domain = NA)
            }
            ind[i] <- ll > 0
        }
    }
    ## found name in  search()[ ind ]
    if(numeric) structure(which(ind), names=sp[ind]) else sp[ind]
}

