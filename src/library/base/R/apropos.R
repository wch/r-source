apropos <- function (what, where = FALSE, mode = "any")
{
    if(!is.character(what))
	what <- as.character(substitute(what))
    x <- character(0)
    check.mode <- mode != "any"
    for (i in seq(search())) {
	ll <- length(li <- ls(pos = i, pattern = what, all.names = TRUE))
	if (ll) {
	    if(check.mode)
		ll <- length(li <- li[sapply(li, function(x)
					     exists(x, where = i,
						    mode = mode, inherits=FALSE))])
	    x <- c(x, if (where) structure(li, names = rep.int(i, ll)) else li)
	}
    }
    x
}

find <- function(what, mode = "any", numeric. = FALSE, simple.words=TRUE) {
    if(!is.character(what))
	what <- as.character(substitute(what))
    if(simple.words)
	what <- gsub("([.[])", "\\\\\\1", paste("^",what,"$", sep=""))
    len.s <- length(sp <- search())
    ind <- logical(len.s)
    if((check.mode <- mode != "any"))
	nam <- character(len.s)
    for (i in 1:len.s) {
	ll <- length(li <- ls(pos = i, pattern = what, all.names = TRUE))
	ind[i] <- ll > 0
	if(ll >= 2) warning(paste(ll, "occurrences in", sp[i]))
	if(check.mode && ind[i]) nam[i] <- li[1]
    }
    ## found name in  search()[ ind ]

    ii <- which(ind)
    if(check.mode && any(ind)) {
	mode.ok <- sapply(ii, function(i) exists(nam[i], where = i,
						 mode = mode, inherits=FALSE))
	ii <- ii[mode.ok]
    }
    if(numeric.) structure(ii, names=sp[ii]) else sp[ii]
}

