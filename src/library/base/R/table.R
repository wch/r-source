table <- function (..., exclude = c(NA, NaN),
   dnn = list.names(...), deparse.level = 1) 
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq(along = l)
        else nm == ""
        dep <- sapply(l[fixup], function(x) 
	    switch (deparse.level + 1,
		"", 
		if (is.symbol(x)) as.character(x) else "", 
		deparse(x)[1]
	    )
        )
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    
    args <- list(...)
    if (length(args) == 0)
	stop("nothing to tabulate")
    if (length(args) == 1 && is.list(args[[1]])) {
	args <- args[[1]]
	if (length(dnn) != length(args))
	    dnn <- if (!is.null(argn <- names(args))) 
	         argn 
	    else 
                 paste(dnn[1],1:length(args),sep='.') 
    }
    bin <- 0
    lens <- NULL
    dims <- integer(0)
    pd <- 1
    dn <- NULL
    for (a in args) {
	if (is.null(lens)) lens <- length(a)
	else if (length(a) != lens)
	    stop("all arguments must have the same length")
	if (is.factor(a))
	    cat <- a
	else
	    cat <- factor(a, exclude = exclude)
	nl <- length(l <- levels(cat))
	dims <- c(dims, nl)
	dn <- c(dn, list(l))
	## requiring   all(unique(as.integer(cat)) == 1:nlevels(cat))  :
	bin <- bin + pd * (as.integer(cat) - 1)
	pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    array(tabulate(bin + 1, pd), dims, dimnames = dn)
}
