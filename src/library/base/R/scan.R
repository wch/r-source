scan <-
    function(file="", what= double(0), nmax=-1, n=-1, sep="", quote="", 
             dec=".", skip=0, nlines=0, 
	     na.strings="NA", flush=FALSE, strip.white=FALSE, quiet=FALSE) {
	if(!missing(sep) && missing(na.strings))
	    na.strings <- c(na.strings,"")
	na.strings <- as.character(na.strings) # allow it to be NULL
	if(!missing(n)) {
	    if(missing(nmax))
		nmax <- n / pmax(length(what), 1)
	    else
		stop("Either specify `nmax' or `n', but not both.")
	}
	.Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
		       na.strings,flush,strip.white, quiet))
    }
