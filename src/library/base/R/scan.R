scan <-
    function(file="", what= double(0), nmax=-1, n=-1, sep="", skip=0, nlines=0,
	     na.strings="NA", flush=FALSE, strip.white=FALSE, quiet=FALSE) {
	if(!missing(sep))
	    na.strings<-c(na.strings,"")
	if(!missing(n)) {
	    if(missing(nmax))
		nmax <- n / pmax(length(what), 1)
	    else
		stop("Either specify `nmax' or `n', but not both.")
	}
	.Internal(scan(file, what, nmax, sep, skip, nlines,
		       na.strings,flush,strip.white, quiet))
    }
