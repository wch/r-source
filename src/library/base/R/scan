scan <- function(file="", what=0, nmax=-1, sep="", skip=0, nlines=0,
	na.strings="NA", flush=FALSE, strip.white=FALSE, quiet=FALSE) {
	if( !missing(sep) )
		na.strings<-c(na.strings,"")
	.Internal(scan(file, what, nmax, sep, skip, nlines, na.strings,flush,strip.white, quiet))
}
