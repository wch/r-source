.NotYetImplemented <- function() {
    stop(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
	       "is not implemented yet"))
}

.NotYetUsed <- function(arg, error = TRUE) {
    msg <- paste("argument", sQuote(arg), "is not used (yet)")
    if(error) stop(msg) else warning(msg)
}
