.NotYetImplemented <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is not implemented yet", sep = ""))
}

.NotYetUsed <- function(x) {
    warning(paste("argument `", x, "' is not used (yet)", sep = ""))
}
