.NotYetImplemented <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is not implemented yet", sep = ""))
}

.NotYetUsed <- function(arg) {
    warning(paste("argument `", arg, "' is not used (yet)", sep = ""))
}
