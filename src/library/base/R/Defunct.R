.Defunct <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is defunct.\n",
	       "See ?Defunct.",
	       sep = ""))
}

Version <- function() .Defunct()
provide <- function(package) .Defunct()
