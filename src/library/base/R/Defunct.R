.Defunct <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is defunct.\n",
	       "See ?Defunct.",
	       sep = ""))
}

provide <- function(package) .Defunct()
