.Defunct <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is defunct.\n",
	       "See ?Defunct.",
	       sep = ""))
}

Version <- function() .Defunct()
provide <- function(package) .Defunct()

## <entry>
## Deprecated in 1.2.0
## Defunct in 1.3.0
getenv <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.2.3
## Defunct in 1.3.0
dotplot <- function(...) .Defunct()
stripplot <- function(...) .Defunct()
## </entry>
