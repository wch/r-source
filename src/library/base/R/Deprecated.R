###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""))
}

## Deprecated in 1.2.0
getenv <- function(x) {
    .Deprecated("Sys.getenv")
    Sys.getenv(x)
}
## </Deprecated>

## Deprecated in 1.2.3
dotplot <- function(...) {
    .Deprecated("dotchart")
    dotchart(...)
}
stripplot <- function(...) {
    .Deprecated("stripchart")
    stripchart(...)
}
## </Deprecated>
