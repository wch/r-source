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

## Deprecated in 1.4.0
## </Deprecated>
