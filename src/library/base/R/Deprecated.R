###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""), call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 1.6.0
print.ordered <- function (x, ...)
{
    .Deprecated("print.factor")
    invisible(NextMethod("print", x))
}
## </entry>
