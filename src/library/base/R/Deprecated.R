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
## Deprecated in 1.7.0
## from methods, should be deprecated.
printNoClass <-
    function(x, digits = NULL,quote = TRUE, na.print = NULL, print.gap = NULL,
             right = FALSE, ...) {
        .Deprecated("print.default")
        .Internal(print.default(x, digits, quote, na.print, print.gap,
                                right, FALSE))
}
## </entry>

