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

## Deprecated in 1.5.0
piechart <- function(x, labels = names(x), edges = 200, radius = 0.8,
              density = NULL, angle = 45, col = NULL,
              main = NULL, ...)
{
    .Deprecated("pie")
    mcall <- match.call()
    mcall[[1]] <- as.name("pie")
    eval(mcall, parent.frame())
}
## </Deprecated>
