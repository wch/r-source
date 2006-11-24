## it is not clear why this remaps special names: it has for a very long time
## nor why it treats 'tsp' and 'levels' specially.
## as factors will normally have "class" after "levels", ordered factors
## do work correctly.
structure <- function (.Data, ...)
{
    attrib <- list(...)
    if(length(attrib) > 0) {
        specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
        replace <- c("dim", "dimnames", "names", "tsp", "levels")
	m <- match(names(attrib), specials)
	ok <- (!is.na(m) & m > 0)
	names(attrib)[ok] <- replace[m[ok]]
        nm <- names(attrib)
	if("tsp" %in% nm &&
           !("ts" %in% c(attributes(.Data), attrib$class)))
	    attrib$class <- c(attrib$class, "ts")
	if(is.numeric(.Data) && "levels" %in% nm)
	    .Data <- factor(.Data, levels = seq_along(attrib$levels))
	attributes(.Data) <- c(attributes(.Data), attrib)
    }
    return(.Data)
}
