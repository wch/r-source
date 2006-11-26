## This remaps special names are they are used by deparsing, but why are they?
##
## It is not clear why it treats 'tsp' and 'levels' specially.
## It does ensure that factors which were deparsed as a double vector
## are converted to integers.
## As factors will normally have "class" after "levels", ordered factors
## do work correctly.
## behaviour will be changed in R 2.5.0
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
           !("ts" %in% c(attributes(.Data), attrib$class))) {
	    attrib$class <- c(attrib$class, "ts")
	    warning('adding class "ts": this is deprecated')
	}
	if(is.numeric(.Data) && "levels" %in% nm) {
	    if(! "factor" %in% c(attributes(.Data), attrib$class))
	    warning("conversion to factor is deprecated")
	    .Data <- factor(.Data, levels = seq_along(attrib$levels))
	}
	attributes(.Data) <- c(attributes(.Data), attrib)
    }
    return(.Data)
}
