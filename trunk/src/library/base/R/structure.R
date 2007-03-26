## This remaps special names as they are used by deparsing, but why are they?
##
structure <- function (.Data, ...)
{
    attrib <- list(...)
    if(length(attrib) > 0) {
        specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
        replace <- c("dim", "dimnames", "names", "tsp", "levels")
	m <- match(names(attrib), specials)
	ok <- (!is.na(m) & m > 0)
	names(attrib)[ok] <- replace[m[ok]]
        ## prior to 2.5.0 factors would deparse to double codes
	if("factor" %in% attrib$class && typeof(.Data) == "double")
	   storage.mode(.Data) <- "integer"
	attributes(.Data) <- c(attributes(.Data), attrib)
    }
    return(.Data)
}
