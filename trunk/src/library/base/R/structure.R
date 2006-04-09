"structure" <-
    function (.Data, ...)
{
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
    replace <- c("dim", "dimnames", "names", "tsp", "levels")
    attrib <- list(...)
    if(length(attrib) > 0) {
	m <- match(names(attrib), specials)
	ok <- (!is.na(m) & m > 0)
	names(attrib)[ok] <- replace[m[ok]]
	if(any(names(attrib) == "tsp"))
	    attrib$class <- unique(c("ts", attrib$class))
	if(is.numeric(.Data) && any(names(attrib) == "levels"))
	    .Data <- factor(.Data,levels=seq(along=attrib$levels))
	attributes(.Data) <- c(attributes(.Data), attrib)
    }
    return(.Data)
}
