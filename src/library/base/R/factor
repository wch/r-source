"factor" <-
function (x, levels = sort(unique(x), na.last = TRUE), labels, exclude = NA, 
	ordered = FALSE) 
{
	if (length(x) == 0) 
		return(character(0))
	exclude <- as.vector(exclude, typeof(x))
	levels <- levels[is.na(match(levels, exclude))]
	x <- .Internal(factor(match(x, levels), length(levels), 
		ordered))
	if (missing(labels)) 
		levels(x) <- levels
	else levels(x) <- labels
	x
}
as.factor <-
function(x, ordered=FALSE)
{
	test <- if(ordered) is.ordered else is.factor
        if(!test(x)) {
                levs <- sort(unique(x))
                x <- .Internal(factor(match(x, levs), length(levs), ordered))
                levels(x) <- levs
        }
        x
}
ordered <-
function(x, levels=sort(unique(x), na.last = TRUE), labels, exclude = NA,
	ordered=TRUE)
{
	if (length(x) == 0)
		return(character(0))
	exclude <- as.vector(exclude, typeof(x))
	levels <- levels[is.na(match(levels, exclude))]
	x <- .Internal(factor(match(x, levels), length(levels), ordered))
	if(missing(labels)) levels(x) <- levels
	else levels(x) <- labels
	x
}

as.ordered <- function(x) { 
  if (is.ordered(x)) x else ordered(x)
}
