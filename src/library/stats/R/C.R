C <- function(object, contr, how.many, ...)
{
    if(!nlevels(object)) stop("object not interpretable as a factor")
    if(!missing(contr) && is.name(Xcontr <- substitute(contr)))
	contr <- switch(as.character(Xcontr),
			poly =	"contr.poly",
			helmert = "contr.helmert",
			sum = "contr.sum",
			treatment = "contr.treatment",
			SAS = "contr.SAS",
			contr
			)
    if(missing(contr)) {
	oc <- getOption("contrasts")
	contr <-
	    if(length(oc) < 2) # should not happen
		if(is.ordered(object)) contr.poly else contr.treatment
	    else oc[1 + is.ordered(object)]
    }
    if(missing(how.many) && !length(list(...)))
	contrasts(object) <- contr
    else {
	if(is.character(contr)) contr <- get(contr, mode = "function")
	if(is.function(contr)) contr <- contr(nlevels(object), ...)
	contrasts(object, how.many) <- contr
    }
    object
}
