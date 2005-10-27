update.default <-
    function (object, formula., ..., evaluate = TRUE)
{
    call <- object$call
    if (is.null(call))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.))
	call$formula <- update.formula(formula(object), formula.)
    if(length(extras) > 0) {
	existing <- !is.na(match(names(extras), names(call)))
	## do these individually to allow NULL to remove entries.
	for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
	if(any(!existing)) {
	    call <- c(as.list(call), extras[!existing])
	    call <- as.call(call)
	}
    }
    if(evaluate) eval(call, parent.frame())
    else call
}

update.formula <- function (old, new, ...) {
    tmp <- .Internal(update.formula(as.formula(old), as.formula(new)))
    out <- formula(terms.formula(tmp, simplify = TRUE))
    return(out)
}
