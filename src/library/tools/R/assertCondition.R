assertCondition <- function(expr, ..., verbose=getOption("verbose")) {
    fe <- function(e)e
    d.expr <- deparse(substitute(expr), width.cutoff = 30L)
    if(length(d.expr) > 1)
	d.expr <- paste(d.expr[[1]], "...")
    conds <- if(nargs() > 1) c(...) else "condition"
    .Wanted <- paste(conds, collapse = " | ")
    res <- tryCatch(expr, error = fe, warning = fe, finally = fe)
    if(inherits(res, "condition")) {
	if(any(ii <- class(res) %in% conds)) {
	    if(verbose) cat(sprintf("Asserted %s: %s\n",
				    class(res)[which.max(ii)],
				    conditionMessage(res)))
	    invisible(res)
	} else
	    stop(gettextf("Expected %s, got %s in evaluating %s",
			 .Wanted, class(res)[[1]], d.expr))
    }
    else
	stop(gettextf("Failed to get expected %s in evaluating %s",
		     .Wanted, d.expr))
}
