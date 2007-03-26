### Originates from Deepayan Sarkar as  updateList() from 'lattice' package
modifyList <- function(x, val)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    for (v in names(val)) {
	x[[v]] <-
	    if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
		modifyList(x[[v]], val[[v]]) else val[[v]]
    }
    x
}
