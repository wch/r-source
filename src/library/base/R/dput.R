dput <-
    function(x, file = "",
             control = c("keepNA", "keepInteger", "showAttributes"))
{
    if(is.character(file))
        if(nchar(file) > 0) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    opts <- .deparseOpts(control)
    if(isS4(x)) {
        ## FIXME: this should happen in C {deparse2() in main/deparse.c}
        ##        but we are missing a C-level slotNames()
	cat('new("', class(x),'"\n', file = file, sep = '')
	for(n in slotNames(x)) {
	    cat("    ,", n, "= ", file = file)
	    dput(slot(x, n), file = file, control = control)
	}
	cat(")\n", file = file)
	invisible()
    }
    else .Internal(dput(x, file, opts))
}

dget <- function(file)
    eval(parse(file = file))
