.First.lib <- function(lib, pkg)
{
    library.dynam("ts", pkg, lib)
    if(interactive() || .Options$ verbose)
	cat("\n	     This is a preliminary time series package for R 0.65\n",
	    "	   See `library(help=ts)' for details.\n\n")
}
#.conflicts.OK <- TRUE
options(ts.S.compat = FALSE)
