.First.lib <- function(lib, pkg)
{
    library.dynam("ts", pkg, lib)
    if(interactive() || getOption("verbose"))
	cat("\n	   This is a preliminary time series package for R\n",
	    "	   See `library(help=ts)' for details.\n\n")
}
options(ts.S.compat = FALSE)
