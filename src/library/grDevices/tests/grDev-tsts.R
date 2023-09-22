## xy.coords() and xyz.coords() -- gets *classed* warning
tools::assertWarning(xy.coords(-2:10, log = "y"), verbose=TRUE)
op <- options(warn = 2)# ==> warnings are errors
suppressWarnings(xy.coords(-2:10, log = "y"), classes="log_le_0") -> xy
stopifnot(identical(xy$y, c(rep(NA_real_,3), 1:10)))
options(op) # (reverting)
tools::assertWarning(xy.coords(-2:10, log = "y"), verbose=TRUE)
