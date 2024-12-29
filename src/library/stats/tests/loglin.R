### examples from investigating UBSAN reports in vcdExtra::seq_loglm

x <- margin.table(Titanic, 1)
## Next two had UBSAN reports in R 4.5.0 from using --x on a 0-length R-allocated  array.
loglin(x, NULL)
loglin(x, list())

loglin(x, list(1))

## failed in R 4.4.2 as R code assumed >= 2 parameters
loglin(x, NULL, param = TRUE)
loglin(x, list(1), param = TRUE)
