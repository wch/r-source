## some examples of the KS test

## unrealistic one of PR#14561
ds1 <- c(1.7,2,3,3,4,4,5,5,6,6)
ks.test(ds1, "pnorm", mean = 3.3, sd = 1.55216)
# how on earth can sigma = 1.55216 be known?

# R >= 2.14.0 allows the equally invalid
ks.test(ds1, "pnorm", mean = 3.3, sd = 1.55216, exact = TRUE)

## Try out the effects of rounding
set.seed(123)
ds2 <- rnorm(1000)
ks.test(ds2, "pnorm") # exact = FALSE is default for n = 1000
ks.test(ds2, "pnorm", exact = TRUE)
## next two are still close
ks.test(round(ds2, 2), "pnorm")
ks.test(round(ds2, 2), "pnorm", exact = TRUE)
# now D has doubled, but p-values remain similar (if very different from ds2)
ks.test(round(ds2, 1), "pnorm")
ks.test(round(ds2, 1), "pnorm", exact = TRUE)

