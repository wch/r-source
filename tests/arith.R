options(digits=7)

## powers, including complex ones
outer(a <- -4:12,     -2:7, "^")
m <- outer(-4:12 +0i, b <- seq(-.5,2, by=.5), "^")
dimnames(m) <- list(paste(a), "^" = sapply(b,format))
round(m,3)

for (n1 in 1:7)
    print(zapsmall(polyroot(1:n1), digits = 10))

## fft():
for(n in 1:30) cat("\nn=",n,":", round(fft(1:n), 8),"\n")
