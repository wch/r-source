###---- ALL tests here should return  TRUE !
###
### '##P': This lines give not 'T' but relevant ``Print output''
###

Meps <- .Machine $ double.eps
abs(1- .Machine$double.xmax * 10^(-.Machine$double.max.exp*log10(2)))/Meps < 1e3
abs(1- .Machine$double.xmin * 10^(-.Machine$double.min.exp*log10(2)))/Meps < 1e3
##P (1- .Machine$double.xmax * 10^(-.Machine$double.max.exp*log10(2)))/Meps
##P (1- .Machine$double.xmin * 10^(-.Machine$double.min.exp*log10(2)))/Meps
log10(.Machine$double.xmax) / log10(2) == .Machine$double.max.exp
log10(.Machine$double.xmin) / log10(2) == .Machine$double.min.exp


abs(Im(cos(acos(1i))) -  1) < 2*Meps
abs(Im(sin(asin(1i))) -  1) < 2*Meps
abs(Im(acos(cos(1i))) -  1) < 4*Meps
abs(Im(asin(sin(1i))) -  1) < 2*Meps
##P (1 - Im(sin(asin(Ii))))/Meps
##P (1 - Im(cos(acos(Ii))))/Meps

.Random.seed <- c(629, 6137, 22167) # want reproducible output
Isi <- Im(sin(asin(1i + rnorm(100))))
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)  / Meps)
Isi <- Im(cos(acos(1i + rnorm(100))))
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)  / Meps)
Isi <- Im(atan(tan(1i + rnorm(100)))) #-- tan(atan(..)) does NOT work (Math!)
all(abs(Isi-1) < 100* Meps)
##P table(2*abs(Isi-1)  / Meps)

all(names(c(a=pi, b=1, d=1:4)) == c("a","b", paste("d", 1:4, sep="")))
##P names(c(a=pi, b=1, d=1:4))
ncb <- dimnames(cbind(a=1, yy=1:3))[[2]]
(!is.null(ncb)) && all(ncb == c("a","yy"))

all(cbind(a=1:2, b=1:3, c=1:6) == t(rbind(a=1:2, b=1:3, c=1:6)))
##P rbind(a=1:2, b=1:3, c=1:6)

