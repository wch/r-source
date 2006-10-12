## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
reshapeWide <- function(x, i, j, val, jnames = levels(j)) .Defunct("reshape")
reshapeLong <- function(x,jvars,  ilev = row.names(x),
                        jlev = names(x)[jvars], iname = "reshape.i",
                        jname = "reshape.j", vname = "reshape.v")
    .Defunct("reshape")
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
print.coefmat <- function(x, digits=max(3, getOption("digits") - 2),
              signif.stars = getOption("show.signif.stars"),
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind, tst.ind, zap.ind = integer(0),
              P.values = NULL,
              has.Pvalue,
              eps.Pvalue = .Machine$double.eps,
              na.print = "", ...) .Defunct()
anovalist.lm <- function (object, ..., test = NULL) .Defunct()
lm.fit.null <- function(x, y, method = "qr", tol = 1e-07, ...)
    .Defunct("lm.fit")
lm.wfit.null <- function(x, y, w, method = "qr", tol = 1e-07, ...)
    .Defunct("lm.wfit")
glm.fit.null <- function(x, y, weights , start = NULL,
             etastart = NULL, mustart = NULL, offset,
             family = gaussian(), control = glm.control(),
             intercept = FALSE)
    .Defunct("glm.fit")
## </entry>

## <entry>
## Deprecated in 2.2.1
## Defunct in 2.4.0
mauchley.test <- function(...) .Defunct("mauchly.test")
## </entry>
