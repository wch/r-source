.Defunct <- function(new, package=NULL) {
    msg <- sprintf(gettext("'%s' is defunct.\n"),
                   as.character(sys.call(sys.parent())[[1]]))
    if(!missing(new))
        msg <- c(msg, sprintf(gettext("Use '%s' instead.\n"), new))
    if(!is.null(package))
        msg <- c(msg,
                 sprintf(gettext("See help(\"Defunct\") and help(\"%s-defunct\")."), package))
    else msg <- c(msg, gettext("See help(\"Defunct\")"))
    stop(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

Version <- function() .Defunct("R.Version")
provide <- function(package) .Defunct()

## <entry>
## Deprecated in 1.2.0
## Defunct in 1.3.0
getenv <- function(...) .Defunct("Sys.getenv")
## </entry>

## <entry>
## Deprecated in 1.2.3
## Defunct in 1.3.0
## Removed in 1.4.0: conflicts with lattice
## dotplot <- function(...) .Defunct()
## stripplot <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.3.0
## Defunct in 1.4.0
read.table.url <- function(url, method, ...) .Defunct("read.table(url())")
scan.url <- function(url, file = tempfile(), method, ...)
    .Defunct("scan(url())")
source.url <- function(url, file = tempfile(), method, ...)
    .Defunct("source(url())")
httpclient <- function(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
                       file=tempfile(), drop.ctrl.z=TRUE)
    .Defunct()
parse.dcf <- function(text = NULL, file = "", fields = NULL,
                      versionfix = FALSE) .Defunct("read.dcf")
## </entry>

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
.Alias <- function(expr) .Defunct()
reshapeWide <- function(x, i, j, val, jnames = levels(j)) .Defunct("reshape")
reshapeLong <- function(x,jvars,  ilev = row.names(x),
                        jlev = names(x)[jvars], iname = "reshape.i",
                        jname = "reshape.j", vname = "reshape.v")
    .Defunct("reshape")
## </entry>

## <entry>
## Deprecated in 1.5.0
## Defunct in 1.6.0
piechart <- function(x, labels = names(x), edges = 200, radius = 0.8,
                     density = NULL, angle = 45, col = NULL, main = NULL, ...)
    .Defunct("pie")
## </entry>

## <entry>
## Deprecated in 1.6.0
## Defunct in 1.7.0
machine <- function() .Defunct()
Machine <- function() .Defunct(".Machine")
Platform <- function() .Defunct(".Platform")
restart <- function() .Defunct("try")
## </entry>

## <entry>
## Deprecated in 1.7.0
## Defunct in 1.8.0
printNoClass <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
                         print.gap = NULL, right = FALSE, ...)
    .Defunct()
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
codes <- function(x, ...) .Defunct()
codes.factor <- function(x, ...) .Defunct("unclass")
codes.ordered <- function(x, ...) .Defunct("unclass")
"codes<-" <- function(x, ..., value) .Defunct()
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
print.atomic <- function(x, quote = TRUE, ...) .Defunct("print.default")
## </entry>

## <entry>
## Deprecated in 1.9.0
## Defunct in 2.0.0
La.eigen <- function(x, symmetric, only.values = FALSE,
                     method = c("dsyevr", "dsyev")) .Defunct("eigen")
tetragamma <- function(x) .Defunct("psigamma")
pentagamma <- function(x) .Defunct("psigamma")
package.description <- function(pkg, lib.loc = NULL, fields = NULL)
    .Defunct("packageDescription")
## </entry>
