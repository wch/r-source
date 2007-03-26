.Defunct <- function(new, package=NULL, msg) {
    if (missing(msg)) {
	msg <- gettextf("'%s' is defunct.\n",
			as.character(sys.call(sys.parent())[[1]]))
	if(!missing(new))
	    msg <- c(msg, gettextf("Use '%s' instead.\n", new))
	msg <- c(msg,
		 if(!is.null(package))
		 gettextf("See help(\"Defunct\") and help(\"%s-defunct\").",
			  package)
		 else gettext("See help(\"Defunct\")"))
    }
    else msg <- as.character(msg)

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
codes <- function(x, ...) .Defunct()
codes.factor <- function(x, ...) .Defunct("unclass")
codes.ordered <- function(x, ...) .Defunct("unclass")
"codes<-" <- function(x, ..., value) .Defunct()
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

## <entry>
## Deprecated in 2.1.0
## Defunct in 2.2.0
delay <- function(x, env=.GlobalEnv) .Defunct("delayedAssign")
loadURL <- function (url, envir = parent.frame(), quiet = TRUE, ...)
    .Defunct("load(url())")
## </entry>

## Defunct in 2.3.0
write.table0 <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
    .Defunct("write.table")
format.char <- function(x, width = NULL, flag = "-")
    .Defunct("format.default")
## </entry>

## <entry>
## Deprecated in 2.3.0
## Defunct in 2.4.0
La.chol <- function(x) .Defunct("chol")
La.chol2inv <- function(x, size = ncol(x)) .Defunct("chol2inv")
## </entry>

## <entry>
## Deprecated in 2.4.0
## Defunct in 2.5.0
symbol.C <- function(name)
{
    warning("'symbol.C' is not needed: please remove it", immediate.=TRUE)
    name
}
symbol.For <- function(name)
{
    warning("'symbol.For' is not needed: please remove it", immediate.=TRUE)
    name
}
## </entry>

## <entry>
## Deprecated in 1999
## Defunct in 2.5.0
unix <- function(call, intern = FALSE) .Defunct("system")
## </entry>


