.Defunct <- function(new, package=NULL) {
    stop(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
	       " is defunct.\n",
               if (!missing(new))
               paste("Use", sQuote(new), "instead.\n"),
	       "See help(\"Defunct\")",
               if(!is.null(package))
               paste(" and help(\"", package, "-defunct\").", sep=""),
	       sep = ""),
         call. = FALSE)
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
read.table.url <- function(...) .Defunct("read.table(url())")
scan.url <- function(...) .Defunct("scan(url())")
source.url <- function(...) .Defunct("source(url())")
httpclient <- function(...) .Defunct()
parse.dcf <- function(...) .Defunct("read.dcf")
## </entry>

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
.Alias <- function(...) .Defunct()
reshapeLong <- function(...) .Defunct("reshape")
reshapeWide <- function(...) .Defunct("reshape")
## </entry>

## <entry>
## Deprecated in 1.5.0
## Defunct in 1.6.0
piechart <- function(...) .Defunct("pie")
## </entry>

## <entry>
## Deprecated in 1.6.0
## Defunct in 1.7.0
machine <- function(...) .Defunct()
Machine <- function(...) .Defunct(".Machine")
Platform <- function(...) .Defunct(".Platform")
restart <- function(...) .Defunct("try")
## </entry>

## <entry>
## Deprecated in 1.7.0
## Defunct in 1.8.0
printNoClass <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
print.coefmat <- function(...) .Defunct()
codes <- function(x, ...) .Defunct()
codes.factor <- function(x, ...) .Defunct("unclass")
codes.ordered <- function(x, ...) .Defunct("unclass")
"codes<-" <- function(x, ..., value) .Defunct()
anovalist.lm <- function (...) .Defunct()
lm.fit.null <- function(...) .Defunct("lm.fit")
lm.wfit.null <- function(...) .Defunct("lm.wfit")
glm.fit.null <- function(...) .Defunct("glm.fit")
print.atomic <- function(...) .Defunct("print.default")
## </entry>

## <entry>
## Deprecated in 1.9.0
## Defunct in 2.0.0
La.eigen <- function(...) .Defunct("eigen")
tetragamma <- function(x) .Defunct("psigamma")
pentagamma <- function(x) .Defunct("psigamma")
package.description <- function(...) .Defunct("packageDescription")
## </entry>
