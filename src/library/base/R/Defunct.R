.Defunct <- function() {
    stop(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
	       " is defunct.\n",
	       "See ?Defunct.",
	       sep = ""),
         call. = FALSE)
}

Version <- function() .Defunct()
provide <- function(package) .Defunct()

## <entry>
## Deprecated in 1.2.0
## Defunct in 1.3.0
getenv <- function(...) .Defunct()
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
read.table.url <- function(...) .Defunct()
scan.url <- function(...) .Defunct()
source.url <- function(...) .Defunct()
httpclient <- function(...) .Defunct()
parse.dcf <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
.Alias <- function(...) .Defunct()
reshapeLong <- function(...) .Defunct()
reshapeWide <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.5.0
## Defunct in 1.6.0
piechart <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.6.0
## Defunct in 1.7.0
machine <- function(...) .Defunct()
Machine <- function(...) .Defunct()
Platform <- function(...) .Defunct()
restart <- function(...) .Defunct()
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
codes.factor <- function(x, ...) .Defunct()
codes.ordered <- function(x, ...) .Defunct()
"codes<-" <- function(x, ..., value) .Defunct()
anovalist.lm <- function (...) .Defunct()
lm.fit.null <- function(...) .Defunct()
lm.wfit.null <- function(...) .Defunct()
glm.fit.null <- function(...) .Defunct()
print.atomic <- function(...) .Defunct()
## </entry>
