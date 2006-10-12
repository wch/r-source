## <entry>
## Deprecated in 2.2.0
CRAN.packages <- function(CRAN = getOption("repos"), method,
                          contriburl = contrib.url(CRAN))
{
    .Deprecated("available.packages")
    available.packages(contriburl = contriburl, method = method)
}
## </entry>
