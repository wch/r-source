.onLoad <- function(lib, pkg)
{
#    library.dynam("ts", pkg, lib)
    options(ts.S.compat = FALSE)
}

.noGenerics <- TRUE
