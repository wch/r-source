.noGenerics <- TRUE

.First.lib <- function(pkg, lib)
{
    cat("Package 'lqs' has been moved back to package 'MASS'\n")
    have.VR <- "package:MASS" %in% search()
    if(!have.VR) {
        if(require(MASS, quietly=TRUE))
            cat("Package 'MASS' has now been loaded\n")
        else
            cat("Package 'MASS' seems to be missing from this R installation\n")
    }
}
