sessionInfo <- function(package=NULL)
{
    z <- list()
    z$R.version <- R.Version()
    z$R.version.string <- paste(R.version.string, R.version$platform, sep=", ")

    if(is.null(package)){
        package <- grep("^package:", search(), value=TRUE)
        package <- sub("^package:", "", package)
    }

    pkgDesc <- lapply(package, packageDescription)
    basePkgs <- sapply(pkgDesc,
                       function(x) !is.null(x$Priority) && x$Priority=="base")
    z$basePkgs <- package[basePkgs]
    if(any(!basePkgs)){
        z$otherPkgs <- pkgDesc[!basePkgs]
        names(z$otherPkgs) <- package[!basePkgs]
    }
    class(z) <- "sessionInfo"
    z
}

print.sessionInfo <- function(x, ...)
{
    cat(x$R.version.string,"\n\n")
    cat("attached base packages:\n")
    print(x$basePkgs)
    if(!is.null(x$otherPkgs)){
        cat("\nother attached packages:\n")
        print(sapply(x$otherPkgs,
                     function(x) x$Version))
    }

}

latex.sessionInfo <- function(object, ...)
{
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    z <- c("\\begin{itemize}",
           paste("  \\item", object$R.version.string),
           strwrap(paste("\\item Base packages:",
                         paste(sort(object$basePkgs), collapse=", ")),
                   indent=2, exdent=4))

    if(length(opkgver)){
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z,
               strwrap(paste("  \\item Other packages: ",
                             paste(names(opkgver), opkgver, sep="~",
                                   collapse=", ")),
                       indent=2, exdent=4))
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "latex"
    z
}


