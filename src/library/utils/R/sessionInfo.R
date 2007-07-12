sessionInfo <- function(package=NULL)
{
    z <- list()
    z$R.version <- R.Version()
    z$locale <- Sys.getlocale()

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
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% package)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        z$loadedOnly <- pkgDesc[loadedOnly]
    }
    class(z) <- "sessionInfo"
    z
}

print.sessionInfo <- function(x, ...)
{
    mkLabel <- function(L, n) {
        vers <- sapply(L[[n]], function(x) x[["Version"]])
        pkg <-  sapply(L[[n]], function(x) x[["Package"]])
        paste(pkg, vers, sep="_")
    }

    cat(x$R.version$version.string, "\n")
    cat(x$R.version$platform, "\n\n")
    cat("locale:\n")
    cat(x$locale, "\n\n", sep="")
    cat("attached base packages:\n")
    print(x$basePkgs, quote=FALSE)
    if(!is.null(x$otherPkgs)){
        cat("\nother attached packages:\n")
        print(mkLabel(x, "otherPkgs"), quote=FALSE)
    }
    if(!is.null(x$loadedOnly)){
        cat("\nloaded via a namespace (and not attached):\n")
        print(mkLabel(x, "loadedOnly"), quote=FALSE)
    }
    x
}

toLatex.sessionInfo <- function(object, ...)
{
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("\\begin{itemize}",
           paste("  \\item ", object$R.version$version.string,
                 ", \\verb|", object$R.version$platform, "|", sep=""),
           paste("  \\item Locale: \\verb|", object$locale, "|", sep=""),
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
    if(length(nspkgver)){
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z,
               strwrap(paste("  \\item Loaded via a namespace (and not attached): ",
                             paste(names(nspkgver), nspkgver, sep="~",
                                   collapse=", ")),
                       indent=2, exdent=4))
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "Latex"
    z
}
