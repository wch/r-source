local({
    info <- loadingNamespaceInfo()
    ns <- .Internal(getRegisteredNamespace(as.name(info$pkgname)))
    if (is.null(ns))
        stop("can't find name space environment");
    dataFile <- file.path(info$libname, info$pkgname, "R", "all.rda")
    load(dataFile, ns)
})
