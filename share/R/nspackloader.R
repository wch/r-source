local({
    info <- loadingNamespaceInfo()
    ns <- .Internal(getRegisteredNamespace(as.name(info$pkgname)))
    if (is.null(ns))
        stop("can't find name space environment");
    dbbase <- file.path(info$libname, info$pkgname, "R", info$pkgname)
    lazyLoad(dbbase, ns, filter = function(n) n != ".__NAMESPACE__.")
})
