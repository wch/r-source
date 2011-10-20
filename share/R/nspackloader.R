local({
    info <- loadingNamespaceInfo()
    ns <- .Internal(getRegisteredNamespace(as.name(info$pkgname)))
    if (is.null(ns))
        stop("cannot find namespace environment");
    barepackage <- sub("([^-]+)_.*", "\\1", info$pkgname)
    dbbase <- file.path(info$libname, info$pkgname, "R", barepackage)
    lazyLoad(dbbase, ns, filter = function(n) n != ".__NAMESPACE__.")
})
