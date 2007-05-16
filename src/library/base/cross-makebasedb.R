local({
    omit <- c(".Last.value", ".AutoloadEnv", ".BaseNamespaceEnv",
              ".Device", ".Devices", ".Machine", ".Options", ".Platform")

    baseFileBase <- file.path(.libPaths()[1],"base","R","base")

    if (file.info(baseFileBase)["size"] < 20000) # crude heuristic
        stop("may already be using lazy loading on base");

    loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
    sys.source(baseFileBase, loadenv, keep.source = FALSE)

    basevars <- ls(loadenv, all.names=TRUE)
    basevars <- basevars[! basevars %in% omit]
    for(n in basevars) {
        f <- get(n, loadenv, inherits=FALSE)
        if(is.function(f)) {
            if(identical(environment(f), loadenv)) {
                environment(f) <- .BaseNamespaceEnv
                assign(n, f, envir=loadenv)
            }
        }
    }

    tools:::makeLazyLoadDB(loadenv, baseFileBase, variables = basevars)
})
