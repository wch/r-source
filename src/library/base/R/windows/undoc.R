undoc <- function(pkg)
{
    if (!missing(pkg)) {
        pkgdir <- system.file(pkg = pkg)
        if (length(pkgdir) > 1) {
            warning(paste("package `", pkg, "' found more than once"))
            pkgdir <- pkgdir[1]
        }
        if (pkgdir == "")
            stop(paste("package `", pkg, "' is not installed"))
        code.file <- file.path(pkgdir, "R", pkg)
        objsdocs <- sort(scan(file = file.path(pkgdir, "help",
                              "AnIndex"),
                              what = list("", ""),
                              quiet = TRUE, sep="\t")[[1]])
    }
    else {
        stop("you must supply `pkgs'")
    }
    lib.source <- function(file, env) {
        oop <- options(keep.source = FALSE)
        on.exit(options(oop))
        exprs <- parse(n = -1, file = file)
        if (length(exprs) == 0)
            return(invisible())
        for (i in exprs) yy <- eval(i, env)
        invisible()
    }
    .CodeEnv <- new.env()
    lib.source(code.file, env = .CodeEnv)
    objscode <- ls(envir = .CodeEnv, all.names = TRUE)
    ## Undocumented objects?
    objscode[! objscode %in% c(objsdocs, ".First.lib")]
}
