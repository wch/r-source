undoc <- function(pkg, dir, code.dir, docs.dir) {
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
                              quiet = TRUE)[[1]])
    }
    else {
        if (missing(code.dir)) {
            if (missing(dir))
                stop("cannot get code")
            else {
                dir <- system(paste("cd", dir, "; pwd"), intern = TRUE)
                code.dir <- file.path(dir, "R")
            }
        }
        code.file <- tempfile()
        on.exit(unlink(code.file))
        cmd <- paste("cat",
                     "`ls",
                     file.path(code.dir, "*.[Rr]"),
                     file.path(code.dir, "unix", "*.[Rr]"),
                     "2>/dev/null",
                     "| sed '/zzz.*/d'`")
        system(paste(cmd, ">", code.file))
        if (missing(docs.dir)) {
            if (missing(dir))
                stop("cannot get docs")
            else
                docs.dir <- file.path(dir, "man")
        }
        cmd <- paste("grep -h '^\\\\\\(alias\\|name\\)'",
                     "`ls",
                     file.path(docs.dir, "*[Rr]d"),
                     file.path(docs.dir, "unix", "*.[Rr]d"),
                     "2>/dev/null`", 
                     "| sed 's/\\\\\\(alias\\|name\\){\\(.*\\)}/\\2/'")
        objsdocs <- system(cmd, intern = TRUE)
        objsdocs <- gsub("\\\\%", "%", objsdocs)
        objsdocs <- gsub(" ", "", objsdocs)
        objsdocs <- sort(unique(objsdocs))
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
