
addTclPath <- function(path = ".")
{
    a <- tclvalue(tcl("set", "auto_path"))
    paths <- strsplit(a, " ", fixed=TRUE)[[1]]
    if (is.na(match(path, paths)))
        tcl("lappend", "auto_path", path)
    invisible(paths)
}

tclRequire <- function(package, warn = TRUE)
{
    a <- try(tcl("package", "require", package), silent=TRUE)
    if (inherits(a, "try-error")){
        if (warn)
            warning(gettextf("Tcl package '%s' not found", package),
                    domain = NA)
        return(FALSE)
    } else return(a)
}
