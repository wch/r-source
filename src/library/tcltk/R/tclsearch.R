
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
    a <- tclvalue(tcl("package","versions", package))
    if (length(a)==1 && nchar(a)==""){
        if (warn) warning(paste("Tcl package", package, "not found."))
        return(FALSE)
    }
    else
        tcl("package", "require", package)
}
