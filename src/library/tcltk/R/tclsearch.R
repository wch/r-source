
addTclPath <- function(path = ".")
{
    a <- tclvalue(.Tcl("set auto_path"))
    paths <- strsplit(a," ")[[1]]
    if (is.na(match(path, paths)))
        .Tcl(paste("lappend auto_path ", path))
    invisible(paths)
}

tclRequire <- function(package, warn = TRUE)
{
    a <- tclvalue(.Tcl(paste("package versions ", package)))
    if (length(a)==1 && nchar(a)==""){
        if (warn) warning(paste("Tcl package", package, "not found."))
        return(FALSE)
    }
    else
        .Tcl(paste("package require ", package))
}
