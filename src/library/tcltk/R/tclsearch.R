
addTclPath<-function(directory=".") {
    a<-.Tcl("set auto_path")
    paths<-strsplit(a," ")[[1]]
    if (is.na(match(directory,paths)))
        .Tcl(paste("lappend auto_path ",directory))
    invisible(paths)
}

tclRequire<-function(pkg,warn=TRUE){
    a<-.Tcl(paste("package versions ",pkg))
    if (length(a)==1 && nchar(a)==""){
        if (warn) warning(paste("Tcl package",pkg,"not found."))
        return(FALSE)
    }
    else
        .Tcl(paste("package require ",pkg))
}
