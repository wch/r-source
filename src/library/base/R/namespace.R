"::" <- function(pkg, name) {
    pkg <- substitute(pkg)
    name <- substitute(name)
    if (as.character(pkg) == "base")
        get(as.character(name), env = .BaseNamespaceEnv)
    else
        stop(paste("unknown name space:", pkg))
}

topenv <- function(envir = parent.frame()) {
    while (! is.null(envir)) {
        if (! is.null(attr(envir, "name")) ||
            identical(envir, .GlobalEnv) ||
            .Internal(isNamespaceEnv(envir)))
            return(envir)
        else envir <- parent.env(envir)
    }
    return(.GlobalEnv)
}
