"::" <- function(pkg, name) {
    pkg <- substitute(pkg)
    name <- substitute(name)
    if (as.character(pkg) == "base")
        get(as.character(name), env = .BaseNamespaceEnv)
    else
        stop(paste("unknown name space:", pkg))
}
