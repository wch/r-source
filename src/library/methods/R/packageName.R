## utilities to manage package names

getPackageName <- function(where = 1) {
    ## the default for where should really be "top level environment"
    ## to work correctly during all installations.
    pkg <- ""
    if(exists(".packageName", where, inherits = FALSE))
        pkg <- get(".packageName", where)
    else  if(identical(where, 1) || identical(as.environment(where), .GlobalEnv))
        pkg <- Sys.getenv("R_PACKAGE_NAME")
    if(nchar(pkg) == 0) {
        env <- as.environment(where)
        if(identical(env, .GlobalEnv))
            pkg <- ".GlobalEnv"
        else {
            if(is.numeric(where))
                pkg <- search()[[where]]
            else if(is.environment(where)) {
                for(db in search())
                    if(identical(as.environment(db), where)) {
                        pkg <- db; break
                    }
            }
            else
                pkg <- as.character(where)
            if(identical(substr(pkg, 1, 8), "package:"))
                pkg <- substr(pkg, 9, nchar(pkg))
        }
        ## save the package name, but not in the global environment
        ## (where it might cause confusion if the image is saved)
        if(!identical(pkg, ".GlobalEnv")) {
            setPackageName(pkg, env)
            ## uncomment the following if we decide that packages OUGHT
            ## to be self-identifying
            ##   warning("The package name \"", pkg, "\" was inferred, but not found in that package")
        }
    }
    pkg
}

setPackageName <- function(pkg, env)
    assign(".packageName", pkg, env)

functionPackageName <- function(name) {
    where <- findFunction(name)
    if(length(where) ==0)
        stop(paste("No function \"", name, "\" found"))
    else if(length(where) > 1)
       sapply(as.list(where), getPackageName)
    else
        getPackageName(where)
}
