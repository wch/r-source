## utilities to manage package names

getPackageName <- function(where = topenv(parent.frame())) {
    pkg <- ""
    if(exists(".packageName", where, inherits = FALSE))
        pkg <- get(".packageName", where)
    else  if(identical(where, 1) || identical(as.environment(where), topenv(parent.frame())))
        pkg <- Sys.getenv("R_PACKAGE_NAME")
    if(!nzchar(pkg)) {
        env <- as.environment(where)
        if(identical(env, .GlobalEnv))
            pkg <- ".GlobalEnv"
        else if(identical(env, .BaseNamespaceEnv))
            pkg <- "base"
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
                pkg <- substr(pkg, 9, nchar(pkg, "c"))
        }
#  Problem:  the library() function should now be putting .packageName in package environments
#   but namespace makes them invisible from outside.
        ## save the package name, but .GlobalEnv is not a package name,
        ## and package base doesn't have a .packageName (yet?)
#         if(!(identical(pkg, ".GlobalEnv") || identical(pkg, "base")) ) {
#             setPackageName(pkg, env)
#             ## packages OUGHT
#             ## to be self-identifying
#              warning("The package name \"", pkg, "\" was inferred, but not found in that package")
#         }
    }
    pkg
}

setPackageName <- function(pkg, env)
    assign(".packageName", pkg, env)

##FIXME:  rather than an attribute, the className should have a formal class
## (but there may be bootstrap problems)
packageSlot <- function(object)
    attr(object, "package")

"packageSlot<-" <- function(object, value) {
    attr(object, "package") <- value
    object
}
