#  File src/library/methods/R/packageName.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## utilities to manage package names

getPackageName <- function(where = topenv(parent.frame()), create = TRUE) {
    env <- as.environment(where)
    notSaved <- is.null(pkg <- env[[".packageName"]])
    if(notSaved) {
	pkg <- if(identical(where, 1) || identical(env, topenv(parent.frame())))
	    Sys.getenv("R_PACKAGE_NAME")
	else ""
    }
    envName <- environmentName(env)
    if(nzchar(envName) && grepl("package:", envName, fixed = TRUE))
	pkg <- .rmpkg(envName)
    if(!nzchar(pkg)) { ## is still ""
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
            else if(nzchar(envName))
                pkg <- envName
            else
                pkg <- as.character(where)

	    pkg <- .rmpkg(pkg)
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
    if (!nzchar(pkg)) { # recurse to top
        top <- topenv(env)
        if (!identical(top, env))
            pkg <- getPackageName(top, create=create)
    }
    if(!nzchar(pkg) && create) {
        pkg <- as.character(Sys.time())
        warning(gettextf("Created a package name, %s, when none found",
                         sQuote(pkg)),
                domain = NA)
	.PackageEnvironments[[pkg]] <- env
	if(notSaved && !environmentIsLocked(env))
            setPackageName(pkg, env)
    }
    pkg
}

setPackageName <- function(pkg, env) env[[".packageName"]] <- pkg

##FIXME:  rather than an attribute, the className should have a formal class
## (but there may be bootstrap problems)
packageSlot <- function(object)
    attr(object, "package")

`packageSlot<-` <- function(object, value) {
    attr(object, "package") <- value
    object
}
