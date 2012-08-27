#  File src/library/methods/R/zzz.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

utils::globalVariables(c(".possibleExtends", ".makeGeneric",
                         ".newClassRepresentation", ".classGeneratorFunction",
                         ".mergeClassDefSlots", "..mergeClassDefSlots"))

..First.lib  <-
  ## Initialize the methods package.  Called from .onLoad, first
  ## during INSTALL, when saved will be FALSE, at which time
  ## the serious computation is done.  (The name ..First.lib is only historical)
  function(libname, pkgname, where)
{
    if(missing(where)) {
        where <- match(paste0("package:", pkgname), search())
        if(is.na(where)) {
            warning(gettextf("not a package name: %s", sQuote(pkgname)),
                    domain = NA)
            return()
        }
        where <- as.environment(where)
    }
    initMethodDispatch(where)
    ## temporary empty reference to the package's own namespace
    assign(".methodsNamespace", new.env(), envir = where)
    .Call("R_set_method_dispatch", TRUE, PACKAGE = "methods")
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        ## optionally turn off old-style mlists
        mopt <- Sys.getenv("R_MLIST")
        .noMlistsFlag <<- (is.character(mopt) && all(mopt != "YES"))
        if(!.noMlistsFlag)
            cat("Initializing with support for old-style methods list objects\n")
        cat("initializing class and method definitions ...\n")
        on.exit(assign(".saveImage", NA, envir = where))
        ## set up default prototype (uses .Call so has be at load time)
        assign(".defaultPrototype",
                .Call("Rf_allocS4Object",PACKAGE="methods"),
               envir = where)
        assign(".SealedClasses", character(), envir = where)
        .InitClassDefinition(where)
        assign("possibleExtends", .possibleExtends, envir = where)
        .InitBasicClasses(where)
        .initClassSupport(where)
        .InitMethodsListClass(where)
        .setCoerceGeneric(where)
        ## now install the non-dummy versions of some functions
        assign(".classEnv", ..classEnv, envir = where)
        assign("makeGeneric", .makeGeneric, envir = where)
        assign("newClassRepresentation", .newClassRepresentation, envir = where)
        assign("classGeneratorFunction", .classGeneratorFunction, envir = where)
        assign(".mergeClassDefSlots", ..mergeClassDefSlots, envir = where)
        assign(".addToMetaTable", ..addToMetaTable, envir = where)
        assign(".extendsForS3", ..extendsForS3, envir = where)
        .makeBasicFuns(where)
        rm(.makeGeneric, .newClassRepresentation, .possibleExtends,
           ..mergeClassDefSlots, .classGeneratorFunction, envir = where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".isPrototype", ..isPrototype, envir = where)
        .InitClassUnion(where)
        .InitS3Classes(where)
        .InitSpecialTypesAndClasses(where)
        .InitTraceFunctions(where)
        .InitRefClasses(where)
        ## now seal the classes defined in the package
        for(cl in get(".SealedClasses", where))
            sealClass(cl, where)
        assign("isSealedMethod", .isSealedMethod, envir = where)
        assign(".requirePackage", ..requirePackage, envir = where)
        ## initialize implicit generics for base package
        ## Note that this is done before making a non-vacuous implicitGeneric()
        ## so that non-default signatures are allowed in setGeneric()
        .initImplicitGenerics(where)
        assign("implicitGeneric", .implicitGeneric, envir = where)
        cacheMetaData(where, TRUE, searchWhere = .GlobalEnv, FALSE)
        assign(".checkRequiredGenerics", ..checkRequiredGenerics,envir = where)
        assign(".methodPackageSlots", ..methodPackageSlots, envir = where)
        ## unlock some bindings that must be modifiable
        unlockBinding(".BasicFunsList", where)
         assign(".saveImage", TRUE, envir = where)
        on.exit()
        cat("done\n")
    }
    else {
        if(!isTRUE(saved))
            stop("maybe the methods package was not installed correctly; check the make results!",
                 domain = NA)

        ## assign the environment of a function from the methods
        ## package-- the namespace of the methods package, if it has
        ## one, or the global environment

        mns <- environment(get("setGeneric", where))
        assign(".methodsNamespace", mns, where)
        ## assign to baseenv also, signalling methods loaded
        assign(".methodsNamespace", mns, baseenv())
    }
}

.onLoad <- function(libname, pkgname) {
    env <- environment(sys.function())
    doSave <- identical(get(".saveImage", envir = env), FALSE)
    ..First.lib(libname, pkgname, env)
    if(doSave) {
        dbbase <- file.path(libname, pkgname, "R", pkgname)
        ns <- asNamespace(pkgname)
        tools:::makeLazyLoadDB(ns, dbbase)
    }
    if(Sys.getenv("R_S4_BIND") == "active")
        methods:::bind_activation(TRUE)
}

.onUnload <- function(libpath) {
    cat("unloading 'methods' package ...\n")# see when this is called
    .isMethodsDispatchOn(FALSE)
    methods:::bind_activation(FALSE)
    library.dynam.unload("methods", libpath)
}


.onAttach <- function(libname, pkgname) {
    env <- environment(sys.function())
    ## unlock some bindings that must be modifiable
    unlockBinding(".BasicFunsList", env)
    if(methods:::.hasS4MetaData(.GlobalEnv)) {
        result <- try(cacheMetaData(.GlobalEnv, TRUE))
        ## still attach  methods package if global env has bad objets
        if(is(result, "try-error"))
          warning("apparently bad method or class metadata in saved environment;\n",
                  "move the file or remove the class/method")
    }
}

.Last.lib <- function(libpath) {
    methods:::.onUnload(libpath)
}
## redefining it here, invalidates the one above:
## Why don't we unload "methods" on detach() ?
.Last.lib <- function(libpath) .isMethodsDispatchOn(FALSE)

.saveImage <- FALSE
## cat("Saving namespace image ...\n")

## want ASCII quotes, not fancy nor translated ones
.dQ <- function (x) paste0('"', x, '"')
