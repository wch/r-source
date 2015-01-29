#  File src/library/methods/R/zzz.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

## utils::globalVariables("...onLoad")

## Initial version of .onLoad
...onLoad  <-
  ## Initialize the methods package.
  function(libname, pkgname)
{
    where <- environment(sys.function())  # the namespace
    initMethodDispatch(where)
    ## temporary empty reference to the package's own namespace
    assign(".methodsNamespace", new.env(), envir = where)
    .Call(C_R_set_method_dispatch, TRUE)
    cat("initializing class and method definitions ...")
    ## set up default prototype (uses .Call so has be at load time)
    assign(".defaultPrototype", .Call(C_Rf_allocS4Object), envir = where)
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
       ..mergeClassDefSlots, .classGeneratorFunction, ..classEnv,
       ..addToMetaTable, ..extendsForS3,
       .InitClassDefinition, .InitBasicClasses, .initClassSupport,
       .InitMethodsListClass, .setCoerceGeneric, .makeBasicFuns,
       envir = where)
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
    assign(".checkRequiredGenerics", ..checkRequiredGenerics, envir = where)
    assign(".methodPackageSlots", ..methodPackageSlots, envir = where)
    rm(..isPrototype, .isSealedMethod, ..requirePackage, .implicitGeneric,
       ..checkRequiredGenerics, ..methodPackageSlots, .envRefMethods,
       .InitBasicClassMethods, .InitExtensions, .InitStructureMethods,
       .InitMethodDefinitions, .InitShowMethods, .InitClassUnion,
       .InitS3Classes, .InitSpecialTypesAndClasses, .InitTraceFunctions,
       .InitRefClasses, .initImplicitGenerics,
       envir = where)
    ## unlock some bindings that must be modifiable
    unlockBinding(".BasicFunsList", where)
    assign(".saveImage", TRUE, envir = where)
    cat(" done\n")

    assign("envRefMethodNames",
	   names(getClassDef("envRefClass")@refMethods), envir = where)
    assign(".onLoad", ..onLoad, envir = where)
    rm(...onLoad, ..onLoad, envir = where)
    dbbase <- file.path(libname, pkgname, "R", pkgname)
    ns <- asNamespace(pkgname)
    ## we need to exclude the registration vars
    vars <- grep("^C_", names(ns), invert = TRUE, value = TRUE)
    tools:::makeLazyLoadDB(ns, dbbase, variables = vars)
}

## avoid warnings from static analysis code by extra call
.onLoad <- function(libname, pkgname) ...onLoad(libname, pkgname)

##  .onLoad for routine use, installed by ...onLoad
..onLoad <- function(libname, pkgname)
{
    where <- environment(sys.function())  # the namespace
    initMethodDispatch(where)
    .Call(C_R_set_method_dispatch, TRUE)
    ## initialize generics cache more thoroughly:
    setPrimitiveMethods("$", `$`, code="reset", generic = getGeneric("$"), mlist = NULL)
    assign(".methodsNamespace", where, where)
    ## assign to baseenv also, signalling methods loaded
    assign(".methodsNamespace", where, baseenv())
    if(Sys.getenv("R_S4_BIND") == "active")
        methods:::bind_activation(TRUE)
}

.onUnload <- function(libpath)
{
    message("unloading 'methods' package ...") # see when this is called
    .isMethodsDispatchOn(FALSE)
    methods:::bind_activation(FALSE)
    library.dynam.unload("methods", libpath)
}


.onAttach <- function(libname, pkgname)
{
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

.onDetach <- function(libpath) methods:::.onUnload(libpath)

## redefining it here, invalidates the one above:
## Why don't we unload "methods" on detach() ?
.onDetach <- function(libpath) .isMethodsDispatchOn(FALSE)

## used for .methodsIsLoaded
.saveImage <- FALSE

## want ASCII quotes, not fancy nor translated ones
.dQ <- function (x) paste0('"', x, '"')
