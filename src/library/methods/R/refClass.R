#  File src/library/methods/R/refClass.R
#  Part of the R package, http://www.R-project.org
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


## Classes to support OOP-style classes with reference-semantics for fields
## and class-based methods.
## Implementation of the R-based version of these classes (using environments)


envRefInferField <- function(self, field, thisClass, selfEnv = as.environment(self)) {
    'Install a field method into the environment of object
self from reference class thisClass.'
    fields <- thisClass@fieldPrototypes
    if(exists(field, envir = fields, inherits = FALSE)) {
        ## this allows lazy installation of fields (not currently used)
        value <- get(field, envir = fields)
    }
    else {
        methods <- thisClass@refMethods
        if(exists(field, envir = methods, inherits = FALSE)) {
            value <- get(field, envir = methods)
            ## install this method and any methods it may call
            value <- installClassMethod(value, self, field, selfEnv, thisClass)
        }
        else
            stop(gettextf("Field \"%s\" is not a valid field or method name for reference class \"%s\"",
                          field, thisClass@className),
                 domain = NA)
    }
    value
}

installClassMethod <- function(def, self, me, selfEnv, thisClass) {
    if(!is(def, "refMethodDef")) {  #should not happen? => need warning
        warning(gettextf("Method %s from class %s was not processed into a class method until being installed.  Possible corruption of the methods in the class.",
                         me, thisClass@className),
                domain = NA)
        def <-makeClassMethod(def, me, thisClass@className, "", objects(thisClass@refMethods, all.names = TRUE))
        .checkFieldsInMethod(def, names(thisClass@fieldClasses))
        ## cache the analysed method definition
        assign(me, def, envir = thisClass@refMethods)
    }
    depends <- def@mayCall
    environment(def) <- selfEnv # for access to fields and methods
    assign(me, def, envir = selfEnv)
    ## process those that are not in the instance environment, now that
    ## this method has been assigned.
    done <- objects(selfEnv, all.names = TRUE)
    notDone <- depends[is.na(match(depends, done))]
    superCase <- match("callSuper", notDone, 0)
    if(superCase > 0) {
        if(nzchar(def@superClassMethod))
            notDone[[superCase]] <- def@superClassMethod
        else
            stop(gettextf(
            "a call to superClass() is in the method \"%s\" but there is no superclass definition of this method for class \"%s\"",
               me, thisClass@className), domain = NA)
    }
    for(what in notDone)
        installClassMethod(get(what, envir = thisClass@refMethods), self, what, selfEnv, thisClass)
    if(superCase > 0) {
        ## provide an environment with the correct callSuper() definition,
        ## with selfEnv as its parent (can't override the definition of "callSuper"
        ## in selfEnv--there may  be other methods with a callSuper() in them
        newEnv <- new.env(FALSE, parent = selfEnv)
        assign("callSuper", get(def@superClassMethod, envir = selfEnv),
               envir = newEnv)
        environment(def) <- newEnv
        assign(me, def, envir = selfEnv)
        ## the callSuper() inside def now goes to the right method
    }
    def
   }

..hasCodeTools <- FALSE
.hasCodeTools <- function() {
    if(!identical(..hasCodeTools, TRUE)) # will be FALSE when methods is built, keep checking
        .assignOverBinding("..hasCodeTools",length(list.files(system.file(package = "codetools"))) > 0,
                           .methodsNamespace, FALSE)
    ..hasCodeTools
}

.getGlobalFuns <- function(def) {
    if(.hasCodeTools())
        codetools::findGlobals(def, merge = FALSE)$functions
    else
        unique(unlist(lapply(def, all.names)))
}

makeClassMethod <- function(def, name, Class, superClassMethod = "", allMethods) {
    depends <- .getGlobalFuns(def)
    ## find the field methods called ...
    depends <- depends[match(depends, allMethods, 0) > 0]
    new("refMethodDef", def, mayCall = depends, name = name,
        refClassName = Class, superClassMethod = superClassMethod)
}

refObjectClass <- function(object) {
    Class <- class(object)
    classDef <- getClassDef(Class)
    if(is(classDef, "refClassRepresentation"))
        classDef
    else
        stop(gettextf("\"%s\" is not a reference class", Class),
             domain = NA)
}

envRefSetField <- function(object, field,
                           thisClass = refObjectClass(object),
                           env = as.environment(object), value) {
    fieldClass <- thisClass@fieldClasses[[field]]
    if(is.null(fieldClass))
        stop(gettextf("\"%s\" is not a field in class \"%s\"",field, thisClass@className),
             domain = NA)
    else
        assign(field, value, envir = env)
    object
}

.initForEnvRefClass <- function(.Object, ...) {
    Class <- class(.Object)
    classDef <- getClass(Class)
    selfEnv <- new.env(TRUE, .NamespaceOrPackage(classDef@package))
    ## the parent environment will be used by field methods, to make
    ## them consistent with functions in this class's package
    .Object@.xData <- selfEnv
    ## install prototypes and active bindings
    prototypes <- classDef@fieldPrototypes
    fieldClasses <- classDef@fieldClasses
    fields <- names(fieldClasses)
    for(field in fields) {
        fp <- prototypes[[field]] # prototype or NULL
        if(is(fp, "activeBindingFunction")) {
            environment(fp) <- selfEnv
            makeActiveBinding(field, fp, selfEnv)
            if(is(fp, "defaultBindingFunction")) {
                ## ensure an initial value
                class <- fieldClasses[[field]]
                if(isVirtualClass(class))
                    value <- NULL
                else
                    value <- new(class)
                assign(.bindingMetaName(field), value, envir = selfEnv)
            }
        }
        else
            assign(field, fp, envir = selfEnv)
    }
    ## assign references to the object and to its class definition
    assign(".self", .Object, envir = selfEnv)
    assign(".refClassDef", classDef, envir = selfEnv)
    if(is.function(classDef@refMethods$finalize))
        reg.finalizer(selfEnv, function(x) x$.self$finalize())
    if(is.function(classDef@refMethods$initialize))
        .Object$initialize(...)
    else {
        if(nargs() > 1) {
            .Object <-
                methods::initFieldArgs(.Object, classDef, selfEnv, ...)
            ## reassign in case something changed
            assign(".self", .Object, envir = selfEnv)
        }
        .Object
    }
}

initFieldArgs <- function(.Object, classDef, selfEnv, ...) {
    args <- list(...)
    if(length(args)) {
        fieldDefs <- classDef@fieldClasses
        fieldNames <- names(fieldDefs)
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        elNames <- names(elements)
        supers <- args[!which]
        whichFields <- match(elNames, fieldNames, 0) > 0
        for(field in elNames[whichFields])
            envRefSetField(.Object, field, classDef, selfEnv, elements[[field]])
        other <- c(supers, elements[!whichFields])
        if(length(other))
            ## invoke the default method for superclasses & slots
            .Object <- do.call(methods:::.initialize,
                             c(list(.Object), other))
    }
    .Object
}

.dollarForEnvRefClass <- function(x, name) {
    what <- substitute(name)
    if(is.symbol(what))
        what <- as.character(what)
    else
        what <- name
    selfEnv <- as.environment(x)
    if(exists(what, envir = selfEnv, inherits = FALSE))
        ## either a field or previously cached method
        get(what, envir = selfEnv)
    else
        ## infer (usually) the method, cache it and return it
        envRefInferField(x, what, getClass(class(x)), selfEnv)
}

.dollarGetsForEnvRefClass <- function(x, name, value) {
    what <- substitute(name)
    if(is.symbol(what))
        what <- as.character(what)
    else
        what <- name
    selfEnv <- as.environment(x)
    envRefSetField(x, what, refObjectClass(x), selfEnv, value)
    invisible(x)
}

.envRefMethods <-
    list(
         export = function(Class) {
             '
Returns the result of coercing the object to
Class.  No effect on the object itself.
'
             if(match(.refClassDef@className, Class, 0) > 0)
                 return(.self)
             classDef <- getClass(Class)
             if(is(classDef, "refClassRepresentation") &&
                !is.na(match(Class, .refClassDef@refSuperClasses))) {
                 value <- new(classDef)
                 env <- as.environment(value)
                 selfEnv <- as.environment(.self)
                 fieldClasses <- classDef@fieldClasses
                 for(field in names(fieldClasses)) {
                     current <- get(field, envir = selfEnv)
                     if(!is(current, fieldClasses[[field]]))
                         stop(gettextf("The class of field \"%s\" in the object is not compatible with the desired class \"%s\" in the target",
                                       field, fieldClasses[[field]]),
                              domain = NA)
                     assign(field, envir = env, current)
                 }
                 value
             }
             else if(is(classDef, "classRepresentation")) # use standard S4 as()
                 methods::as(.self, Class)
             else if(is.character(Class) && length(Class) == 1)
                 stop(gettextf("\"%s\" is not a defined class in this environment",
                               Class), domain = NA)
             else
                 stop("Invalid Class argument:  should be a single string")
         },
         import =   function(value, Class = class(value)) {
             '
Imports value, replacing the part of the current object
corresponding to Class (if argument Class is missing
it is taken to be class(value)).  The Class must be one
of the reference superclasses of the current class (or
that class itself, but then you could just overrwite the object).
'
             if(!missing(Class))
                 value <- value$export(Class)
             classDef <- getClass(Class)
             if(is(classDef, "refClassRepresentation") &&
                !is.na(match(Class, .refClassDef@refSuperClasses))) {
                 env <- as.environment(value)
                 selfEnv <- as.environment(.self)
                 fieldClasses <- .refClassDef@fieldClasses
                 for(field in names(classDef@fieldClasses)) {
                     current <- get(field, envir = env)
                     if(!is(current, fieldClasses[[field]]))
                         stop(gettextf("The class of field \"%s\" in the object is not compatible with the desired class \"%s\" in the target",
                                       field, fieldClasses[[field]]),
                              domain = NA)
                     assign(field, envir = selfEnv, current)
                 }
                 invisible(.self)
             }
             else
                 stop(gettextf("\"%s\" is not one of the reference super classes for this object",
                               Class), domain = NA)
         },
         callSuper = function(...) stop("direct calls to callSuper() are invalid:  should only be called from another method"),
         initFields = function(...) {
             if(length(list(...))>0)
                 initFieldArgs(.self, .refClassDef, as.environment(.self), ...)
             else
                 .self
         },
         copy = function(shallow = FALSE) {
             def <- .refClassDef
             value <- new(def)
             vEnv <- as.environment(value)
             selfEnv <- as.environment(.self)
             for(field in names(def@fieldClasses)) {
                 if(shallow)
                     assign(field, get(field, envir = selfEnv), envir = vEnv)
                 else {
                     current <- get(field, envir = selfEnv)
                     if(is(current, "envRefClass"))
                         current <- current$copy(FALSE)
                     assign(field, current, envir = vEnv)
                 }
             }
             value
         },
         getRefClass = function(Class = .refClassDef) methods::getRefClass(Class),
         getClass = function(...) if(nargs()) methods::getClass(...) else .refClassDef,
         field = function(name, value) if(missing(value)) get(name, envir = .self) else {
             if(is.na(match(name, names(.refClassDef@fieldClasses))))
                 stop(gettextf("\"%s\" is not a field in this class", name),
                      domain = NA)
             assign(name, value, envir = .self)
         }
         )

## construct a list of class methods for envRefClass
makeEnvRefMethods <- function() {
    methods <- .envRefMethods
    allMethods <- names(methods)
    for(method in allMethods) {
        methods[[method]] <- makeClassMethod(methods[[method]],
                   method, "envRefClass", "", allMethods)
    }
    methods
}

## initialize some reference classes
.InitRefClasses <- function(envir)
{
    ## class to define a reference class
    ## Should be split into an abstract class and a standard version
    ## to use environments, so other variants might use interfaces
    ## to OOP languages, and proxy objects

    setClass("refClassRepresentation",
             representation(fieldClasses = "list",
                            fieldPrototypes = "environment",
                            refMethods = "environment",
                            refSuperClasses = "character"),
             contains = "classRepresentation", where = envir)
    ## the virtual class from which all true reference clases
    ## inherit.  Its subclasses require methods
    ## for getting & setting fields and related tasks
    setClassUnion("refClass", where = envir)
    ## the union of all reference objects
    ## (including those not belonging to refClass)
    setClassUnion("refObject", c("environment", "externalptr", "name",                                "refClass"), where = envir)
    ## a class for field methods, with a slot for their dependencies,
    ## allowing installation of all required instance methods
    setClassUnion("SuperClassMethod", "character")
    ## helper classes for active binding of fields
    setClass("activeBindingFunction", contains = "function")
    setClass("defaultBindingFunction",
             representation(field = "character", className = "character"),
             contains = "activeBindingFunction")
    ## class to mark uninitialized fields
    setClass("uninitializedField",
             representation(field = "character", className = "character"))
    setClass("refMethodDef",
             representation(mayCall = "character", name = "character",
                            refClassName = "character",
                            superClassMethod = "SuperClassMethod"),
             contains = "function", where = envir)
    setIs("refMethodDef", "SuperClassMethod", where = envir)
    setClass("envRefClass", contains = c("environment","refClass"), where =envir)
    ## bootstrap envRefClass as a refClass
    def <- new("refClassRepresentation",
               refMethods = as.environment(makeEnvRefMethods()))
    as(def, "classRepresentation") <- getClassDef("envRefClass", where = envir)
    assignClassDef("envRefClass", def, where = envir)
    setMethod("initialize", "envRefClass", methods:::.initForEnvRefClass,
              where = envir)
    ## NOTE:  "$" method requires setting in methods:::.InitStructureMethods
    setMethod("$", "envRefClass", .dollarForEnvRefClass, where = envir)
    setMethod("$<-", "envRefClass", .dollarGetsForEnvRefClass, where = envir)
    ## next call is touchy:  setRefClass() returns an object of class
    ## refObjectGenerator, but the class should have been defined before
    ## the return value is constructed.
    setRefClass("refObjectGenerator",
                fields = list(def = "ANY", className = "ANY"),
                methods = .GeneratorMethods)
    setMethod("show", "refClassRepresentation",
              function(object) showRefClassDef(object), where = envir)
    setMethod("show", "refObjectGenerator",
              function(object) showRefClassDef(object$def, "Generator object for class"),
                where = envir)
    setMethod("show", "refMethodDef", showClassMethod, where = envir)
}

getRefSuperClasses <- function(classes, classDefs) {
    supers <- character()
    for(i in seq_along(classes)) {
        clDef <- classDefs[[i]]
        supers <- c(supers, clDef@refSuperClasses)
    }
    unique(supers)
}

.GeneratorMethods <- list(methods =  function(...) {
    methodsEnv <- def@refMethods
    if(nargs() == 0)
        return(objects(methodsEnv, all.names = TRUE))
    methodDefs <- list(...)
    ## allow either name=function, ... or a single list
    if(length(methodDefs) == 1 && is.list(methodDefs[[1]]))
        methodDefs <- methodDefs[[1]]
    mnames <- names(methodDefs)
    if(is.null(mnames) || !all(nzchar(mnames)))
        stop("Arguments to methods() must be named, or one named list")
    ## look for methods to remove (new definition is NULL)
    removeThese <- sapply(methodDefs, is.null)
    if(any(removeThese)) {
        rmNames <- mnames[removeThese]
        mnames <- mnames[!removeThese]
        methodDefs <- methodDefs[!removeThese]
        remove(list = rmNames, envir = methodsEnv)
        if(length(mnames) == 0)
            return(invisible(methodsEnv))
    }
    allMethods <- as.list(methodsEnv)
    ## get a list of processed methods, plus any
    ## overriden superclass methods
    newMethods <- insertClassMethods(allMethods, className, methodDefs, names(def@fieldClasses), FALSE)
    for(what in names(newMethods))
        assign(what, newMethods[[what]], envir = methodsEnv)
    invisible(methodsEnv)
},

fields =  function() {
    '
Returns the named vector of classes
for the fields in this class.  Fields
defined with accessor functions have
class "activeBindingFunction".
'
    unlist(def@fieldClasses)
},
new =  function(...) {
    methods::new(def, ...)
},
  help =  function(topic) {
    '
Prints simple documentation for the method or field
specified by argument topic, which should be the name
of the method or field, quoted or not.  With no topic,
prints the definition of the class.
'
    if(missing(topic)) {
        writeLines(
c('Usage:  $help(topic) where topic is the name of a method (quoted or not)',
  paste('The definition of class', className, 'follows.')))
        show(def)
    }
    else {
        if(is.name(substitute(topic)))
            topic <- as.character(substitute(topic))
        else
            topic <- as.character(topic)
        env <- def@refMethods
        if(exists(topic, envir = env)) {
            f <- get(topic, envir = env)
            cat("Call:",.makeCall(topic, f), "\n")
            bb <- body(f)
            ## look for self-documentation
            if(is(bb, "{") && length(bb) > 1 && is(bb[[2]], "character"))
                writeLines(c(bb[[2]], ""))
        }
        else {
            cat(gettextf("Topic \"%s\" is not a method name in class \"%s\"\nThe class definition follows\n",
                         topic, className))
            show(def)
        }
    }
},
lock =  function(...) {
    fields <- c(...)
    if(is.character(fields) && all(nzchar(fields))) {}
    else
        stop("Arguments must all be character string names of fields")
    env <- def@fieldPrototypes
    className <- def@className
    for(what in fields) {
        current <- env[[what]]
        if(is.null(current))
            stop(gettextf("\"%s\" is not a field in class \"%s\"", what, className),
                 domain = NA)
        if(is(current, "activeBindingFunction")) {
            if(is(current, "defaultBindingFunction"))
                env[[what]] <- .makeDefaultBinding(current@field,
                    current@className, TRUE, environment(current))[[what]]
            else
                stop(gettextf("Field \"%s\"  of class \"%s\" has a non-default binding and cannot be locked",
                              what, className), domain = NA)
        }
        else {
            ## capture the current prototype value with a read-only binding function
            binding <- .makeDefaultBinding(current@field,
               current@className, TRUE, environment(current))
            env[[what]] <- binding[[what]]
            metaName <- .bindingMetaName(what)
            env[[metaName]] <- current
        }
    }
    invisible(env)
},
## define accessor functions, store them in the refMethods environment
## of the class definition.
accessors = function(...) {
    fieldNames <- c(...)
    methodNames <- firstCap(fieldNames)
    getters <- methodNames$get
    setters <- methodNames$set
    accessors <- list()
    for(i in seq_along(fieldNames)) {
        what <- fieldNames[[i]]
        field <- as.name(what)
        CLASS <- def@fieldClasses[[what]]
        if(is.null(CLASS))
            stop(gettextf("\"%s\" is not a field in class \"%s\"",
               what, def@className), domain = NA)
        accessors[[getters[[i]] ]] <-
                     eval(substitute(function() X, list(X = field)))
        if(CLASS == "ANY")
            accessors[[setters[[i]] ]] <-
                eval(substitute(function(value) {
                    value <- as(value, CLASS, strict = FALSE)
                    X <<- value
                    invisible(value)
                    },
                                list(X = field, CLASS = CLASS)))
        else
            accessors[[setters[[i]] ]] <-
                eval(substitute(function(value) {
                    X <<- value
                    invisible(value)
                    },
                                list(X = field)))
    }
    ## install the accessors
    methods(accessors)
    invisible(accessors)
}
)

.makeCall <- function(name, x) {
    paste0 <- function(...)paste(..., sep = "")
    n <- length(argls <- formals(x))
    noDeflt <- if(n > 0) sapply(argls,function(x)  !is.name(x) || nzchar(as.character(x)))
    if (n) {
        arg.names <- arg.n <- names(argls)
    }
    Call <- paste0("$", name, "(")
    for (i in seq_len(n)) {
        Call <- paste0(Call, arg.names[i], if (noDeflt[[i]]) " = "
            )
        if (i != n)
            Call <- paste0(Call, ", ")
    }
    Call <- paste0(Call, ")\n")
    Call
}


`insertFields<-` <- function(fieldList, value) {
    newNames <- names(value)
    ## check for valid overrides of existing field definitions
    hasFields <- match(newNames, names(fieldList),0) > 0
    if(any(hasFields)) {
        for(field in newNames[hasFields])
            ## the new field class must be a subclass of the old
            if(is.na(match(fieldList[[field]], extends(value[[field]]))))
                stop(gettextf("The overriding class(\"%s\") of field \"%s\" is not a subclass of the existing field definition (\"%s\")",
                      value[[field]], field, fieldList[[field]]),
                     domain = NA)
    }
    fieldList[newNames] <- value
    fieldList
}

.bindingMetaName <- function(fieldName)
    paste(".->", fieldName, sep="")

.makeActiveBinding <- function(thisField) {
    if(is(thisField, "activeBindingFunction"))
     thisField
    else
     new("activeBindingFunction", thisField)
}

.makeDefaultBinding <- function(fieldName, fieldClass, readOnly = FALSE, where) {
    metaName <- .bindingMetaName(fieldName)
    if(readOnly)
        ## write-once into the metaName object
        f <-  eval(substitute(function(value) {
            if(missing(value))
                dummyField
            else {
                selfEnv <- as.environment(.self)
                if(bindingIsLocked(metaName, selfEnv))
                    stop("Field \"", thisField, "\" is read-only")
                else {
                    dummyField <<- value
                    lockBinding(metaName, selfEnv)
                }
                value
            }
        }, list(dummyField = as.name(metaName), thisField = fieldName,
                metaName = metaName)))
    else
        f <- eval(substitute(function(value) {
            if(missing(value))
                dummyField
            else {
                value <- as(value, dummyClass)
                dummyField <<- value
                value
            }
        }, list(dummyField = as.name(metaName), dummyClass = fieldClass)))
    environment(f) <- where ## <note> Does this matter? </note>
    f <- new("defaultBindingFunction", f,
             field = fieldName, className = fieldClass)
    init <- (if(isVirtualClass(fieldClass))
                 new("uninitializedField", field = fieldName,
                     className = fieldClass)
        else new(fieldClass))
    value <- list(f, init)
    names(value) <- c(fieldName, metaName)
    value
}


refClassInformation <- function(Class, contains, fields, refMethods, where) {
    if(length(contains) > 0) {
        superClassDefs <- lapply(contains,
                                 function(what) {
                                     if(is(what, "classRepresentation"))
                                         what
                                     else if(is.character(what))
                                         getClass(what, where = where)
                                     else
                                         stop(gettextf(
                                                       "The contains= argument should be the names of superclasses:  got an element of class \"%s\"",
                                                       class(what)), domain = NA)
                                 })
        missingDefs <- sapply(superClassDefs, is.null)
        if(any(missingDefs))
            stop(gettextf("No definition found for inherited class: %s",
                          paste('"',contains[missingDefs], '"', sep = "", collapse = ", ")),
                 domain = NA)
        superClasses <- unlist(lapply(superClassDefs,
                          function(def) def@className), FALSE)
        isRefSuperClass <- sapply(superClassDefs, function(def)
                              is(def, "refClassRepresentation"))
    }
    else {
        superClassDefs <- list()
        superClasses <- character()
        isRefSuperClass <- logical()
    }
    if(!any(isRefSuperClass)) {
        superClasses <- c(superClasses, "envRefClass")
        isRefSuperClass <- c(isRefSuperClass, TRUE)
        superClassDefs[["envRefClass"]] <- getClass("envRefClass", where = where)
    }
    refSuperClasses <- superClasses[isRefSuperClass]
    otherRefClasses <- getRefSuperClasses(refSuperClasses, superClassDefs[isRefSuperClass])
    refSuperClasses <- unique(c(refSuperClasses, otherRefClasses))
    ## process the field definitions.  The call from setRefClass
    ## guarantees that fields is a named list.
    fieldNames <- names(fields)
    nf <- length(fields)
    fieldClasses <- character(nf)
    names(fieldClasses) <- fieldNames
    fieldPrototypes <- list()
    for(i in seq_len(nf)) {
        thisName <- fieldNames[[i]]
        thisField <- fields[[i]]
        ## a field definition can be:
        ## 1. character string name of the class
        ## 2. a binding function
        if(is.character(thisField)) {
            if(length(thisField) != 1)
                stop(gettextf("Single class name needed for field \"%s\", got a character vector of length %d", thisName, length(thisField)),
                     domain = NA)
            if(is.null(getClassDef(thisField, where = where)))
                stop(gettextf("Class \"%s\" for field \"%s\" is not defined", thisField, thisName),
                     domain = NA)
            fieldClasses[[i]] <- thisField
            if(thisField != "ANY")
                fieldPrototypes <- c(fieldPrototypes,
                    .makeDefaultBinding(thisName, thisField, where = where))
            else
                fieldPrototypes[[thisName]] <-
    new("uninitializedField", field = thisName,
                        className = "ANY")
        }
        else if(is.function(thisField)) {
            fieldClasses[[i]] <- "activeBindingFunction"
            fieldPrototypes[[thisName]] <-
                .makeActiveBinding(thisField)
        }
        else
            stop(gettextf("Field \"%s\" was supplied as an object of class \"%s\"; must be a class name or a binding function",
                thisName, class(thisField)
                ), domain = NA)
    }
    ## assemble inherited information
    fc <- fp <- cm <- list(); fr <- character()
    ## assign in reverse order so nearer superclass overrides
    for(cl in rev(superClassDefs[isRefSuperClass])) {
        fcl <- cl@fieldClasses
        fpl <- as.list(cl@fieldPrototypes, all.names = TRUE) # turn env into list
        cml <- as.list(cl@refMethods, all.names = TRUE) # ditto
        insertFields(fc) <- fcl
        fp[names(fpl)] <- fpl
        cm[names(cml)] <- cml
    }
    insertFields(fc) <- fieldClasses
    fp[names(fieldPrototypes)] <- fieldPrototypes

    ## process and insert reference methods
    cm <- insertClassMethods(cm, Class, refMethods, names(fc), TRUE)
    list(superClasses = superClasses, refSuperClasses = refSuperClasses,
         fieldClasses = fc, fieldPrototypes = fp,
         refMethods = cm)
}

superClassMethodName <- function(def)
    paste(def@name, def@refClassName, sep = "#")

insertClassMethods <- function(methods, Class, value, fieldNames, returnAll) {
    ## process reference methods, return either the entire updated methods
    ## or the processed new methods in value, plus superclass versions
    theseMethods <- names(value)
    prevMethods <- names(methods) # catch refs to inherited methods as well
    allMethods <- unique(c(theseMethods, prevMethods))
    if(returnAll)
        returnMethods <- methods
    else
        returnMethods <- value
    check <- TRUE
    for(method in theseMethods) {
        prevMethod <- methods[[method]] # NULL or superClass method
        if(is.null(prevMethod)) {
            ## kludge because default version of $initialize() breaks bootstrapping of methods package
            if(identical(method, "initialize"))
                superClassMethod <- "initFields"
            else
                superClassMethod <- ""
        }
        else if(identical(prevMethod@refClassName, Class))
            superClassMethod <- prevMethod@superClassMethod
        else {
            superClassMethod <- superClassMethodName(prevMethod)
            returnMethods[[superClassMethod]] <- prevMethod
        }
        def <- makeClassMethod(value[[method]], method, Class,
                               superClassMethod, allMethods)
        check <- check && .checkFieldsInMethod(def, fieldNames, allMethods)
        returnMethods[[method]] <- def
    }
    if(is.na(check) && .methodsIsLoaded())
        message(gettextf("Code for methods in class \"%s\" was not checked for suspicious field assignments (recommended package \"codetools\" not available?)",
                         Class), domain = NA)
    returnMethods
}


## refField <- function(class = "ANY", get = .stdGetField, set = .stdSetField, binding = NULL,
##                      name = "", where = topenv(parent.frame())) {
##     if(identical(set, FALSE))
##         set <- .invalidSetField
##     new("refFieldDefinition",  fieldName = name, fieldClass = class,
##         get = get, set = set, binding = binding)
##   }

setRefClass <- function(Class, fields = character(),
                        contains = character(),
                        methods = list(),
                        where = topenv(parent.frame()),
                        ...) {
    ## process the field definitions
    if(is.character(fields)) {
        fieldNames <- fields
        ## treat as "ANY"
        fields <- as.list(rep("ANY", length(fields)))
        names(fields) <- fieldNames
    }
    else if(is.list(fields)) {
        fieldNames <- names(fields)
        if(is.null(fieldNames) ||
           !all(nzchar(fieldNames)))
            stop("A list argument for fields must have nonempty names for all the fields")
    }
    else
        stop(gettextf("Argument fields must be a list of the field classes or definitions, or else just the names of the fields; got an object of class \"%s\"",
                      class(fields)), domain = NA)
    theseMethods <- names(methods) # non-inherited, for processing later
    ## collect the method and field definitions
    info <- refClassInformation(Class, contains, fields, methods, where)
    ## make codetools happy:
    superClasses <- refSuperClasses <- fieldClasses <- fieldPrototypes <-
        refMethods <- NULL
    ## think Python's multiple assignment operator
    for(what in c("superClasses", "refSuperClasses", "fieldClasses",
                  "fieldPrototypes", "refMethods"))
        assign(what, info[[what]])
    ## temporarily assign an ordinary class definition
    ## to allow the checks and defaults from setClass to be applied
    setClass(Class, contains = superClasses,
             where = where, ...)
    ## kludge: as.environment fails on an empty list
    asEnv <- function(x) {
        if(length(x)) as.environment(x) else new.env(FALSE)
    }
    ## now, override that with the complete definition
    classDef <- new("refClassRepresentation",
                    getClassDef(Class, where = where),
                    fieldClasses = fieldClasses,
                    refMethods = asEnv(refMethods),
                    fieldPrototypes = asEnv(fieldPrototypes),
                    refSuperClasses = refSuperClasses)
    assignClassDef(Class, classDef, where)
    value <- new("refObjectGenerator")
    env <- as.environment(value)
    env$def <- classDef
    env$className <- Class
    value
}

getRefClass <- function(Class, where = topenv(parent.frame())) {
    if(is(Class, "envRefClass"))
        classDef <- get(".refClassDef", envir = Class)
    else
        classDef <- getClass(Class, where = where)
    if(!is(classDef, "refClassRepresentation"))
        stop(gettextf("Class \"%s\" is defined but is not a reference class",
                      Class), domain = NA)
    value <- new("refObjectGenerator")
    env <- as.environment(value)
    env$def <- classDef
    env$className <- Class
    value
}

refClassFields <- function(Class) {
    ClassDef <- getClass(Class)
    if(is(ClassDef, "refClassRepresentation"))
        ClassDef@fieldClasses
    else
        stop("Not a reference class:", ClassDef@name)
}

refClassMethods <- function(Class) {
    ClassDef <- getClass(Class)
    if(is(ClassDef, "refClassRepresentation"))
        value <- as.list(ClassDef@refMethods)
    else
        stop("Not a reference class:", ClassDef@name)
    ## possibly temporary:  return methods to pure functions
    for(i in seq_along(value))
        value[[i]] <- as(value[[i]], "function")
    value
}

showClassMethod <- function(object) {
    cl <- class(object)
    cat("Class method definition")
    if(!.identC(cl, "refMethodDef"))
        cat(gettextf(" (class \"%s\")", cl))
    cat(gettextf(" for method %s()\n", object@name))
    show(as(object, "function"))
    if(length(object@mayCall))
        .printNames("Methods used: ", object@mayCall)
}

.printNames <- function(header, names) {
        cat("\n",header,"\n    ")
        cat(paste('"', names, '"', sep = ""), sep = ", ", fill = TRUE)
        cat("\n")
    }

showRefClassDef <- function(object, title = "Reference Class") {
    cat(title," \"", object@className,"\":\n", sep="")
    fields <- object@fieldClasses
    if(length(fields))
        printPropertiesList(fields, "Class fields")
    else
        cat("\nNo fields defined\n")
    methods <- objects(object@refMethods, all.names = TRUE)
    if(length(methods))
        .printNames("Class Methods: ", methods)
    else
        cat ("\nNo Class Methods\n")
    supers <- object@refSuperClasses
    if(length(supers))
        .printNames("Reference Superclasses: ", supers)
}


firstCap <- function(names) {
    firstChars <- substr(names, 1,1)
    modChars <- toupper(firstChars)
    substr(names, 1, 1) <- modChars
    list(get = paste("get", names, sep = ""), set = paste("set", names, sep = ""))
}


## all.equal and identical both screw up on environments
## but a bigger change is needed to all.equal than the following
## because it also screws up on, e.g., externalptr objects
if(FALSE) {
all.equal.environment <- function(target, current, ...) {
    nt <- sort(objects(target, all.names = TRUE))
    nc <- sort(objects(current, all.names = TRUE))
    tmp <- all.equal(nt, nc, ...)
    if(!identical(tmp, TRUE))
        return(paste("Different objects in target, current:", tmp))
    if(length(nt) == 0)
        return(TRUE)
    differ <- sapply(nt, function(what) {
        tmp <- all.equal(get(what, envir = target),
                         get(what, envir = current), ...)
        if(identical(tmp, TRUE)) FALSE
        else TRUE
    })
    if(any(differ))
        paste("Objects differ: ", paste(nt[differ], collapse = ", "))
    else
        TRUE
}
}

.assignExpr <- function(e) {
    value <- list()
    value[[codetools::getAssignedVar(e)]] <- deparse(e, nlines = 1L)
    value
}

.mergeAssigns <- function(previous, new) {
    for(what in names(new)) {
        if(is.null(previous[[what]]))
            previous[[what]] <- new[[what]]
        else
            previous[[what]] <- paste(previous[[what]], new[[what]], sep="; ")
    }
    previous
}


.assignedVars <- function(e) {
    locals <- list()
    globals <- list()
    walker <- codetools::makeCodeWalker(call = function(e, w) {
        callto <- e[[1]]
        if(is.symbol(callto)) switch(as.character(callto),
               "<-" = , "=" = {
                   locals <<- .mergeAssigns(locals, .assignExpr(e))
               },
               "<<-" = {
                   globals <<- .mergeAssigns(globals, .assignExpr(e))
               })
        for (ee in as.list(e))
            if (! missing(ee)) codetools::walkCode(ee, w)
    },
    leaf = function(e, w) NULL
    )
    codetools::walkCode(e, walker)
    list(locals = locals, globals = globals)
}

.checkFieldsInMethod <- function(methodDef, fieldNames, methodNames) {
    if(!.hasCodeTools())
        return(NA)
    if(length(fieldNames) == 0)
        return(TRUE)
    paste0 <- function(x) paste('"', x, '"', sep = "", collapse = "; ")
    if(is(methodDef, "refMethodDef")) {
        methodName <- paste0(methodDef@name)
        className <- paste0(methodDef@refClassName)
    }
    else {
        methodName <- className <- ""
    }
    assigned <- .assignedVars(body(methodDef))
    locals <- names(assigned$locals)
    localsAreFields <- match(locals, fieldNames, 0) > 0
    if(any(localsAreFields))
        warning(gettextf("Local assignment to field name will not change the field:\n    %s\n Did you mean to use \"<<-\"? ( in method %s for class %s)",
                paste(unlist(assigned$locals)[localsAreFields], collapse="; "), methodName, className),
                domain = NA)
    globals <- names(assigned$globals)
    globalsNotFields <- is.na(match(globals, fieldNames))
    if(any(globalsNotFields))
        warning(gettextf("Non-local assignment to non-field names (possibly misspelled?)\n    %s\n( in method %s for class %s)",
                paste(unlist(assigned$globals)[globalsNotFields], collapse="; "), methodName, className),
                domain = NA)
    globalsInMethods <- match(globals, methodNames, 0) > 0
    if(any(globalsInMethods))
        stop(gettextf("Non-local assignment to method names is not allowed\n    %s\n( in method %s for class %s)",
                paste(unlist(assigned$globals)[globalsInMethods], collapse="; "), methodName, className),
                domain = NA)
    !any(localsAreFields) && !any(globalsNotFields)
}
