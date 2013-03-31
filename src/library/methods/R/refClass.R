#  File src/library/methods/R/refClass.R
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
            stop(gettextf("%s is not a valid field or method name for reference class %s",
                          sQuote(field),
                          dQuote(thisClass@className)),
                 domain = NA)
    }
    value
}

installClassMethod <- function(def, self, me, selfEnv, thisClass) {
    if(!is(def, "refMethodDef")) {  #should not happen? => need warning
        warning(sprintf("method %s from class %s was not processed into a class method until being installed.  Possible corruption of the methods in the class.",
                         me, thisClass@className),
                domain = NA)
        def <- makeClassMethod(def, me, thisClass@className, "", objects(thisClass@refMethods, all.names = TRUE))
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
            stop(gettextf("a call to superClass() is in the method %s but there is no superclass definition of this method for class %s",
                          sQuote(me),
                          dQuote(thisClass@className)),
                 domain = NA)
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
    if("usingMethods" %in% depends) { # including those declared
        declared <- .declaredMethods(def)
        ## look for invalid declared methods
        if(length(declared) && any(! declared %in% allMethods))
            warning(gettextf("methods declared in usingMethods() but not found: %s",
                paste0(declared[! declared %in% allMethods], collapse = ", ")))
        depends <- c(declared, depends)
    }
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
        stop(gettextf("%s is not a reference class",
                      dQuote(Class)),
             domain = NA)
}

envRefSetField <- function(object, field,
                           thisClass = refObjectClass(object),
                           env = as.environment(object), value) {
    fieldClass <- thisClass@fieldClasses[[field]]
    if(is.null(fieldClass))
        stop(gettextf("%s is not a field in class %s",
                      sQuote(field),
                      dQuote(thisClass@className)),
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
    selfEnv$.self <- .Object
    selfEnv$.refClassDef <- classDef
    if(is.function(classDef@refMethods$initialize)) {
        .Object$initialize(...)
        ## intialize methods are allowed to change .self
        .Object <- selfEnv$.self
    }
    else {
        if(nargs() > 1) {
            .Object <-
                methods::initRefFields(.Object, classDef, selfEnv, list(...))
        }
    }
    if(is.function(classDef@refMethods$finalize))
        reg.finalizer(selfEnv, function(x) x$.self$finalize())
    lockBinding(".self", selfEnv)
    lockBinding(".refClassDef", selfEnv)
    .Object
}

## old version, for back compatibility.  Could be deleted after 2.15.0
initFieldArgs <- function(.Object, classDef, selfEnv, ...)
    initRefFields(.Object, classDef, selfEnv, list(...))

initRefFields <- function(.Object, classDef, selfEnv, args) {
    if(length(args)) {
        fieldDefs <- classDef@fieldClasses
        fieldNames <- names(fieldDefs)
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        supers <- args[!which]
        elNames <- names(elements)
        for(super in supers) {
            if(!is(super, "refClass")) {
                warning(gettextf("unnamed arguments to $new() must be objects from a reference class; got an object of class %s",
                                 dQuote(class(super))),
                        domain = NA)
                next
            }
            fields <- names(super$.refClassDef@fieldClasses)
            ##<FIXME> need an object$fields for the above </FIXME>
            ## assign field if it is not already specified
            fields <- fields[is.na(match(fields, elNames))]
            for(field in fields)
                elements[[field]] <- super$field(field)
            elNames <- names(elements)
        }
        ## assign the fields
        for(field in elNames)
            envRefSetField(.Object, field, classDef, selfEnv, elements[[field]])
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
    else if(is(x, "envRefClass"))
        ## infer (usually) the method, cache it and return it
        envRefInferField(x, what, getClass(class(x)), selfEnv)
    else # don't know the reference class(e.g., x is the refMethods env.)
        stop(gettextf("%s is not a valid field or method name for this class",
                      sQuote(what)),
             domain = NA)
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
                         stop(gettextf("the class of field %s in the object is not compatible with the desired class %s in the target",
                                       sQuote(field),
                                       dQuote(fieldClasses[[field]])),
                              domain = NA)
                     assign(field, envir = env, current)
                 }
                 value
             }
             else if(is(classDef, "classRepresentation")) # use standard S4 as()
                 methods::as(.self, Class)
             else if(is.character(Class) && length(Class) == 1)
                 stop(gettextf("%s is not a defined class in this environment",
                               dQuote(Class)),
                      domain = NA)
             else
                 stop("invalid 'Class' argument:  should be a single string")
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
                (!is.na(match(Class, .refClassDef@refSuperClasses))
                || identical(classDef@className, .refClassDef@className))) {
                 env <- as.environment(value)
                 selfEnv <- as.environment(.self)
                 fieldClasses <- .refClassDef@fieldClasses
                 for(field in names(classDef@fieldClasses)) {
                     current <- get(field, envir = env)
                     if(!is(current, fieldClasses[[field]]))
                         stop(gettextf("the class of field %s in the object is not compatible with the desired class %s in the target",
                                       sQuote(field),
                                       dQuote(fieldClasses[[field]])),
                              domain = NA)
                     assign(field, envir = selfEnv, current)
                 }
                 invisible(.self)
             }
             else
                 stop(gettextf("%s is not one of the reference super classes for this object",
                               dQuote(Class)),
                      domain = NA)
         },
         callSuper = function(...) stop("direct calls to callSuper() are invalid:  should only be called from another method"),
         initFields = function(...) {
             if(missing(...)) .self else
             initRefFields(.self, .refClassDef, as.environment(.self), list(...))
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
                 stop(gettextf("%s is not a field in this class",
                               sQuote(name)),
                      domain = NA)
             assign(name, value, envir = .self)
         },
         trace = function(..., classMethod = FALSE) {
             ' Insert trace debugging for the specified method.  The arguments are
 the same as for the trace() function in package "base".  The first argument
 should be the name of the method to be traced, quoted or not.

 The additional argument classMethod= can be supplied as TRUE (by name only)
 in order to trace a method in a generator object (e.g., "new") rather than
 in the objects generated from that class.
'
             .TraceWithMethods(..., where = .self, classMethod = classMethod)
         },
         untrace = function(..., classMethod = FALSE) {
             ' Untrace the method given as the first argument.
'
             .TraceWithMethods(..., untrace = TRUE,  where = .self, classMethod = classMethod)
         },
         show = function() {
             cat('Reference class object of class ', classLabel(class(.self)),
        '\n', sep = "")
             fields <- names(.refClassDef@fieldClasses)
             for(fi in fields) {
                 cat('Field "', fi, '":\n', sep = "")
                 methods::show(field(fi))
             }
         },
         usingMethods = function(...) {
             ' Reference methods used by this method are named as the arguments
 either quoted or unquoted.  In the code analysis phase of installing the
 the present method, the declared methods will be included.  It is essntial
 to declare any methods used in a nonstandard way (e.g., via an apply function).
 Methods called directly do not need to be declared, but it is harmless to do so.
 $usingMethods() does nothing at run time.
'
             NULL
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
    ## and make a traceable version of the class
    .makeTraceClass(.traceClassName("refMethodDef"), "refMethodDef", FALSE)
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
    setMethod("show", "envRefClass", function(object) object$show())
    setClass("refGeneratorSlot") # a temporary virtual class to allow the next definition
    ## the refClassGenerator class
    setClass("refObjectGenerator", representation(generator ="refGeneratorSlot"),
             contains = c("classGeneratorFunction", "refClass"), where = envir)

    setMethod("$", "refObjectGenerator",
              function(x, name) eval.parent(substitute(x@generator$name)), where = envir)

    setMethod("$<-", "refObjectGenerator",
              function(x, name, value) eval.parent(substitute(x@generator$name <- value)),
              where = envir)
    ## next call is touchy:  setRefClass() uses an object of class
    ## refGeneratorSlot, but the class should have been defined before
    ## that object is created.
    setRefClass("refGeneratorSlot",
                fields = list(def = "ANY", className = "ANY"),
                methods = .GeneratorMethods, where = envir)
    setMethod("show", "refClassRepresentation",
              function(object) showRefClassDef(object), where = envir)
    setMethod("show", "refObjectGenerator",
              function(object) showRefClassDef(object$def, "Generator for class"),
              where = envir)
    setMethod("show", "refMethodDef", showClassMethod, where = envir)
    ## Now do "localRefClass"; doesn't need to be precisely here
    ## but this ensures it is not done too early or too late
    setRefClass("localRefClass", methods = .localRefMethods,
                where = envir)  # should this have contains = "VIRTUAL"?

    setMethod("$<-", "localRefClass",
              function(x, name, value) {
                  w <- parent.frame()
                  x <- .ensureLocal(x, w)
                  what <- substitute(name)
                  if (is.symbol(what))
                      what <- as.character(what)
                  else what <- name
                  selfEnv <- as.environment(x)
                  envRefSetField(x, what, refObjectClass(x), selfEnv, value)
                  invisible(x)
              } , where = envir)
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
    if(methods:::.classDefIsLocked(def))
        stop(gettextf("the definition of class %s in package %s is locked, methods may not be redefined",
                      dQuote(def@className),
                      sQuote(def@package)),
             domain = NA)
    methodDefs <- list(...)
    ## allow either name=function, ... or a single list
    if(length(methodDefs) == 1 && is.list(methodDefs[[1]]))
        methodDefs <- methodDefs[[1]]
    mnames <- names(methodDefs)
    if(is.null(mnames) || !all(nzchar(mnames)))
        stop("arguments to methods() must be named, or one named list")
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
    ## calls to $methods() only work in package source or
    ## as load actions.  Use the topenv() if that seems like
    ## the namespace in preparation, or the namespace if available
    env <- topenv(parent.frame()); declare <- TRUE
    if(exists(".packageName", envir = env) &&
       get(".packageName", envir = env) == def@package) {}
    else if(def@package %in% loadedNamespaces())
        env <- asNamespace(def@package)
    else
        declare <- FALSE
    if(declare)
        utils::globalVariables(names(newMethods), env)
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
        methods::show(def)
    }
    else {
        if(is.name(substitute(topic)))
            topic <- as.character(substitute(topic))
        else
            topic <- as.character(topic)
        env <- def@refMethods
        if(exists(topic, envir = env)) {
            writeLines(.refMethodDoc(topic, env))
        }
        else {
            cat(gettextf("topic %s is not a method name in class %s\nThe class definition follows\n",
                         sQuote(topic),
                         dQuote(className)))
            show(def)
        }
    }
},
lock =  function(...) methods:::.lockRefFields(def, ...),
## define accessor functions, store them in the refMethods environment
## of the class definition.
accessors = function(...) {
    firstCap <- function(names) {
        firstChars <- substr(names, 1,1)
        modChars <- toupper(firstChars)
        substr(names, 1, 1) <- modChars
        list(get = paste0("get", names), set = paste0("set", names))
    }
    if(methods:::.classDefIsLocked(def))
        stop(gettextf("the definition of class %s in package %s is locked so fields may not be modified",
                      dQuote(def@className),
                      sQuote(def@package)),
             domain = NA)
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
            stop(gettextf("%s is not a field in class %s",
                          sQuote(what),
                          dQuote(def@className)),
                 domain = NA)
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

.localRefMethods <-
    list(
         ensureLocal = function() {
             'Ensure that a shallow copy has been made of this object
to localize any further changes.  Must be called before any reference
class method modifies a field.
'
             methods:::.ensureLocal(.self, parent.frame())
         }
     )

.makeCall <- function(name, x) {
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
            if(is.na(match(fieldList[[field]], c(extends(value[[field]]),"ANY"))))
                stop(gettextf("the overriding class (\"%s\") of field %s is not a subclass of the existing field definition (\"%s\")",
                              value[[field]],
                              sQuote(field),
                              fieldList[[field]]),
                     domain = NA)
    }
    fieldList[newNames] <- value
    fieldList
}

.bindingMetaName <- function(fieldName)
    paste0(".->", fieldName)

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
                dummyFieldName
            else {
                methods:::.setDummyField(.self, dummyField, dummyClass, thisField, TRUE, value)
                value
            }
        }, list(dummyField = metaName, thisField = fieldName,
                dummyClass = fieldClass, dummyFieldName = as.name(metaName))))
    else
        f <- eval(substitute(function(value) {
            if(missing(value))
                dummyFieldName
            else {
                methods:::.setDummyField(.self, dummyField, dummyClass, thisField, FALSE, value)
                value
            }
        }, list(dummyField = metaName, dummyClass = fieldClass,
                thisField = fieldName, dummyFieldName = as.name(metaName))))
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

.setDummyField <- function(self, metaName, fieldClass, fieldName, onceOnly, value) {
    if(is(value, fieldClass))
        value <- as(value, fieldClass, strict = FALSE) # could be more efficient?
    else
        stop(gettextf("invalid assignment for reference class field %s, should be from class %s or a subclass (was class %s)",
                       sQuote(fieldName), dQuote(fieldClass), dQuote(class(value))), call. = FALSE)
    selfEnv <- as.environment(self)
    if(onceOnly) {
        if(bindingIsLocked(metaName, selfEnv))
            stop(gettextf("invalid replacement: reference class field %s is read-only", sQuote(fieldName)),
                 call. = FALSE)
        else {
            assign(metaName, value, envir = selfEnv)
            lockBinding(metaName, selfEnv)
        }
    }
    else
       assign(metaName, value, envir = selfEnv)
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
                                         stop(gettextf("the 'contains' argument should be the names of superclasses:  got an element of class %s",
                                                       dQuote(class(what))),
                                              domain = NA)
                                 })
        missingDefs <- sapply(superClassDefs, is.null)
        if(any(missingDefs))
            stop(gettextf("no definition found for inherited class: %s",
                          paste0('"',contains[missingDefs], '"', collapse = ", ")),
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
                stop(gettextf("a single class name is needed for field %s, got a character vector of length %d",
                              sQuote(thisName),
                              length(thisField)),
                     domain = NA)
            if(is.null(getClassDef(thisField, where = where)))
                stop(gettextf("class %s for field %s is not defined",
                              dQuote(thisField),
                              sQuote(thisName)),
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
            stop(gettextf("field %s was supplied as an object of class %s; must be a class name or a binding function",
                          sQuote(thisName),
                          dQuote(class(thisField))),
                 domain = NA)
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
        message(gettextf("code for methods in class %s was not checked for suspicious field assignments (recommended package %s not available?)",
                         dQuote(Class),
                         sQuote("codetools"))
                , domain = NA)
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
    fields <- inferProperties(fields, "field")
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
    ## and to get the classGeneratorFunction
    ## Note:  the classGeneratorFunction has the class name, not the explicit definition
    classFun <- setClass(Class, contains = superClasses,
             where = where, ...)
    ## kludge: as.environment fails on an empty list
    asEnv <- function(x) {
        if(length(x)) as.environment(x) else new.env(FALSE)
    }
    ## now, override the class definiton with the complete definition
    classDef <- new("refClassRepresentation",
                    getClassDef(Class, where = where),
                    fieldClasses = fieldClasses,
                    refMethods = asEnv(refMethods),
                    fieldPrototypes = asEnv(fieldPrototypes),
                    refSuperClasses = refSuperClasses)
    assignClassDef(Class, classDef, where)
    generator <- new("refGeneratorSlot")
    env <- as.environment(generator)
    env$def <- classDef
    env$className <- Class
    .declareVariables(classDef, where)
    value <- new("refObjectGenerator", classFun, generator = generator)
    invisible(value)
}

getRefClass <- function(Class, where = topenv(parent.frame())) {
    if(is(Class, "refClassRepresentation")) {
        classDef <- Class
        Class <- classDef@className
    }
    else if(is.character(Class)) {
        classDef <- getClass(Class, where = where)
        if(!is(classDef, "refClassRepresentation"))
            stop(gettextf("class %s is defined but is not a reference class",
                          dQuote(Class)),
                 domain = NA)
    }
    else
        stop(gettextf("class must be a reference class representation or a character string; got an object of class %s",
                      dQuote(class(Class))),
             domain = NA)
    generator <- new("refGeneratorSlot")
    env <- as.environment(generator)
    env$className <- Class
    env$def <- classDef
    classFun <- classGeneratorFunction(Class, where)
    ## but, the package is always from the class definition, not the local environment
    classFun@package <- classDef@package
    new("refObjectGenerator", classFun, generator = generator)
}

refClassFields <- function(Class) {
    ClassDef <- getClass(Class)
    if(is(ClassDef, "refClassRepresentation"))
        ClassDef@fieldClasses
    else
        stop(gettextf("not a reference class: %s", ClassDef@name),
             domain = NA)
}

refClassMethods <- function(Class) {
    ClassDef <- getClass(Class)
    if(is(ClassDef, "refClassRepresentation"))
        value <- as.list(ClassDef@refMethods)
    else
        stop(gettextf("not a reference class: %s", ClassDef@name),
             domain = NA)
    ## possibly temporary:  return methods to pure functions
    for(i in seq_along(value))
        value[[i]] <- as(value[[i]], "function")
    value
}

showClassMethod <- function(object) {
    cl <- class(object)
    cat("Class method definition")
    if(!.identC(cl, "refMethodDef"))
        cat(sprintf(" (class %s)", dQuote(cl)))
    cat(sprintf(" for method %s()\n", object@name))
    show(as(object, "function"))
    if(length(object@mayCall))
        .printNames("Methods used: ", object@mayCall)
}

.printNames <- function(header, names, separateLine = TRUE) {
    if(separateLine)
        cat("\n",header,"\n    ")
    else
        cat(header,": ",sep="")
    cat(paste0('"', names, '"'), sep = ", ", fill = TRUE)
    cat("\n")
    }

showRefClassDef <- function(object, title = "Reference Class") {
    cat(title," \"", object@className,"\":\n", sep="")
    fields <- object@fieldClasses
    if(length(fields)) {
        printPropertiesList(fields, "Class fields")
        locked <- .getLockedFieldNames(object)
        if(length(locked))
            .printNames("Locked Fields", locked, FALSE)
    }
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
    p0q <- function(x) paste0('"', x, '"', collapse = "; ")
    if(is(methodDef, "refMethodDef")) {
        methodName <- p0q(methodDef@name)
        className <- p0q(methodDef@refClassName)
    }
    else {
        methodName <- className <- ""
    }
    assigned <- .assignedVars(body(methodDef))
    locals <- names(assigned$locals)
    localsAreFields <- match(locals, fieldNames, 0) > 0
    if(any(localsAreFields))
        warning(gettextf("local assignment to field name will not change the field:\n    %s\n Did you mean to use \"<<-\"? ( in method %s for class %s)",
                paste(unlist(assigned$locals)[localsAreFields], collapse="; "), methodName, className),
                domain = NA)
    globals <- names(assigned$globals)
    ## check non-fields, but allow to .self (will be an
    ## error except in $initialize())
    globalsNotFields <- is.na(match(globals, c(fieldNames, ".self")))
    if(any(globalsNotFields))
        warning(gettextf("non-local assignment to non-field names (possibly misspelled?)\n    %s\n( in method %s for class %s)",
                paste(unlist(assigned$globals)[globalsNotFields], collapse="; "), methodName, className),
                domain = NA)
    globalsInMethods <- match(globals, methodNames, 0) > 0
    if(any(globalsInMethods))
        stop(gettextf("non-local assignment to method names is not allowed\n    %s\n( in method %s for class %s)",
                paste(unlist(assigned$globals)[globalsInMethods], collapse="; "), methodName, className),
                domain = NA)
    !any(localsAreFields) && !any(globalsNotFields)
}

.refMethodDoc <- function(topic, env) {
    f <- get(topic, envir = env)
    msg <- c("Call:",.makeCall(topic, f), "")
    bb <- body(f)
    ## look for self-documentation
    if(is(bb, "{") && length(bb) > 1 && is(bb[[2]], "character"))
        msg <- c(msg, bb[[2]], "")
    msg
}

## the locked fields are stored as a hidden object in the fieldPrototypes environment
## but this might change, so the .get, .set functions should be used
.lockedFieldsMetaName <- ".#lockedFields"
.getLockedFieldNames <- function(def) {
    env <- def@fieldPrototypes
    value <- env[[.lockedFieldsMetaName]]
    if(is.null(value))
        character()
    else
        value
}
.setLockedFieldNames <- function(def, value) {
    env <- def@fieldPrototypes
    env[[.lockedFieldsMetaName]] <- value
    value
}

.lockRefFields <- function(def, ...) {
    lockedFields <- .getLockedFieldNames(def)
    if(nargs()<2)
        return(lockedFields)
    fields <- c(...)
    if(is.character(fields) && all(nzchar(fields))) {}
    else
        stop("arguments must all be character string names of fields")
    if(.classDefIsLocked(def))
        stop(gettextf("the definition of class %s in package %s is locked so fields may not be modified",
                      dQuote(def@className),
                      sQuote(def@package)),
             domain = NA)
    env <- def@fieldPrototypes
    className <- def@className
    for(what in fields) {
        if(what %in% lockedFields) {
            warning(gettextf("field %s is already locked", sQuote(what)),
                    domain = NA)
            next
        }
        current <- env[[what]]
        if(is.null(current))
            stop(gettextf("%s is not a field in class %s",
                          sQuote(what),
                          dQuote(className)),
                 domain = NA)
        if(is(current, "activeBindingFunction")) {
            if(is(current, "defaultBindingFunction"))
                env[[what]] <- .makeDefaultBinding(current@field,
                    current@className, TRUE, environment(current))[[what]]
            else
                stop(gettextf("field %s of class %s has a non-default binding and cannot be locked",
                              sQuote(what),
                              dQuote(className)),
                     domain = NA)
        }
        else {
            ## capture the current prototype value with a read-only binding function
            binding <- .makeDefaultBinding(current@field,
               current@className, TRUE, environment(current))
            env[[what]] <- binding[[what]]
            metaName <- .bindingMetaName(what)
            env[[metaName]] <- current
        }
        lockedFields <- c(lockedFields, what)
    }
    .setLockedFieldNames(def, lockedFields)
    invisible(env)
}

## declare field and method names global to avoid spurious
## messages from codetools
.declareVariables <- function(def, env) {
    utils::globalVariables(c(names(def@fieldClasses), objects(def@refMethods)),
                           env)
}

.declaredMethods <- function(method) {
    methods <- character()
    if(!.hasCodeTools())
        return(methods)
    .theseMethods <- function(e, w) {
        if(length(e) < 2) character()
        else
            sapply(as.list(e)[-1], function(what)
                   methods <<- c(methods, if(is.symbol(what)) as.character(what) else if(is.character(what)) what else character()))
    }
    walker <- codetools::makeCodeWalker(
                handler = function(v, w) {
                    if(identical(v, "usingMethods"))
                        .theseMethods
                    else
                        NULL
                },
                leaf = function(e, w) NULL)
    codetools::walkCode(body(method), walker)
    unique(methods)
}

getMethodsAndAccessors <- function(Class) {
    def <- getClass(Class)
    if(!is(def, "refClassRepresentation"))
        stop(gettextf("%s is not a reference class",
             dQuote(def@className)))
    ff <- def@fieldPrototypes
    accs <- sapply(ff, function(what) is(what, "activeBindingFunction") && !is(what, "defaultBindingFunction"))
    c(as.list(def@refMethods), as.list(ff)[accs])
}

## Reference classes that guarantee to change fields only in the
## local environment.  The method for `$<-` checks that the lhs object
## has been registered in a list of local reference class objects in
## the frame where the call is evaluated.  If not, a shallow copy
## of the object's .self (environment) is made, replaces the variable
## and is registered.  The effect should be that locality of assignment
## is preserved wtihout the deep copy generated by the R evaluator
## for complex assignments that are not primitives, e.g., `@<-`

.ensureLocal <- function(object, where) {
    if(!is(object, "envRefClass"))
        stop(gettextf("Class %s is not a subclass of %s; functional semantics not defined for this class", dQuote(class(object)), dQuote("envRefClass")))
    selfEnv <- as.environment(object)
    if(exists(".localRefObjects", envir = where, inherits = FALSE)) {
        locals <- get(".localRefObjects", envir = where)
        for(i in rev(seq_along(locals)))
            if(identical(selfEnv, locals[[i]]))
                return(object)
    }
    else
        locals <- list()
    ## the object should be assigned in environment where=
    what <- NULL
    for(objName in objects(envir = where, all.names = TRUE)) {
        obj <- get(objName, envir = where)
        if(is(obj, "envRefClass") && identical(selfEnv, as.environment(obj))) {
            what <- obj
            break
        }
    }
    if(is.null(what))
        stop("Could not find local object in supplied environment")
    ## do a shallow copy and record it as local
    value <- .shallowCopy(object, selfEnv)
    locals[[length(locals)+1]] <- as.environment(value)
    assign(".localRefObjects", locals, envir = where)
    value
}

## a shallow copy of a reference object
## This code depends on knowledge of how classes extend "environment"
.shallowCopy <- function(object, selfEnv) {
    newEnv <- new.env()
    for(what in objects(envir = selfEnv, all.names = TRUE))
        assign(what, get(what, envir = selfEnv), envir = newEnv)
    attr(object, ".xData") <- newEnv
    assign(".self", object, envir = newEnv)
    object
}
