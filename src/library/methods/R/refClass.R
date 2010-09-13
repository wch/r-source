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

envRefAs <- function(Class) {
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
}

envRefImport <- function(value, Class = class(value)) {
    if(!missing(Class))
        value <- as(value, Class)
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
}

installClassMethod <- function(def, self, me, selfEnv, thisClass) {
    if(!is(def, "refMethodDef")) {  #should not happen? => need warning
        warning(gettextf("Method %s from class %s was not processed into a class method until being installed.  Possible corruption of the methods in the class.",
                         me, thisClass@className),
                domain = NA)
        def <-makeClassMethod(def, me, thisClass@className, "", objects(thisClass@refMethods, all.names = TRUE))
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
        newEnv <- new.env(parent = selfEnv)
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
    else {
        if(is(value, fieldClass)) {
            value <- as(value, fieldClass, strict = FALSE)
            assign(field, value, envir = env)
        }
        else
           stop(gettextf("Value supplied for field \"%s\" (with class \"%s\") is not a subclass of required class \"%s\"",
                     field, class(value), fieldClass),
             domain = NA)
    }
    object
}

.initForEnvRefClass <- function(.Object, ...) {
    args <- list(...)
    Class <- class(.Object)
    classDef <- getClass(Class)
    selfEnv <- new.env(TRUE, .NamespaceOrPackage(classDef@package))
    ## the parent environment will be used by field methods, to make
    ## them consistent with functions in this class's package
    .Object@.xData <- selfEnv
    if(length(args)) {
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        elNames <- names(elements)
        supers <- args[!which]
        fieldDefs <- classDef@fieldClasses
        fieldNames <- names(fieldDefs)
        whichFields <- match(elNames, fieldNames, 0) > 0
        whichProto <- is.na(match(fieldNames, elNames))
        for(field in elNames[whichFields])
            envRefSetField(.Object, field, classDef, selfEnv, elements[[field]])
        prototypes <- classDef@fieldPrototypes
        for(field in fieldNames[whichProto])
            envRefSetField(.Object, field, classDef, selfEnv,
                           get(field, envir = prototypes))
        for(readOnly in classDef@fieldReadOnly)
            lockBinding(readOnly, selfEnv)
        other <- c(supers, elements[!whichFields])
        if(length(other))
            ## invoke the default method for superclasses & slots
            return(do.call(methods:::.initialize,
                             c(list(.Object), other)))
    }
    ## assign references to the object and to its class definition
    assign(".self", .Object, envir = selfEnv)
    assign(".refClassDef", classDef, envir = selfEnv)
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

.checkAccessorList <- function(object) {
    if(!is.list(object))
        paste("need a list, got an object of class", class(object)[[1]])
    else if(length(object)){
        funs <- sapply(object, function(x) is(x, "function"))
        if(!identical(unique(funs), TRUE))
           gettextf("Some elements of the list (e.g., %d) are not functions",
                    which(!funs)[[1]])
        else
            TRUE
    }
    else
        TRUE
}

## construct a list of class methods for envRefClass
makeEnvRefMethods <- function() {
    methods <- list( export = envRefAs, import = envRefImport,
                    callSuper = function(...) stop("direct calls to callSuper() are invalid:  should only be called from another method"))
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
                            fieldReadOnly = "character",
                            refMethods = "environment",
                            refSuperClasses = "character",
                            interfaceClasses = "list"),
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
    setClass("refMethodDef",
             representation(mayCall = "character", name = "character",
                            refClassName = "character",
                            superClassMethod = "SuperClassMethod"),
             contains = "function", where = envir)
    setIs("refMethodDef", "SuperClassMethod", where = envir)
    ## the standard R ref class extends "environment" to store fields
    ## but the API allows a different implementation in the future
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
                fieldClasses = list(def = "refClassRepresentation", className = "character"),
                refMethods = list(methods = .refMethods,
                new = .newRefObject, fields = .refFields,
                help = .helpRefMethod))
    setMethod("show", "refClassRepresentation", showRefClassDef, where = envir)
    setMethod("show", "refMethodDef", showClassMethod, where = envir)
    ## a generic function whose methods allow inter-system interface
    ## classes to deliver methods to a reference class
    setGeneric("referenceMethods", function(classDef, where)
               standardGeneric("referenceMethods"), where = envir)
    setMethod("referenceMethods", "ANY", function(classDef, where)
              list(), where = envir)
}

getRefSuperClasses <- function(classes, classDefs) {
    supers <- character()
    for(i in seq_along(classes)) {
        clDef <- classDefs[[i]]
        supers <- c(supers, clDef@refSuperClasses)
    }
    unique(supers)
}

.refMethods <- function(...) {
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
    insertClassMethods(allMethods, className) <- methodDefs
    for(what in mnames)
        assign(what, allMethods[[what]], envir = methodsEnv)
    invisible(methodsEnv)
}

.refFields <- function()
    unlist(def@fieldClasses)

.newRefObject <- function(...) {
    methods::new(def, ...)
}

.helpRefMethod <- function(topic) {
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
}

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

refClassInheritance <- function(Class, contains, interfaceClasses, fieldClasses, fieldPrototypes, fieldReadOnly, refMethods, where) {
    superClassDefs <- lapply(contains,
                             function(what) {
                                 if(is(what, "classRepresentation"))
                                     what
                                 else
                                     getClass(what, where = where)
                             })
    missingDefs <- sapply(superClassDefs, is.null)
    if(any(missingDefs))
        stop(gettextf("No definition found for inherited class: %s",
                      paste('"',contains[missingDefs], '"', sep = "", collapse = ", ")),
             domain = NA)
    superClasses <- unlist(lapply(superClassDefs,
                                  function(def) def@className), FALSE)
    ## interfaceClasses is NULL, or a class definition, or a list of
    ## class definitions.  .
    if(is.null(interfaceClasses)) {}
    else if(is.character(interfaceClasses))
        interfaceClasses <- lapply(interfaceClasses, function(x)getClassDef(x, where = where))
    else if(!is.list(interfaceClasses))
        interfaceClasses <- list(interfaceClasses)
    if(length(interfaceClasses) > 0) {
        for(cl in interfaceClasses) {
            if(is(cl, "classRepresentation")) {
                clName <- cl@className
                if(is.na(match(clName,superClasses)))
                    superClassDefs[[clName]] <- cl
                else
                    warning(gettextf("Interface class \"%s\"  was an explicitly specified superclass as well", clName),
                            domain = NA)
                ## obtain any methods that the interface class wants
                ## included. Standard R reference methods
                ## will be obtained in the loop over refSuperClasses
                ## and so need not be supplied here.
                clMethods <- referenceMethods(cl, where)
                refMethods <- c(refMethods, clMethods)
            }
            ## else ? is there another way to supply reference methods
        }
        superClasses <- unlist(lapply(superClassDefs,
                                      function(def) def@className), FALSE)
    }
    if(length(superClassDefs) > 0) {
        isRefSuperClass <- sapply(superClassDefs, function(def)
                                  is(def, "refClassRepresentation"))
        refSuperClasses <- superClasses[isRefSuperClass]
        ## get the indirectly inherited ref classes:  for consistent
        ## superclass ordering these must precede any other indirectly inherited
        ## classes
        otherRefClasses <- getRefSuperClasses(refSuperClasses, superClassDefs[isRefSuperClass])
        superClasses <- unique(c(superClasses, otherRefClasses))
        refSuperClasses <- unique(c(refSuperClasses, otherRefClasses))
    }
    else {
        superClasses <- refSuperClasses <- character()
        isRefSuperClass <- 0
    }
    ## assemble inherited information
    fc <- fp <- cm <- list(); fr <- character()
    ## assign in reverse order so nearer superclass overrides
    for(cl in rev(superClassDefs[isRefSuperClass])) {
        fcl <- cl@fieldClasses
        fpl <- as.list(cl@fieldPrototypes) # turn env into list
        cml <- as.list(cl@refMethods) # ditto
        frl <- as.character(cl@fieldReadOnly)
        insertFields(fc) <- fcl
        fp[names(fpl)] <- fpl
        fr <- unique(c(fr, frl))
        cm[names(cml)] <- cml
    }
    insertFields(fc) <- fieldClasses
    if(length(fieldReadOnly) > 0)
        fr <- unique(c(fieldReadOnly, fr))
    fp[names(fieldPrototypes)] <- fieldPrototypes

    ## process and insert reference methods
    insertClassMethods(cm, Class) <- refMethods
    list(superClasses = superClasses, refSuperClasses = refSuperClasses,
         fieldClasses = fc, fieldPrototypes = fp,
         fieldReadOnly = fr, refMethods = cm,
         interfaceClasses = interfaceClasses)
}

superClassMethodName <- function(def)
    paste(def@name, def@refClassName, sep = "#")

`insertClassMethods<-` <- function(methods, Class, value) { # `value' is refMethods
    ## process the class methods to include references
    ## (this information is needed for the instance environment as used
    ## in envRefClass, and conceivably might not be needed for other
    ## implementations of class methods.  This step could then be optional.)
    theseMethods <- names(value)
    prevMethods <- names(methods) # catch refs to inherited methods as well
    allMethods <- unique(c(theseMethods, prevMethods))
    for(method in theseMethods) {
        prevMethod <- methods[[method]] # NULL or superClass method
        if(is.null(prevMethod))
            superClassMethod <- ""
        else {
            superClassMethod <- superClassMethodName(prevMethod)
            methods[[superClassMethod]] <- prevMethod
        }
        methods[[method]] <-
               makeClassMethod(value[[method]], method, Class,
                               superClassMethod, allMethods)
    }
    methods
}

setRefClass <- function(Class, fieldClasses = list(),
                        contains = character(),
                        refMethods = list(),
                        fieldPrototypes = list(),
                        fieldReadOnly = character(),
                        interfaceClasses = NULL,
                        where = topenv(parent.frame()),
                        ...) {
    fieldNames <- names(fieldClasses)
    if(length(fieldReadOnly) > 0) {
        if(!is(fieldReadOnly, "character"))
            stop(gettextf("The fieldReadOnly argument must be the names of the read-only fields; got an object of class \"%s\"", class(fieldReadOnly)),
                 domain = NA)
        fieldReadOnly <- as(fieldReadOnly, "character")
        if(any(is.na(match(fieldReadOnly, fieldNames)))) {
            bad <- paste('"',
                        fieldReadOnly[is.na(match(fieldReadOnly, fieldNames))],
                        '"', sep = "", collapse = ", ")
            stop(gettextf("Some fieldReadOnly names are not fields: %s", bad),
                domain = NA)
        }
    }
    ## generate the field access methods, but allow explicit methods to
    ## override
    if(length(fieldClasses) > 0) {
        accessorList <- envRefMakeAccessors(fieldClasses, fieldReadOnly, where)
        if(length(refMethods) > 0) {
            notExplicit <- is.na(match(names(accessorList), names(refMethods)))
            accessorList <- accessorList[notExplicit]
        }
        refMethods <- c(refMethods, accessorList)
    }
    if(length(fieldPrototypes)) {
        which <- names(fieldPrototypes)
        if(any(is.na(match(which, fieldNames)))) {
            which <- paste(which[is.na(match(which, fieldNames))],
                           sep = "", collapse = ", ")
            stop(gettextf("Prototypes supplied for non-fields: %s",which),
                 domain = NA)
        }
        ## the remaining fields will get prototypes from class definiition
        fieldNames <- fieldNames[is.na(match(fieldNames, which))]
    }
    undef <- character()
    for(field in fieldNames) {
        fieldDef <- getClassDef(fieldClasses[[field]], where = where)
        if(is.null(fieldDef))
            undef <- c(undef, field)
        else
            fieldPrototypes[[field]] <- fieldDef@prototype
    }
    if(length(undef))
        stop(gettextf("Field classes undefined: %s", paste(undef, collapse = ", ")),
             domain = NA)
    theseMethods <- names(refMethods) # non-inherited, for processing later
    ## include envRefClass automatically (and so refClass indirectly).
    contains <- unique(c(contains, "envRefClass"))
    ## collect the method and field definitions, including interfaceClasses
    info <- refClassInheritance(Class, contains, interfaceClasses, fieldClasses, fieldPrototypes, fieldReadOnly, refMethods, where)
    ## think Python's multiple assignment operator
    for(what in c("superClasses", "refSuperClasses", "fieldClasses",
                  "fieldPrototypes", "fieldReadOnly", "refMethods",
                  "interfaceClasses"))
        assign(what, info[[what]])
    ## temporarily assign an ordinary class definition
    ## to allow the checks and defaults from setClass to be applied
    setClass(Class, contains = superClasses,
             where = where, ...)
    ## now, override that with the complete definition
    classDef <- new("refClassRepresentation",
                    getClassDef(Class, where = where),
                    fieldClasses = fieldClasses,
                    refMethods = as.environment(refMethods),
                    fieldPrototypes = as.environment(fieldPrototypes),
                    fieldReadOnly = fieldReadOnly,
                    refSuperClasses = refSuperClasses)
    assignClassDef(Class, classDef, where)
    value <- new("refObjectGenerator", def = classDef, className = Class)
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

showRefClassDef <- function(object) {
    cat("Reference Class \"", object@className,"\":\n")
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


envRefMakeAccessors <- function(fieldClasses, readOnly, env = .GlobalEnv) {
    fieldNames <- names(fieldClasses)
    methodNames <- firstCap(fieldNames)
    getters <- methodNames$get
    setters <- methodNames$set
    methods <- list()
    for(i in seq_along(fieldNames)) {
        field <- as.name(fieldNames[[i]])
        CLASS <- fieldClasses[[i]]
        methods[[getters[[i]] ]] <- eval(substitute(function() X,
                                          list(X = field)), env)
        if(is.na(match(fieldNames[[i]], readOnly)))
            methods[[setters[[i]] ]] <-
                eval(substitute(function(value) {
                    value <- as(value, CLASS, strict = FALSE)
                    X <<- value
                    value
                    },
                                list(X = field, CLASS = CLASS)), env)
        else
            methods[[setters[[i]] ]] <-
                eval(substitute(function(value)
                     stop("Trying to set a read-only field (", X, ")"),
                                list(X = fieldNames[[i]])), env)
    }
    methods
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
