## methods for the standard reference class (using R environments)
## Several of these are candidates for implementing in C since they
## will be called frequently.

stdRefInferField <- function(self, field, thisClass, selfEnv = as.environment(self)) {
    'Install a field method into the environment of object
self from reference class thisClass.'
    fields <- thisClass@fieldPrototypes
    if(exists(field, envir = fields, inherits = FALSE)) {
        ## this allows lazy installation of fields (not currently used)
        value <- get(field, envir = fields)
    }
    else {
        methods <- thisClass@classMethods
        if(exists(field, envir = methods, inherits = FALSE)) {
            value <- get(field, envir = methods)
            ## install this method and any methods it may call
            value <- installClassMethod(value, self, field, selfEnv, thisClass)
        }
        else
            stop(gettextf("Field \"%s\" is not a valid field or method name for reference class \"%s\"",
                          field, thisClass@refClassName),
                 domain = NA)
        ## make a copy with the instance as environmen
        ## This is expensive & should be replaced, e.g., by
        ## special evaluator code for functions that are classMethods
        environment(value) <- selfEnv
    }
    value
}

installClassMethod <- function(def, self, me, selfEnv, thisClass) {
    if(!is(def, "classMethodDef")) {
        def <-makeClassMethod(def, me, objects(thisClass@classMethods, all.names = TRUE))
        ## cache the analysed method definition
        assign(me, def, envir = thisClass@classMethods)
    }
    else
        depends <- def@mayCall
    environment(def) <- selfEnv # for access to fields and methods
    assign(me, def, envir = selfEnv)
    ## process those that are not in the instance environment, now that
    ## this method has been assigned.
    done <- objects(selfEnv, all.names = TRUE)
    notdone <- depends[is.na(match(depends, done))]
    for(what in notdone)
        installClassMethod(get(what, envir = thisClass@classMethods), self, what, selfEnv, thisClass)
    def
   }

makeClassMethod <- function(def, name, allMethods) {
    ## NB:  recomended package codetools must be present
    depends <- codetools::findGlobals(def, merge = FALSE)$functions
    ## find the field methods called ...
    depends <- depends[match(depends, allMethods, 0) > 0]
    new("classMethodDef", def, mayCall = depends, name = name)
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

stdRefSetField <- function(object, field, thisClass = refObjectClass(object), value) {
    fieldClass <- thisClass@fieldClasses[[field]]
    if(is.null(fieldClass))
        stop(gettextf("\"%s\" is not a field in class \"%s\"",field, thisClass@refClassName),
             domain = NA)
    else {
        if(is(value, fieldClass)) {
            value <- as(value, fieldClass, strict = FALSE)
            assign(field, value, envir = as.environment(object))
        }
        else
           stop(gettextf("Value supplied for field \"%s\" (with class \"%s\") is not a subclass of required class \"%s\"",
                     field, class(value), fieldClass),
             domain = NA)
    }
    object
}

.initForStdRefClass <- function(.Object, ...) {
    args <- list(...)
    Class <- class(.Object)
    ClassDef <- getClass(Class)
    selfEnv <- new.env(TRUE, .NamespaceOrPackage(ClassDef@package))
    ## the parent environment will be used by field methods, to make
    ## them consistent with functions in this class's package
    .Object@.xData <- selfEnv
    if(length(args)) {
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        elNames <- names(elements)
        supers <- args[!which]
        fieldDefs <- ClassDef@fieldClasses
        fieldNames <- names(fieldDefs)
        whichFields <- match(elNames, fieldNames, 0) > 0
        whichProto <- is.na(match(fieldNames, elNames))
        for(field in elNames[whichFields])
            stdRefSetField(.Object, field, ClassDef, elements[[field]])
        prototypes <- ClassDef@fieldPrototypes
        for(field in fieldNames[whichProto])
            stdRefSetField(.Object, field, ClassDef,
                           get(field, envir = prototypes))
        other <- c(supers, elements[!whichFields])
        if(length(other))
            ## invoke the default method for superclasses & slots
            return(do.call(methods:::.initialize,
                             c(list(.Object), other)))
    }
    assign(".self", .Object, envir = selfEnv) # ref to the completed object
    .Object
}

.dollarForStdRefClass <- function(x, name) {
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
        stdRefInferField(x, what, getClass(class(x)), selfEnv)
}

.dollarGetsForStdRefClass <- function(x, name, value) {
    what <- substitute(name)
    if(is.symbol(what))
        what <- as.character(what)
    else
        what <- name
    selfEnv <- as.environment(x)
    stdRefSetField(x, what, refObjectClass(x), value)
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
                            classMethods = "environment",
                            refClassName = "character",
                            refSuperClasses = "character",
                            fieldAccessorGenerator = "optionalMethod"),
             contains = "classRepresentation", where = envir)
    ## the virtual class from which all true reference clases
    ## inherit.  Its subclasses require methods
    ## for getting & setting fields and related tasks
    setClassUnion("refClass", where = envir)
    ## the union of all reference objects
    ## (including those not belonging to refClass)
    setClassUnion("refObject", c("environment", "externalptr", "name",                                "refClass"), where = envir)
    ## the standard R ref class just uses an environment
    ## but the API allows a different implementation in the future
    setClass("stdRefClass", contains = c("environment"), where =envir)
    ## bootstrap stdRefClass as a refClass
    def <- new("refClassRepresentation", refClassName = "stdRefClass",
               fieldAccessorGenerator = stdRefMakeAccessors)
    as(def, "classRepresentation") <- getClassDef("stdRefClass", where = envir)
    assignClassDef("stdRefClass", def, where = envir)
    setMethod("initialize", "stdRefClass", methods:::.initForStdRefClass,
              where = envir)
    ## a class for field methods, with a slot for their dependencies,
    ## allowing installation of all required instance methods
    setClass("classMethodDef", representation(mayCall = "character", name = "character"),
             contains = "function", where = envir)
    ## NOTE:  "$" method requires setting in methods:::.InitStructureMethods
    setMethod("$", "stdRefClass", .dollarForStdRefClass, where = envir)
    setMethod("$<-", "stdRefClass", .dollarGetsForStdRefClass, where = envir)
    setMethod("show", "refClassRepresentation", showRefClassDef, where = envir)
    setMethod("show", "classMethodDef", showClassMethod, where = envir)
}

getRefSuperClasses <- function(classes, classDefs) {
    supers <- character()
    for(i in seq_along(classes)) {
        clDef <- classDefs[[i]]
        supers <- c(supers, clDef@refSuperClasses)
    }
    unique(supers)
}

refClassInheritance <- function(contains, fieldClasses, fieldPrototypes, classMethods, where) {
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
    isRefSuperClass <- sapply(superClassDefs, function(def)
                              is(def, "refClassRepresentation"))
    superClasses <- unlist(lapply(superClassDefs,
                                  function(def) def@className), FALSE)
    refSuperClasses <- superClasses[isRefSuperClass]
    ## get the indirectly inherited ref classes:  for consistent
    ## superclass ordering these must precede any other indirectly inherited
    ## classes
    otherRefClasses <- getRefSuperClasses(refSuperClasses, superClassDefs[isRefSuperClass])
    superClasses <- unique(c(superClasses, otherRefClasses))
    refSuperClasses <- unique(c(refSuperClasses, otherRefClasses))
    ## assemble inherited information
    fc <- fp <- cm <- list()
    ## assign in reverse order so nearer superclass overrides
    for(cl in rev(superClassDefs[isRefSuperClass])) {
        fcl <- cl@fieldClasses
        fpl <- as.list(cl@fieldPrototypes) # turn env into list
        cml <- as.list(cl@classMethods) # ditto
        fc[names(fcl)] <- fcl
        fp[names(fpl)] <- fpl
        cm[names(cml)] <- cml
    }
    fc[names(fieldClasses)] <- fieldClasses
    fp[names(fieldPrototypes)] <- fieldPrototypes
    ## tentative mechanism for "next method"
    newM <- names(classMethods)
    previous <- newM[match(newM, names(cm), 0) > 0]
    for(method in previous)
        cm[[paste(method, "prev", sep=".")]] <- cm[[method]]
    cm[newM] <- classMethods
    list(superClasses = superClasses, refSuperClasses = refSuperClasses,
         fieldClasses = fc, fieldPrototypes = fp, classMethods = cm)
}

setRefClass <- function(Class, fieldClasses = list(),
                        contains = character(),
                        classMethods = list(),
                        fieldPrototypes = list(),
                        basicRefClass = "stdRefClass",
                        refClassName = Class,
                        where = topenv(parent.frame()),
                        ...) {
    ## validate the basicRefClass and get its accessor generator
    basicDef <- getClassDef(basicRefClass, where = where)
    if(!is(basicDef, "refClassRepresentation"))
        stop(gettextf("\"%s\" is not a known reference class", basicRefClass),
             domain = NA)
    fieldGenerator = basicDef@fieldAccessorGenerator
    ## generate the field access methods, but allow explicit methods to
    ## override
    if(is.function(fieldGenerator) && length(fieldClasses) > 0) {
        accessorList <- fieldGenerator(names(fieldClasses))
        ## validate the methods
        if(!identical(.checkAccessorList(accessorList), TRUE))
            stop(gettextf("Invalid accessor function list: %s",
                          .checkAccessorList(accessorList)),
                 domain = NA)
        if(length(classMethods) > 0) {
            notExplicit <- is.na(match(names(accessorList), names(classMethods)))
            accessorList <- accessorList[notExplicit]
        }
        classMethods <- c(classMethods, accessorList)
    }
    fieldNames <- names(fieldClasses)
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
    theseMethods <- names(classMethods) # non-inherited, for processing later
    ## collect the superclass definitions
    if(length(contains)) {
        info <- refClassInheritance(contains, fieldClasses, fieldPrototypes, classMethods, where)
        ## think Python's multiple assignment operator
        for(what in c("superClasses", "refSuperClasses", "fieldClasses",
                      "fieldPrototypes", "classMethods"))
            assign(what, info[[what]])
    }
    else {
        superClasses <- character()
        refSuperClasses <- character()
    }
    ## process the class methods to include references
    ## (this information is needed for the instance environment as used
    ## in stdRefClass, and conceivably might not be needed for other
    ## implementations of class methods.  This step could then be optional.)
    allMethods <- names(classMethods) # catch refs to inherited methods as well
    for(method in theseMethods)
        classMethods[[method]] <-
               makeClassMethod(classMethods[[method]], method, allMethods)
    ## temporarily assign an ordinary class definition
    ## to allow the checks and defaults from setClass to be applied
    setClass(Class, contains = c(superClasses, basicRefClass, "refClass"),
             where = where, ...)
    ## now, override that with the complete definition
    classDef <- new("refClassRepresentation",
                    getClassDef(Class, where = where),
                    fieldClasses = fieldClasses,
                    classMethods = as.environment(classMethods),
                    fieldPrototypes = as.environment(fieldPrototypes),
                    refClassName = refClassName,
                    refSuperClasses = refSuperClasses)
    assignClassDef(Class, classDef, where)
    invisible(classDef)
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
        value <- as.list(ClassDef@classMethods)
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
    if(!.identC(cl, "classMethodDef"))
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
    cat("Reference Class \"", object@refClassName,"\":\n")
    fields <- object@fieldClasses
    if(length(fields))
        printPropertiesList(fields, "Class fields")
    else
        cat("\nNo fields defined\n")
    methods <- objects(object@classMethods, all.names = TRUE)
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


stdRefMakeAccessors <- function(fields, generator = firstCap, env = .GlobalEnv) {
    methodNames <- generator(fields)
    getters <- methodNames$get
    setters <- methodNames$set
    methods <- list()
    for(i in seq_along(fields)) {
        field <- as.name(fields[[i]])
        methods[[getters[[i]] ]] <- eval(substitute(function() X,
                                          list(X = field)), env)
        methods[[setters[[i]] ]] <- eval(substitute(function(value) X <<- value,
                                          list(X = field)), env)
    }
    methods
}
