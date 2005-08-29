.TraceWithMethods <- function(what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE, signature = NULL, where = .GlobalEnv, edit = FALSE, from = NULL, untrace = FALSE) {
    if(is.function(where)) {
        ## start from the function's environment:  important for tracing from a namespace
        where <- environment(where)
        fromPackage <- getPackageName(where)
    }
    else fromPackage <- ""
    whereF <- baseenv()
    pname <- character()
    def <- NULL
    if(is.function(what)) {
        def <- what
        if(is(def, "genericFunction")) {
            what <- def@generic
            whereF <- .genEnv(what, where)
            pname <- def@package
        }
        else {
            fname <- substitute(what)
            if(is.name(fname)) {
                what <- as.character(fname)
                temp <- .findFunEnvAndName(what, where)
                whereF <- temp$whereF
                pname <- temp$pname
            }
            else if(is.call(fname) && identical(fname[[1]], as.name("::"))) {
                whereF <-as.character(fname[[2]])
                require(whereF, character.only = TRUE)
                whereF <- as.environment(paste("package", whereF, sep=":"))
                pname <-  fname[[2]]
                what <- as.character(fname[[3]])
            }
            else if(is.call(fname) && identical(fname[[1]], as.name(":::"))) {
                pname <- paste(fname[[2]], "(not-exported)")
                whereF <- loadNamespace(as.character(fname[[2]]))
                what <- as.character(fname[[3]])
            }
            else
                stop("argument 'what' should be the name of a function")
        }
    }
    else {
        what <- as(what, "character")
        if(length(what) != 1) {
            for(f in what) {
                if(nargs() == 1)
                    trace(f)
                else
                    Recall(f, tracer, exit, at, print, signature, where, edit, from, untrace)
            }
            return(what)
        }
        temp <- .findFunEnvAndName(what, where, signature)
        whereF <- temp$whereF
        pname <- temp$pname
    }
    if(nargs() == 1)
        return(.primTrace(what)) # for back compatibility
    if(identical(whereF, baseenv())) {
        allWhere <- findFunction(what, where = where)
        if(length(allWhere)==0)
            stop(gettextf("no function definition for '%s' found", what),
                 domain = NA)
        whereF <- as.environment(allWhere[[1]])
    }
        if(is.null(def))
            def <- getFunction(what, where = whereF)
        if(!is.null(signature)) {
            fdef <- def
            def <- selectMethod(what, signature, fdef = def)
        }
    if(untrace) {
    if(is.null(signature)) {
        ## ensure that the version to assign is untraced
        if(is(def, "traceable")) {
            newFun <- .untracedFunction(def)
        }
        else {
            .primUntrace(what) # to be safe--no way to know if it's traced or not
            return(what)
        }
    }
    else {
            if(is(def, "traceable"))
                newFun <- .untracedFunction(def)
            else {
                warning(gettextf("the method for \"%s\" for this signature was not being traced", what), domain = NA)
                return(what)
            }
        }
    }
    else {
        fBody <- body(def)
        if(!is.null(exit)) {
            if(is.function(exit)) {
                tname <- substitute(exit)
                if(is.name(tname))
                    exit <- tname
                exit <- substitute(TRACE(), list(TRACE=exit))
            }
        }
        if(!is.null(tracer)) {
            if(is.function(tracer)) {
                tname <- substitute(tracer)
                if(is.name(tname))
                    tracer <- tname
                tracer <- substitute(TRACE(), list(TRACE=tracer))
            }
        }
        ## calls .makeTracedFunction via the initialize method for "traceable"
        newFun <- new(.traceClassName(class(.untracedFunction(def))),
                      def = def, tracer = tracer, exit = exit, at = at,
                      print = print, doEdit = edit)
    }
    global <- identical(whereF, .GlobalEnv)
    if(is.null(signature)) {
        if(bindingIsLocked(what, whereF))
            .assignOverBinding(what, newFun, whereF, global)
        else
            assign(what, newFun, whereF)
    }
    else {
        ## arrange for setMethod to put the new method in the generic
        ## but NOT to assign the methods list object (binding is ignored)
        setMethod(fdef, signature, newFun, where = baseenv())
    }
    if(!global) {
        action <- if(untrace)"Untracing" else "Tracing"
        location <- if(.identC(fromPackage, "")) {
            if(length(pname)==0  && !identical(whereF, baseenv()))
                pname <- getPackageName(whereF)
            if(length(pname)==0)
                "\""
            else
                paste("\" in package \"",
                                                   pname, "\"", sep="")
        }
        else paste("\" as seen from package \"", fromPackage, "\"", sep="")
        object <- if(is.null(signature)) " function \"" else " specified method for function \""
        .message(action, object, what, location)
    }
    what
}

.makeTracedFunction <- function(def, tracer, exit, at, print, doEdit) {
    switch(typeof(def),
           builtin = , special = {
               fBody <- substitute({.prim <- DEF; .prim(...)},
                                   list(DEF = def))
               def <- eval(function(...)NULL)
               body(def, environment = .GlobalEnv) <- fBody
               warning("making a traced version of a primitive; arguments will be treated as '...'")
           }
           )
    if(!identical(doEdit, FALSE)) {
        if(is.character(doEdit) || is.function(doEdit)) {
            editor <- doEdit
            doEdit <- TRUE
        }
        else
            editor <- getOption("editor")
    }
    ## look for a request to edit the definition
    if(doEdit) {
        if(is(def, "traceable"))
            def <- as(def, "function") # retain previous tracing if editing
        if(!is.na(match(editor, c("emacs","xemacs")))) {
            ## cater to the usual emacs modes for editing R functions
            file <- tempfile("emacs")
            file <- sub('..$', ".R", file)
        }
        else
            file <- ""
        ## insert any requested automatic tracing expressions before editing
        if(!(is.null(tracer) && is.null(exit) && length(at)==0))
            def <- Recall(def, tracer, exit, at, print, FALSE)
        def2 <- edit(def, editor = editor, file = file)
        if(!is.function(def2))
            stop(gettextf("the editing in trace() can only change the body of the function; got an object of class \"%s\"", class(def2)), domain = NA)
        if(!identical(args(def), args(def2)))
            stop("the editing in trace() can only change the body of the function, not the arguments or defaults")
        fBody <- body(def2)
    }
    else {
        def <- .untracedFunction(def) # throw away earlier tracing
        fBody <- body(def)
        if(length(at) > 0) {
            if(is.null(tracer))
                stop("cannot use 'at' argument without a trace expression")
            else if(class(fBody) != "{")
                stop("cannot use 'at' argument unless the function body has the form '{ ... }'")
            for(i in at) {
                if(print)
                    expri <- substitute({if(tracingState()){methods::.doTracePrint(MSG); TRACE}; EXPR},
                                        list(TRACE = tracer, MSG = paste("step",i), EXPR = fBody[[i]]))
                else
                    expri <- substitute({if(tracingState())TRACE; EXPR},
                                        list(TRACE=tracer, EXPR = fBody[[i]]))
                fBody[[i]] <- expri
            }
        }
        else if(!is.null(tracer)){
            if(print)
                fBody <- substitute({if(tracingState()){methods::.doTracePrint(MSG); TRACE}; EXPR},
                                    list(TRACE = tracer, MSG = paste("on entry"), EXPR = fBody))
            else
                fBody <- substitute({if(tracingState())TRACE; EXPR},
                                    list(TRACE=tracer, EXPR = fBody))
        }
        if(!is.null(exit)) {
            if(print)
                exit <- substitute(if(tracingState()){methods::.doTracePrint(MSG); EXPR},
                                   list(EXPR = exit, MSG = paste("on exit")))
            else
                exit <- substitute(if(tracingState())EXPR,
                                   list(EXPR = exit, MSG = paste("on exit")))
            fBody <- substitute({on.exit(TRACE); BODY},
                                list(TRACE=exit, BODY=fBody))
        }
    }
    body(def, envir = environment(def)) <- fBody
    def
}

## return the untraced version of f
.untracedFunction <- function(f) {
    while(is(f, "traceable"))
        f <- f@original
    f
}


.InitTraceFunctions <- function(envir)  {
    setClass("traceable", representation(original = "PossibleMethod"), contains = "VIRTUAL",
             where = envir); clList <- "traceable"
    ## create the traceable classes
    for(cl in c("function", "MethodDefinition", "MethodWithNext", "genericFunction",
                "standardGeneric", "nonstandardGeneric", "groupGenericFunction",
                "derivedDefaultMethod")) {
        setClass(.traceClassName(cl),
                 representation(cl, "traceable"), where = envir)
        clList <- c(clList, .traceClassName(cl))
    }
    assign(".SealedClasses", c(get(".SealedClasses", envir), clList), envir);
    setMethod("initialize", "traceable",
              function(.Object, def, tracer, exit, at, print, doEdit) {
                  oldClass <- class(def)
                  oldClassDef <- getClass(oldClass)
                  if(!is.null(oldClassDef) && length(oldClassDef@slots) > 0)
                      as(.Object, oldClass) <- def # to get other slots in def
                  .Object@original <- def
                  if(!is.null(elNamed(getSlots(getClass(class(def))), ".Data")))
                      def <- def@.Data
                  .Object@.Data <- .makeTracedFunction(def, tracer, exit, at, print, doEdit)
                  .Object
              }, where = envir)
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir)
    setMethod("show", "traceable", function(object) {
        cat("Object of class \"", class(object), "\"\n")
        show(object@original)
        cat("\n## (to see the tracing code, look at body(object))\n")
    }, where = envir)
}

.doTracePrint <- function(msg = "") {
    call <- deparse(sys.call(sys.parent(1)))
    if(length(call)>1)
        call <- paste(call[[1]], "....")
    cat("Tracing", call, msg, "\n")
}

.traceClassName <- function(className) {
    paste(className, "WithTrace", sep="")
}

trySilent <- function(expr) {
    call <- sys.call()
    call[[1]] <- quote(try)
    opt1 <- options(show.error.messages = FALSE)
    opt2 <- options(error = quote(empty.dump()))
    ## following is a workaround of a bug in options that does not
    ## acknowledge NULL as an options value => delete this element.
    if(is.null(opt1[[1]]))
      on.exit({options(show.error.messages = TRUE); options(opt2)})
    else
      on.exit({options(opt1); options(opt2)})
    eval.parent(call)
}

.assignOverBinding <- function(what, value, where, verbose = TRUE) {
    pname <- getPackageName(where)
    if(verbose) {
        msg <-
            gettextf("assigning over the binding of symbol \"%s\" in environment/package \"%s\"",
                     what, pname)
        message(strwrap(msg), domain = NA)
    }
    warnOpt <- options(warn= -1) # kill the obsolete warning from R_LockBinding
    on.exit(options(warnOpt))
    if(is.function(value)) {
        ## assign in the namespace for the function as well
        fenv <- environment(value)
        if(!identical(fenv, where) && exists(what, envir = fenv, inherits = FALSE #?
                                             ) && bindingIsLocked(what, fenv)) {
            unlockBinding(what, fenv)
            assign(what, value, fenv)
            lockBinding(what, fenv)
        }
    }
    unlockBinding(what, where)
    assign(what, value, where)
    lockBinding(what, where)
}

.setMethodOverBinding <- function(what, signature, method, where, verbose = TRUE) {
    if(verbose)
        warning(gettextf("setting a method over the binding of symbol '%s' in environment/package '%s'", what, getPackageName(where)), domain = NA)
    if(exists(what, envir = where, inherits = FALSE)) {
        fdef <- get(what, envir = where)
        hasFunction <- is(fdef, "genericFunction")
    }

        hasFunction <- FALSE
    if(hasFunction) {
        ## find the generic in the corresponding namespace
        metaName <- mlistMetaName(fdef)
        where2 <- findFunction(what, where = environment(fdef))[[1]] # must find it?
        unlockBinding(metaName, where)
        unlockBinding(what, where)
        setMethod(what, signature, method, where = where)
        lockBinding(what, where)
        lockBinding(metaName, where)
        ## assign in the package namespace as well
        unlockBinding(what, where2)
        unlockBinding(metaName, where2) # FIXME:  look for sig. ?
        setMethod(what, signature, method, where = where2)
        lockBinding(metaName, where2)
        lockBinding(what, where2)
    }
    else {
        metaName <- mlistMetaName(what)
        unlockBinding(metaName, where)
        setMethod(what, signature, method, where = where)
        lockBinding(metaName, where)
    }
}

### finding the package name for a loaded namespace -- kludgy but is there
### a table in this direction anywhere?
.searchNamespaceNames <- function(env) {
    namespaces <- .Internal(getNamespaceRegistry())
    names <- objects(namespaces, all = TRUE)
    for(what in names)
        if(identical(get(what, envir=namespaces), env))
            return(paste("namespace", what, sep=":"))
    return(character())
}

.findFunEnvAndName <- function(what, where, signature = NULL) {
    pname <- character()
    if(is.null(signature)) {
        whereF <- findFunction(what, where = where)
        if(length(whereF)>0)
            whereF <- whereF[[1]]
        else return(list(pname = pname, whereF = baseenv()))
    }
    else {
        whereF <- .genEnv(what, where)
    }
    if(is.null(whereF)) { ## stupid convention that NULL == base package
        whereF <- .BaseNamespaceEnv
        pname <- "base"
    }
    else if(!is.null(attr(whereF, "name")))
        pname <- gsub("^.*:", "", attr(whereF, "name"))
    else if(isNamespace(whereF))
        pname <- .searchNamespaceNames(whereF)
    list(pname=pname, whereF = whereF)
}
