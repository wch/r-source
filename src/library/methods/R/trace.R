.TraceWithMethods <- function(what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE, signature = NULL) {
    if(is.function(what)) {
        fname <- substitute(what)
        if(is.name(fname))
            what <- as.character(fname)
        else
            stop("Argument what should be the name of a function")
    }
    else {
        what <- as.character(what)
        if(length(what) != 1) {
            for(f in what) {
                if(nargs() == 1)
                    trace(f)
                else
                    trace(f, tracer, exit, at, print, signature)
            }
            return(what)
        }
    }
    if(nargs() == 1)
        return(.primTrace(what)) # for back compatibility
    if(is.null(signature)) {
        def <- getFunction(what)
        where <- findFunction(what)[[1]]
    }
    else {
        whereM <- findMethod(what, signature)
        if(length(whereM) == 0) {
            def <- selectMethod(what, signature)
            where <- findFunction(what)[[1]]
        }
        else {
            whereM <- whereM[[1]]
            def <- getMethod(what, signature, where = whereM)
            where <- whereM
        }
    }
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
    ## undo any current tracing
    def <- .untracedFunction(def)
    newFun <- new(.traceClassName(class(def)), def = def, tracer = tracer, exit = exit, at = at
                  , print = print)
    if(is.null(signature)) 
        assign(what, newFun, where)
    else
        setMethod(what, signature, newFun, where = where)
    what
}

.makeTracedFunction <- function(def, tracer, exit, at, print) {
    switch(typeof(def),
           builtin = , special = {
               fBody <- substitute({.prim <- DEF; .prim(...)},
                                   list(DEF = def))
               def <- eval(function(...)NULL)
               environment(def) <-  .GlobalEnv
               warning("making a traced version of a primitive; arguments will be treated as \"...\"")
           }, {
               if(is(def, "traceable"))
                  def <- .untracedFunction(def)
               fBody <- body(def)
              }
           )
    if(length(at) > 0) {
        if(is.null(tracer))
            stop("can't use \"at\" argument without a trace expression")
        else if(class(fBody) != "{")
            stop("can't use \"at\" argument unless the function body has the form { ... }")
        for(i in at) {
            if(print)
                expri <- substitute({if(tracingState()){.doTracePrint(MSG); TRACE}; EXPR},
                            list(TRACE = tracer, MSG = paste("step",i), EXPR = fBody[[i]]))
            else
                expri <- substitute({if(tracingState())TRACE; EXPR},
                            list(TRACE=tracer, EXPR = fBody[[i]]))
            fBody[[i]] <- expri
        }
    }
    else if(!is.null(tracer)){
            if(print)
                fBody <- substitute({if(tracingState()){.doTracePrint(MSG); TRACE}; EXPR},
                            list(TRACE = tracer, MSG = paste("on entry"), EXPR = fBody))
            else
                fBody <- substitute({if(tracingState())TRACE; EXPR},
                            list(TRACE=tracer, EXPR = fBody))
    }
    if(!is.null(exit)) {
        if(print)
            exit <- substitute(if(tracingState()){.doTracePrint(MSG); EXPR},
                            list(EXPR = exit, MSG = paste("on exit")))
        else
            exit <- substitute(if(tracingState())EXPR,
                            list(EXPR = exit, MSG = paste("on exit")))
        fBody <- substitute({on.exit(TRACE); BODY},
                            list(TRACE=exit, BODY=fBody))
    }
    body(def, envir = environment(def)) <- fBody
    def
}

.untracedFunction <- function(f) {
    while(is(f, "traceable"))
        f <- f@original
    f
}
        

.InitTraceFunctions <- function(envir)  {
    setClass("traceable", representation("VIRTUAL", original = "PossibleMethod"),
             sealed = TRUE, where = envir)
    ## create the traceable classes
    for(cl in c("function", "MethodDefinition", "MethodWithNext", "genericFunction",
                "groupGenericFunction"))
        setClass(.traceClassName(cl),
                 representation(cl, "traceable"), sealed = TRUE, where = envir)
    setMethod("initialize", "traceable",
              function(.Object, def, tracer, exit, at, print) {
                  oldClass <- class(def)
                  if(isClass(oldClass) && length(getClass(oldClass)@slots) > 0)
                      as(.Object, oldClass) <- def # to get other slots in def
                  .Object@original <- def
                  if(!is.null(elNamed(getSlots(class(def)), ".Data")))
                      def <- def@.Data
                  .Object@.Data <- .makeTracedFunction(def, tracer, exit, at, print)
                  .Object
              })
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
