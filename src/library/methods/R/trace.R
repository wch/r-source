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
                expri <- substitute({.doTracePrint(MSG); TRACE; EXPR},
                            list(TRACE = tracer, MSG = paste("step",i), EXPR = fBody[[i]]))
            else
                expri <- substitute({TRACE; EXPR},
                            list(TRACE=tracer, EXPR = fBody[[i]]))
            fBody[[i]] <- expri
        }
    }
    else if(!is.null(tracer)){
            if(print)
                fBody <- substitute({.doTracePrint(MSG); TRACE; EXPR},
                            list(TRACE = tracer, MSG = paste("on entry"), EXPR = fBody))
            else
                fBody <- substitute({TRACE; EXPR},
                            list(TRACE=tracer, EXPR = fBody))
    }
    if(!is.null(exit)) {
        if(print)
            exit <- substitute({.doTracePrint(MSG); EXPR},
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
    setClass("traceable", representation("VIRTUAL", original = "PossibleMethod"))
    ## create the traceable classes
    for(cl in c("function", "MethodDefinition", "MethodWithNext"))
        setClass(.traceClassName(cl),
                 representation(cl, "traceable"))
    setMethod("initialize", "traceable",
              function(object, def, tracer, exit, at, print) {
                  object@original <- def
                  if(!is.null(elNamed(getSlots(class(def)), ".Data")))
                      def <- def@.Data
                  object@.Data <- .makeTracedFunction(def, tracer, exit, at, print)
                  object
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
