## functions to support a session frame and session-scope tracing
## of function calls in R similar to the S-Plus (S4) trace, untrace, browser functions.

sessionData <-
  ## return the index of the session data in the search list,
  ## attaching it if it is not attached
  function() {
    value <- match(".Session", search())
    if(is.na(value)) {
      attach(NULL, name=".Session")
      value <- match(".Session", search())
    }
    value
  }

traceOn <-
  ## initialize tracing on calls to function `what'.  The function or expression
  ## `tracer' is called on entry, and the function or expression `exit' on exit.
  function(what, tracer = browseAll, exit = NULL)
{
    warning("'traceOn' is deprecated:  use the function 'trace' in the 'methods' package instead")
    name <- what; def <- what
    if(is.function(name))
        name <- as.character(substitute(what))
    else
        def <- getFunction(name)
    if(exists(name, sessionData(), inherits = FALSE)) {
        remove(list=name, pos=sessionData())
        def <- getFunction(name)
    }
    fBody <- body(def)
    if(!is.null(exit)) {
        if(missing(tracer))
            tracer <- NULL
        if(is.function(exit)) {
            tname <- substitute(exit)
            if(is.name(tname))
                exit <- tname
            tracexp <- substitute(TRACE(), list(TRACE=exit))
        }
        else
            tracexp <- exit
        fBody <- substitute({on.exit(TRACE); BODY},
                            list(TRACE=tracexp, BODY=fBody))
    }
    if(!is.null(tracer)) {
        if(is.function(tracer)) {
            tname <- substitute(tracer)
            if(is.name(tname))
                tracer <- tname
            tracexp <- substitute(TRACE(), list(TRACE=tracer))
        }
        else
            tracexp <- tracer
        fBody <- substitute({TRACE; BODY},
                            list(TRACE=tracexp, BODY=fBody))
    }
    body(def) <- fBody
    mode(def) <- "function"
    assign(name, def, pos = sessionData())## NOT S compatible!
    name
}

traceOff <-
  ## turn off tracing of this function
  function(what) {
    warning("'traceOff' is deprecated:  use the function 'untrace' in the 'methods' package instead")
    name <- what; def <- what
    if(is.function(name))
      name <- as.character(substitute(what))
    remove(list = name, pos=sessionData())
  }

browseAll <-
  ## browse the current stack of function calls.
  ##
  ## Uses the function `debugger' to set up browser calls
  ## on the frames.  On exit from that function, computation
  ## continues after the call to browseAll.  Computations done
  ## in the frames will have no effect.
  function() {
    dump.frames(".BrowserStack")
    debugger(.BrowserStack)
  }
