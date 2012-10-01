#  File src/library/methods/R/Session.R
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

## functions to support a session frame and session-scope tracing
## of function calls in R similar to the S-Plus (S4) trace, untrace, browser functions.

sessionData <-
  ## return the index of the session data in the search list,
  ## attaching it if it is not attached
  function() {
      .Deprecated()
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
    .Deprecated("methods::trace")
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
    .Deprecated("methods::untrace")
    name <- what; def <- what
    if(is.function(name))
      name <- as.character(substitute(what))
    remove(list = name, pos=sessionData())
  }

utils::globalVariables(".BrowserStack")
browseAll <-
  ## browse the current stack of function calls.
  ##
  ## Uses the function `debugger' to set up browser calls
  ## on the frames.  On exit from that function, computation
  ## continues after the call to browseAll.  Computations done
  ## in the frames will have no effect.
  function() {
      .Deprecated()
      utils::dump.frames(".BrowserStack")
      utils::debugger(.BrowserStack)
  }
