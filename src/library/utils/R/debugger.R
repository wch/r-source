#  File src/library/utils/R/debugger.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
#  https://www.R-project.org/Licenses/

dump.frames <- function(dumpto = "last.dump", to.file = FALSE,
                        include.GlobalEnv = FALSE)
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    names(last.dump) <- limitedLabels(calls)
    if (include.GlobalEnv) {
	## include a copy of (and not just a reference to) .GlobalEnv in the dump
        ## cp_envir(EE) := as.environment(as.list(EE, all.names=TRUE))
	last.dump <- c(".GlobalEnv" =
                           as.environment(as.list(.GlobalEnv, all.names = TRUE)),
		       last.dump)
    }
    last.dump <- last.dump[-length(last.dump)] # remove this function
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if(dumpto != "last.dump") assign(dumpto, last.dump)
    if (to.file) # compress=TRUE is now the default.
        save(list=dumpto, file = paste0(dumpto, ".rda"))
    else assign(dumpto, last.dump, envir=.GlobalEnv)
    invisible()
}

debugger <- function(dump = last.dump)
{
    debugger.look <- function(.selection)
    {
        ## allow e.g. '...' to fail
        for(.obj in ls(envir=dump[[.selection]], all.names=TRUE))
            tryCatch(assign(.obj, get(.obj, envir=dump[[.selection]])),
                     error=function(e) {})
        cat(gettext("Browsing in the environment with call:\n   "),
            calls[.selection], "\n", sep = "")
        rm(.obj, .selection)
        browser()
    }
    if (!inherits(dump, "dump.frames")) {
        cat(gettextf("'dump' is not an object of class %s\n",
                     dQuote("dump.frames")))
        return(invisible())
    }
    err.action <- getOption("error")
    on.exit(options(error=err.action))
    if (length(msg <- attr(dump, "error.message")))
        cat(gettext("Message: "), msg)
    n <- length(dump)
    if (!n) {
	cat(gettextf("'dump' is empty\n"))
	return(invisible())
    }
    calls <- names(dump)
    repeat {
        cat(gettext("Available environments had calls:\n"))
        cat(paste0(1L:n, ": ", calls), sep = "\n")
        cat(gettext("\nEnter an environment number, or 0 to exit  "))
        repeat {
            ind <- .Call(C_menu, as.character(calls))
            if(ind <= n) break
        }
        if(ind == 0L) return(invisible())
        debugger.look(ind)
    }
}

## allow for the numbering by menu here
limitedLabels <- function(value, maxwidth = getOption("width") - 5L)
{
    srcrefs <- sapply(value, function(v)
                      if (!is.null(srcref <- attr(v, "srcref"))) {
                          srcfile <- attr(srcref, "srcfile")
                          paste0(basename(srcfile$filename), "#", srcref[1L],": ")
                      } else "")
    value <- paste0(srcrefs, as.character(value))
    if(is.null(maxwidth) || maxwidth < 40L) maxwidth <- 40L
    maxwidth <- min(maxwidth, 1000L)
    strtrim(value, maxwidth)
}

recover <-
  function()
{
    if(.isMethodsDispatchOn()) {
        ## turn off tracing
        tState <- tracingState(FALSE)
        on.exit(tracingState(tState))
    }
    ## find an interesting environment to start from
    calls <- sys.calls()
    from <- 0L
    n <- length(calls)
    if(identical(sys.function(n), recover))
        ## options(error=recover) produces a call to this function as an object
        n <- n - 1L
    ## look for a call inserted by trace() (and don't show frames below)
    ## this level.
    for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1L]]
        ## deparse can use more than one line
        if(!is.na(match(deparse(fname)[1L],
                        c("methods::.doTrace", ".doTrace")))) {
            from <- i-1L
            break
        }
    }
  ## if no trace, look for the first frame from the bottom that is not
    ## stop or recover
    if(from == 0L)
      for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1L]]
        if(!is.name(fname) ||
           is.na(match(as.character(fname), c("recover", "stop", "Stop")))) {
            from <- i
            break
        }
    }
    if(from > 0L) {
        if(!interactive()) {
            try(dump.frames())
            cat(gettext("recover called non-interactively; frames dumped, use debugger() to view\n"))
            return(NULL)
        }
        else if(isFALSE(getOption("show.error.messages"))) # from try(silent=TRUE)?
            return(NULL)
        calls <- limitedLabels(calls[1L:from])
        repeat {
            which <- menu(calls,
                          title="\nEnter a frame number, or 0 to exit  ")
            if(which)
                eval(substitute(browser(skipCalls=skip),
                                list(skip=7-which)), envir = sys.frame(which))
            else
                break
        }
    }
    else
        cat(gettext("No suitable frames for recover()\n"))
}
