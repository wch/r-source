dump.frames <- function(dumpto = "last.dump", to.file = FALSE)
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    names(last.dump) <- limitedLabels(calls)
    last.dump <- last.dump[-length(last.dump)] # remove this function
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if(dumpto != "last.dump") assign(dumpto, last.dump)
    if (to.file) # compress=TRUE is now the default.
        save(list=dumpto, file = paste(dumpto, "rda", sep="."))
    else assign(dumpto, last.dump, envir=.GlobalEnv)
    invisible()
}

debugger <- function(dump = last.dump)
{
    debugger.look <- function(.selection)
    {
        for(.obj in ls(envir=dump[[.selection]], all.names=TRUE))
            assign(.obj, get(.obj, envir=dump[[.selection]]))
        cat(gettext("Browsing in the environment with call:\n   "),
            calls[.selection], "\n", sep="")
        rm(.obj, .selection)
        browser()
    }
    if (class(dump) != "dump.frames") {
        cat(gettext("'dump' is not an object of class 'dump.frames'\n"))
        return(invisible())
    }
    err.action <- getOption("error")
    on.exit(options(error=err.action))
    if (length(msg <- attr(dump, "error.message")))
        cat(gettext("Message: "), msg)
    n <- length(dump)
    calls <- names(dump)
    repeat {
        cat(gettext("Available environments had calls:\n"))
        cat(paste(1:n, ": ", calls,  sep=""), sep="\n")
        cat(gettext("\nEnter an environment number, or 0 to exit  "))
        repeat {
            ind <- .Internal(menu(as.character(calls)))
            if(ind <= n) break
        }
        if(ind == 0) return(invisible())
        debugger.look(ind)
    }
}

## allow for the numbering by menu here
limitedLabels <- function(value, maxwidth = getOption("width") - 5)
{
    value <- as.character(value)
    if(is.null(maxwidth) || maxwidth < 40)
        maxwidth <- 40
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
    from <- 0
    n <- length(calls)
    if(identical(sys.function(n), recover))
        ## options(error=recover) produces a call to this function as an object
        n <- n - 1
    ## look for a call inserted by trace() (and don't show frames below)
    ## this level.
    for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1]]
        ## deparse can use more than one line
        if(!is.na(match(deparse(fname)[1],
                        c("methods::.doTrace", ".doTrace")))) {
            from <- i-1
            break
        }
    }
  ## if no trace, look for the first frame from the bottom that is not
    ## stop or recover
    if(from == 0)
      for(i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1]]
        if(!is.name(fname) ||
           is.na(match(as.character(fname), c("recover", "stop", "Stop")))) {
            from <- i
            break
        }
    }
    if(from > 0) {
        if(!interactive()) {
            try(dump.frames())
            cat(gettext("recover called non-interactively; frames dumped, use debugger() to view\n"))
            return(NULL)
        }
        else if(identical(getOption("show.error.messages"), FALSE)) # from try(silent=TRUE)?
            return(NULL)
        calls <- limitedLabels(calls[1:from])
        repeat {
            which <- menu(calls,
                          title="\nEnter a frame number, or 0 to exit  ")
            if(which > 0)
                eval(quote(browser()), envir = sys.frame(which))
            else
                break
        }
    }
    else
        cat(gettext("No suitable frames for recover()\n"))
}
