dump.frames <- function(dumpto = "last.dump", to.file = FALSE)
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    names(last.dump) <- limitedLabels(calls)
    last.dump <- last.dump[-length(last.dump)] # remove this function
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if(dumpto != "last.dump") assign(dumpto, last.dump)
    if (to.file) save(list=dumpto, file = paste(dumpto, "rda", sep="."))
    else assign(dumpto, last.dump, envir=.GlobalEnv)
    invisible()
}

debugger <- function(dump = last.dump)
{
    debugger.look <- function(.selection)
    {
        for(.obj in ls(envir=dump[[.selection]], all.names=TRUE))
            assign(.obj, get(.obj, envir=dump[[.selection]]))
        cat("Browsing in the environment with call:\n   ",
            calls[.selection], "\n", sep="")
        rm(.obj, .selection)
        browser()
    }
    if (class(dump) != "dump.frames") {
        cat("`dump' is not an object of class `dump.frames'\n")
        return(invisible())
    }
    err.action <- getOption("error")
    on.exit(options(error=err.action))
    if (length(msg <- attr(dump, "error.message")))
        cat("Message: ", msg)
    n <- length(dump)
    calls <- names(dump)
    repeat {
        cat("Available environments had calls:\n")
        cat(paste(1:n, ": ", calls,  sep=""), sep="\n")
        cat("\nEnter an environment number, or 0 to exit  ")
        repeat {
            ind <- .Internal(menu(as.character(calls)))
            if(ind <= n) break
        }
        if(ind == 0) return(invisible())
        debugger.look(ind)
    }
}

limitedLabels <- function(value, maxwidth = options()$width)
{
    value <- as.character(value)
    if(is.null(maxwidth) || maxwidth < 40)
        maxwidth <- 40
    if(any(nchar(value) > maxwidth)) {
        trim <- nchar(value) > maxwidth
        value[trim] <- substr(value[trim], 1, maxwidth)
    }
    value
}
      
recover <-
  function()
{
    ## find an interesting environment to dump from
    calls <- sys.calls()
    from <- 0
    n <- length(calls)
    if(identical(sys.function(n), recover))
        ## options(error=recover) produces a call to this function as an object
        n <- n - 1
    for(i in rev(seq(length=n))) {
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
            message("recover called non-interactively; frames dumped, use debugger() to view")
            return(NULL)
        }
        calls <- limitedLabels(calls[1:from])
        repeat {
            which <- menu(calls, title="\nEnter a frame number, or 0 to exit  ")
            if(which > 0)
                eval(quote(browser()), envir = sys.frame(which))
            else
                break
        }
    }
    else
        cat("No suitable frames for recover()\n")
}


trace <- function(what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE, signature = NULL) {
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
    needsAttach <- is.na(match("package:methods", search()))
    if(needsAttach) {
        cat("Tracing functions requires the methods package\n  (in rare cases this may change other results: see ?trace)\n")
        require(methods)
        on.exit(detach("package:methods")) ## in case of error
    }
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
    if(needsAttach)
        on.exit() ## no error
    what
}


untrace <- function(what, signature = NULL) {
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
            for(f in what)
                untrace(f, signature)
            return(what)
        }
    }
    if(is.na(match("package:methods", search())))
        return(.primUntrace(what)) ## can't have called trace exc. in primitive form
    if(is.null(signature)) {
        where <- findFunction(what)
        if(length(where) == 0)
            warning("No function \"", what, "\" to untrace")
        else {
            f <- getFunction(what)
            ## ensure that the version to assign is untraced (should be, but ...)
            if(is(f, "traceable")) {
                assign(what, .untracedFunction(f), where[[1]])
            }
            else
                .primUntrace(what) # to be safe--no way to know if it's traced or not
        }
    }
    else {
        where <- findMethod(what, signature)
        if(length(where) == 0)
            warning("No method for \"", what, "\" for this signature to untrace")
        else {
            where <- where[[1]]
            f <- getMethod(what, signature, where = where)
            if(is(f, "traceable"))
                setMethod(what, signature, .untracedFunction(f), where = where)
            else
                warning("The method for \"", what, "\" for this signature was not being traced")
        }
    }
}
        

