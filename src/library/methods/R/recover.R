recover <-
  function()
{
    ## find an interesting environment to dump from
    calls <- sys.calls()
    frames <- sys.frames()
    from <- 0
    n <- length(calls)
    if(identical(languageEl(calls[[n]],1), recover))# should be sys.function() but has bug
        ## options(error=recover) produces a call to this function as an object
        n <- n - 1
    for(i in rev(seq(length=n))) {
        fname <- languageEl(calls[[i]],1)
        if(!is.name(fname) ||
           is.na(match(as.character(fname), c("recover", "stop", "Stop")))) {
            from <- i
            break
        }
    }
    if(from > 0) {
        dump.frames(".RecoverFrames")
        if(!interactive()) {
            message("Frames dumped to .RecoverFrames")
            return(NULL)
        }
        names(frames) <- sys.calls()    ## why does this work?
        flist <- as.list(frames)
        length(flist) <- from
        frames <- as.pairlist(flist)
        calls <- names(frames)
        repeat {
            which <- menu(calls, title="\nEnter a frame number, or 0 to exit  ")
            if(which > 0)
                eval(quote(browser()), envir = sys.frame(which))
            else
                break
        }
    }
    else
        message("No suitable frames for recover()")
}
