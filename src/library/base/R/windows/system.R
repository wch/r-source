system <- function(command, intern = FALSE, wait = TRUE, input = NULL,
                   show.output.on.console = FALSE, minimized = FALSE,
                   invisible = FALSE)
{
    f <- ""
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        cat(input,file=f,sep="\n")
    }
    if (intern)
        flag <- 3
    else {
        if  (wait)
            flag <- ifelse(show.output.on.console, 2, 1)
        else
            flag <- 0
    }
    if (invisible) flag <- 20 + flag
    else if (minimized) flag <- 10 + flag
    .Internal(system(command, as.integer(flag), f))
}

unix <- function(call, intern = FALSE)
{
    .Deprecated("system")
    system(call, intern)
}

unlink <- function(x, recursive=FALSE)
    invisible(.Internal(unlink(x, recursive)))

shell <- function(cmd, shell, flag="/c", intern=FALSE,
                  wait=TRUE, translate=FALSE, mustWork=FALSE, ...)
{
    if(missing(shell)) {
        shell <- Sys.getenv("R_SHELL")
        if(!nchar(shell)) shell <- Sys.getenv("SHELL")
        if(!nchar(shell)) shell <- Sys.getenv("COMSPEC")
    }
    if(missing(flag) && any(!is.na(pmatch(c("bash", "tcsh"), basename(shell)))))
        flag <- "-c"
    if(translate) cmd <- chartr("/", "\\", cmd)
    if(!is.null(shell)) cmd <- paste(shell, flag, cmd)
    res <- system(cmd, intern=intern, wait=wait | intern,
                  show.output.on.console=wait, ...)
    if(!intern && res !=0)
        if(mustWork)
            if(res == -1) stop("'cmd' could not be run")
            else stop(gettextf("'cmd' execution failed with error code %d",
                               res), domain = NA)
        else
            if(res == -1) warning("'cmd' could not be run")
            else warning(gettextf("'cmd' execution failed with error code %d",
                                  res), domain = NA)
    if(intern) res else invisible(res)
}

shell.exec <- function(file) invisible(.Internal(shell.exec(file)))

Sys.timezone <- function()
{
    z <- as.POSIXlt(Sys.time())
    zz <- attr(z, "tzone")
    if(length(zz) == 3) zz[2 + z$isdst] else zz[1]
}

