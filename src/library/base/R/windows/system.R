#  File src/library/base/R/windows/system.R
#  Part of the R package, http://www.R-project.org
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

system <- function(command, intern = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = TRUE, minimized = FALSE,
                   invisible = TRUE)
{
    f <- ""
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        cat(input, file = f, sep="\n")
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
    if(ignore.stderr) flag <- flag + 100
    .Internal(system(command, as.integer(flag), f))
}

shell <- function(cmd, shell, flag = "/c", intern = FALSE,
                  wait = TRUE, translate = FALSE, mustWork = FALSE, ...)
{
    if(missing(shell)) {
        shell <- Sys.getenv("R_SHELL")
        if(!nzchar(shell)) shell <- Sys.getenv("SHELL")
        if(!nzchar(shell)) shell <- Sys.getenv("COMSPEC")
    }
    if(missing(flag) &&
       any(!is.na(pmatch(c("bash", "tcsh", "sh"), basename(shell)))))
        flag <- "-c"
    cmd0 <- cmd # for messages
    if(translate) cmd <- chartr("/", "\\", cmd)
    if(!is.null(shell)) cmd <- paste(shell, flag, cmd)
    res <- system(cmd, intern = intern, wait = wait | intern,
                  show.output.on.console = wait, ...)
    if(!intern && res !=0 && !is.na(mustWork))
        if(mustWork)
            if(res == -1)
                stop(gettextf("'%s' could not be run", cmd0), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d",
                               cmd0, res), domain = NA)
        else
            if(res == -1)
                warning(gettextf("'%s' could not be run", cmd0), domain = NA)
            else warning(gettextf("'%s' execution failed with error code %d",
                                  cmd0, res), domain = NA)
    if(intern) res else invisible(res)
}

shell.exec <- function(file) invisible(.Internal(shell.exec(file)))

Sys.timezone <- function()
{
    z <- as.POSIXlt(Sys.time())
    zz <- attr(z, "tzone")
    if(length(zz) == 3) zz[2 + z$isdst] else zz[1]
}

Sys.which <- function(names) .Internal(Sys.which(as.character(names)))
