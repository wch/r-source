#  File src/library/base/R/windows/system.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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


.fixupGFortranStdout <- function()
{
    old <- Sys.getenv("GFORTRAN_STDOUT_UNIT")
    if (nzchar(old) && old == "-1") {
	Sys.unsetenv("GFORTRAN_STDOUT_UNIT")
	TRUE
    } else
	FALSE
}

.fixupGFortranStderr <- function()
{
    old <- Sys.getenv("GFORTRAN_STDERR_UNIT")
    if (nzchar(old) && old == "-1") {
	Sys.unsetenv("GFORTRAN_STDERR_UNIT")
	TRUE
    } else
	FALSE
}

system <- function(command, intern = FALSE,
                   ignore.stdout = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = TRUE, minimized = FALSE,
                   invisible = TRUE, timeout = 0)
{
    if(!is.logical(intern) || is.na(intern))
        stop("'intern' must be TRUE or FALSE")
    if(!is.logical(ignore.stdout) || is.na(ignore.stdout))
        stop("'ignore.stdout' must be TRUE or FALSE")
    if(!is.logical(ignore.stderr) || is.na(ignore.stderr))
        stop("'ignore.stderr' must be TRUE or FALSE")
    if(!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")
    if(!is.logical(show.output.on.console) || is.na(show.output.on.console))
        stop("'show.output.on.console' must be TRUE or FALSE")
    if(!is.logical(minimized) || is.na(minimized))
        stop("'minimized' must be TRUE or FALSE")
    if(!is.logical(invisible) || is.na(invisible))
        stop("'invisible' must be TRUE or FALSE")
    stdout <- ifelse(ignore.stdout, FALSE, "")
    stderr <- ifelse(ignore.stderr, FALSE, "")

    f <- ""
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        # cat(input, file = f, sep="\n")
        writeLines(input, f)
    }
    internNothing <- FALSE
    if (intern) {
        flag <- 3L
        ## documented to capture stderr also on Rgui
        if(stdout == "") stdout <- TRUE
        if(!ignore.stderr && .Platform$GUI == "Rgui") stderr <- TRUE
        if (identical(stdout, FALSE) &&
            (identical(stderr, FALSE) || .Platform$GUI != "Rgui")) {
            # could introduce: warning("intern = TRUE but ignoring outputs")
            flag <- 1L
            internNothing <- TRUE
        }
    } else {
        flag <- if (wait) {
            if (show.output.on.console && (!ignore.stderr || !ignore.stdout))
              2L
            else
              1L
        } else
            0L
    }
    if (invisible) flag <- 20L + flag
    else if (minimized) flag <- 10L + flag
    if (.fixupGFortranStdout())
	on.exit(Sys.setenv(GFORTRAN_STDOUT_UNIT = "-1"), add = TRUE)
    if (.fixupGFortranStderr())
	on.exit(Sys.setenv(GFORTRAN_STDERR_UNIT = "-1"), add = TRUE)
    rval <- .Internal(system(command, as.integer(flag), f, stdout, stderr, timeout))
    if (!internNothing)
        rval
    else {
        # NOTE: timeout warning will be different from normal intern
        ans <- character(0)
        if (is.numeric(rval) && length(rval)>0 && rval!=0) {
            rval <- as.integer(rval)
            attr(ans, "status") <- rval
            warning(gettextf("running command '%s' had status %d", command,
                             rval), domain = NA)
        }
        ans
    }
}

system2 <- function(command, args = character(),
                    stdout = "", stderr = "", stdin = "", input = NULL,
                    env = character(),
                    wait = TRUE, minimized = FALSE, invisible = TRUE,
                    timeout = 0)
{
    if(!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")
    if(!is.logical(minimized) || is.na(minimized))
        stop("'minimized' must be TRUE or FALSE")
    if(!is.logical(invisible) || is.na(invisible))
        stop("'invisible' must be TRUE or FALSE")
    command <- paste(c(shQuote(command), env, args), collapse = " ")

    if(is.null(stdout)) stdout <- FALSE
    if(is.null(stderr)) stderr <- FALSE
    
    if(length(stdout) != 1L) stop("'stdout' must be of length 1")
    if(length(stderr) != 1L) stop("'stderr' must be of length 1")
    
    if (!is.null(input)) {
        f <- tempfile()
        on.exit(unlink(f))
        # cat(input, file = f, sep="\n")
        writeLines(input, f)
    } else f <- stdin

    rf <- NULL
    if (.Platform$GUI == "Rgui") {
        # .Internal(system) on RGui needs a pipe to print to the console,
        # but it cannot use it at the same time to capture the output, so
        # we capture the output to be returned via redirection to a file
        if (isTRUE(stdout) && identical(stderr,"")) {
            rf <- tempfile()
            on.exit(unlink(rf), add = TRUE)
            stdout <- rf
            wait <- TRUE
        } else if (isTRUE(stderr) && identical(stdout,"")) {
            rf <- tempfile()
            on.exit(unlink(rf), add = TRUE)
            stderr <- rf
            wait <- TRUE
        }
    }
    flag <- if (isTRUE(stdout) || isTRUE(stderr)) 3L
    else if (wait)
        ifelse(identical(stdout, "") || identical(stderr, ""), 2L, 1L)
    else 0L
    if (invisible) flag <- 20L + flag
    else if (minimized) flag <- 10L + flag
    if (.fixupGFortranStdout())
	on.exit(Sys.setenv(GFORTRAN_STDOUT_UNIT = "-1"), add = TRUE)
    if (.fixupGFortranStderr())
	on.exit(Sys.setenv(GFORTRAN_STDERR_UNIT = "-1"), add = TRUE) 
    rval <- .Internal(system(command, flag, f, stdout, stderr, timeout))

    if (is.null(rf)) {
        if (is.integer(rval))
	    invisible(rval)
	else
	    rval
    } else {
        # NOTE: timeout warning will be different from normal intern
        ans <- readLines(rf)
        if (is.numeric(rval) && length(rval)>0 && rval!=0) {
            rval <- as.integer(rval)
            attr(ans, "status") <- rval
            warning(gettextf("running command '%s' had status %d", command,
                             rval), domain = NA)
        }
        ans
    }
}

shell <- function(cmd, shell, flag = "/c", intern = FALSE,
                  wait = TRUE, translate = FALSE, mustWork = FALSE, ...)
{
    if(missing(shell)) {
        shell <- Sys.getenv("R_SHELL")
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
    if(!intern && res && !is.na(mustWork))
        if(mustWork)
            if(res == -1L)
                stop(gettextf("'%s' could not be run", cmd0), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d",
                               cmd0, res), domain = NA)
        else
            if(res == -1L)
                warning(gettextf("'%s' could not be run", cmd0), domain = NA)
            else warning(gettextf("'%s' execution failed with error code %d",
                                  cmd0, res), domain = NA)
    if(intern) res else invisible(res)
}

shell.exec <- function(file) .Internal(shell.exec(file))

## Sys.timezone() --> common function for all platforms

Sys.which <- function(names) .Internal(Sys.which(as.character(names)))
