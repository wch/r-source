#  File src/library/base/R/unix/system.unix.R
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

system <- function(command, intern = FALSE,
                   ignore.stdout = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = TRUE, minimized = FALSE,
                   invisible = TRUE)
{
    if(!missing(show.output.on.console) || !missing(minimized)
       || !missing(invisible))
        message("arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only")

    if(!is.logical(intern) || is.na(intern))
        stop("'intern' must be TRUE or FALSE")
    if(!is.logical(ignore.stdout) || is.na(ignore.stdout))
        stop("'ignore.stdout' must be TRUE or FALSE")
    if(!is.logical(ignore.stderr) || is.na(ignore.stderr))
        stop("'ignore.stderr' must be TRUE or FALSE")
    if(!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")

    if(ignore.stdout) command <- paste(command, ">/dev/null")
    if(ignore.stderr) command <- paste(command, "2>/dev/null")
    if(!is.null(input)) {
        if(!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
##        command <- paste(command, "<", shQuote(f))
        ## change to use shell-execution-environment redirection, PR#15508
        command <- paste("<", shQuote(f), command)
    }
    if(!wait && !intern) command <- paste(command, "&")
    .Internal(system(command, intern))
}

system2 <- function(command, args = character(),
                    stdout = "", stderr = "", stdin = "", input = NULL,
                    env = character(),
                    wait = TRUE, minimized = FALSE, invisible = TRUE)
{
    if(!missing(minimized) || !missing(invisible))
        message("arguments 'minimized' and 'invisible' are for Windows only")
    if(!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")

    intern <- FALSE
    command <- paste(c(env, shQuote(command), args), collapse = " ")

    if(is.null(stdout)) stdout <- FALSE
    if(is.null(stderr))
	stderr <- FALSE
    else if (isTRUE(stderr)) {
        if (!isTRUE(stdout)) warning("setting stdout = TRUE")
        stdout <- TRUE
    }
    if (identical(stdout, FALSE))
        command <- paste(command, ">/dev/null")
    else if(isTRUE(stdout))
        intern <- TRUE
    else if(is.character(stdout)) {
        if(length(stdout) != 1L) stop("'stdout' must be of length 1")
        if(nzchar(stdout)) {
            command <- if (identical(stdout, stderr))
		paste (command, ">", shQuote(stdout), "2>&1")
	    else paste(command, ">", shQuote(stdout))
        }
    }
    if (identical(stderr, FALSE))
        command <- paste(command, "2>/dev/null")
    else if(isTRUE(stderr)) { # stdout == TRUE
        command <- paste(command, "2>&1")
    } else if(is.character(stderr)) {
        if(length(stderr) != 1L) stop("'stderr' must be of length 1")
        if(nzchar(stderr) && !identical(stdout, stderr))
            command <- paste(command, "2>", shQuote(stderr))
    }
    if(!is.null(input)) {
        if(!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
        ## here 'command' is a single command, unlike system()
        command <- paste(command, "<", shQuote(f))
    } else if (nzchar(stdin)) command <- paste(command, "<", stdin)
    if(!wait && !intern) command <- paste(command, "&")
    .Internal(system(command, intern))
}

## Some people try to use this with NA inputs (PR#15147)
Sys.which <- function(names)
{
    res <- character(length(names)); names(res) <- names
    ## hopefully configure found [/usr]/bin/which
    which <- "@WHICH@"
    if (!nzchar(which)) {
        warning("'which' was not found on this platform")
        return(res)
    }
    for(i in seq_along(names)) {
        if(is.na(names[i])) {res[i] <- NA; next}
        ## Quoting was added in 3.0.0
        ans <- suppressWarnings(system(paste(which, shQuote(names[i])),
                                       intern = TRUE, ignore.stderr = TRUE))
        ## Solaris' which gives 'no foo in ...' message on stdout,
        ## GNU which does it on stderr
        if(grepl("solaris", R.version$os)) {
            tmp <- strsplit(ans[1], " ", fixed = TRUE)[[1]]
            if(identical(tmp[1:3], c("no", i, "in"))) ans <- ""
        }
        res[i] <- if(length(ans)) ans[1] else ""
        ## final check that this is a real path and not an error message
        if(!file.exists(res[i])) res[i] <- ""
    }
    res
}
