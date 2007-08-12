#  File src/library/base/R/unix/system.unix.R
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
    if(!missing(show.output.on.console) || !missing(minimized)
       || !missing(invisible))
        warning("arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only")

    if(ignore.stderr) command <- paste(command, "2>/dev/null")
    if(!is.null(input)) {
        if(!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        cat(input, file=f, sep="\n")
        command <- paste(command, "<", f)
    }
    if(!wait && !intern) command <- paste(command, "&")
    .Internal(system(command, intern))
}

Sys.which <- function(names)
{
    res <- character(length(names)); names(res) <- names
    for(i in names) {
        ans <- system(paste("which", i), intern=TRUE, ignore.stderr=TRUE)
        res[i] <- if(length(ans)) ans[1] else ""
    }
    res
}
