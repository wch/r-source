#  File src/library/utils/R/windows/create.post.R
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

create.post <- function(instructions = character(),
                        description = "post",
                        subject = "",
                        method = getOption("mailer"),
                        address = "the relevant mailing list",
                        ccaddress = getOption("ccaddress", ""),
                        filename = "R.post",
                        info = character())
{
    method <-
	if(is.null(method)) "none"
	else match.arg(method, c("mailto", "mailx", "gnudoit", "none", "ess"))

    body <- c(instructions,
              "--please do not edit the information below--", "",
              info)

    none_method <- function() {
        disclaimer <-
            paste0("# Your mailer is set to \"none\",\n",
                   "# hence we cannot send the, ", description, " directly from R.\n",
                   "# Please copy the ", description, " (after finishing it) to\n",
                   "# your favorite email program and send it to\n#\n",
                   "#       ", address, "\n#\n",
                   "######################################################\n",
                   "\n\n")

        cat(c(disclaimer, body), file = filename, sep = "\n")
        cat("The", description, "is being opened for you to edit.\n")
        flush.console()
        file.edit(filename)
        cat("The unsent ", description, " can be found in file\n",
            normalizePath(filename), "\n", sep ="")
    }

    if (method == "none")
        none_method()
    else if(method == "ess")
	cat(body, sep = "\n")
    else if(method == "gnudoit") {
        ## FIXME: insert subject and ccaddress
	cmd <- paste0("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", paste(body, collapse = "\\n"), "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'")
	system(cmd)
    } else if (method == "mailto") {
        if (missing(address)) stop("must specify 'address'")
        if (!nzchar(subject)) subject <- "<<Enter Meaningful Subject>>"
        if(length(ccaddress) != 1L) stop("'ccaddress' must be of length 1")
        cat("The", description, "is being opened in your default mail program\nfor you to complete and send.\n")
        uri <-  paste0("mailto:", address,
                     "?subject=", subject,
                     if(is.character(ccaddress) && nzchar(ccaddress))
                         paste0("&cc=", ccaddress),
                     "&body=", paste(body, collapse="\r\n"))
        tryCatch(shell.exec(URLencode(uri)), error = function(e) {
            cat("opening the mailer failed, so reverting to 'mailer=\"none\"'\n")
            flush.console()
            Sys.sleep(5)
            none_method()
        })
    } else if(method == "mailx")
	stop("method 'mailx' is Unix-only")

    invisible()
}
