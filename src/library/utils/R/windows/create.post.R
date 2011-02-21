#  File src/library/utils/R/windows/create.post.R
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

bug.report.info <- function() {
    paste("R Version:\\n ",
		  paste(names(version), version, sep=" = ", collapse="\\n "),
                  "\\n\\n",
                  win.version(),
		  "\\n\\n",
                  "Locale:\\n",
                  Sys.getlocale(),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")
}

create.post <- function(instructions = "\\n",
                        description = "post",
                        subject = "",
                        ccaddress = getOption("ccaddress"),
                        method = getOption("mailer"),
                        address = "the relevant mailing list",
                        file = "R.post",
                        info = NULL)
{
    body <- paste(instructions,
		  "--please do not edit the information below--\\n\\n",
		  info,
		  bug.report.info(),
		  "\\n", sep="", collapse="")

    none_method <- function() {
        disclaimer <-
            paste("# R for Windows will not send your ", description,
                  " automatically.\n",
                  "# Please copy the ", description, " (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")

        cat(c(disclaimer, gsub("\\\\n", "\n", body)), file=file)
        file.edit(file)
        cat("The unsent ", description, " can be found in file\n",
            normalizePath(file), "\n", sep ="")
    }

    if (method == "none")
        none_method()
    else if (method == "mailto") {
        if (missing(address)) stop("must specify 'address'")
        if (!nzchar(subject)) subject <- "<<Enter Meaningful Subject>>"
        if(length(ccaddress) != 1L) stop("'ccaddress' must be of length 1")
        cat("The", description, "is being opened in your default mail program\nfor you to complete and send.\n")
        cmd <- paste("start \"title\" \"mailto:", address,
                     "?subject=", subject,
                     if(is.character(ccaddress) && nzchar(ccaddress))
                         paste("&cc=", ccaddress, sep=""),
                     "&body=", body0 <- gsub("\\\\n", "%0A", body),
                     sep = "")
 	tryCatch(shell(cmd, mustWork = TRUE), error = function(e) {
            cat("opening the mailer failed, so reverting to 'mailer=\"none\"'\n")
            none_method()
        })
    } else stop("unknown 'method'")
    invisible()
}

win.version <- function() .Internal(win.version())
