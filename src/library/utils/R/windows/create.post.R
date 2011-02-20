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
                        ccaddress = Sys.getenv("USER"),
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

    if (method == "none") {
        disclaimer <-
            paste("# R for Windows will not send your ", description,
                  " automatically.\n",
                  "# Please copy the ", description, " (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")

        cat(disclaimer, file=file)
        body <- gsub("\\\\n", "\n", body)
        cat(body, file=file, append=TRUE)
        file.edit(file)
        cat("The unsent ", description, " can be found in file\n",
            normalizePath(file), "\n", sep ="")
    } else if (method == "mailto") {
        if (!nzchar(subject)) subject <- "<<Enter Meaningful Subject>>"
	body0 <- gsub("\\\\n", "%0A", body)
        cat("The", description, "is being opened for you to edit and send.\n")
 	tryCatch(shell(paste("start \"title\" \"mailto:", address,
                             "?subject=", subject, "&body=", body0,
                             sep = "")),
                 error = function(e) {
                     cat(body, file=file, append=TRUE)
                     file.edit(file)
                     cat("The unsent ", description, " can be found in file\\n",
                         normalizePath(file), "\n", sep ="")
                 })
    } else stop("unknown 'method'")
    invisible()
}

win.version <- function() .Internal(win.version())
