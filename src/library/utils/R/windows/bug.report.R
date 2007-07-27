#  File src/library/utils/R/windows/bug.report.R
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

bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report")
{
    body <- paste("\\n<<insert bug report here>>\\n\\n\\n\\n",
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
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

    disclaimer <-
        paste("# R for Windows will not send your bug report automatically.\n",
              "# Please copy the bug report (after finishing it) to\n",
              "# your favorite email program and send it to\n#\n",
              "#       ", address, "\n#\n",
              "######################################################\n",
              "\n\n", sep = "")

    cat(disclaimer, file=file)
    body <- gsub("\\\\n", "\n", body)
    cat(body, file=file, append=TRUE)
    file.edit(file)
    cat("The unsent bug report can be found in file", tools::file_path_as_absolute(file), "\n")
    invisible()
}

win.version <- function() .Internal(win.version())
