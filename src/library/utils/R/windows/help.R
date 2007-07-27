#  File src/library/utils/R/windows/help.R
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

.show_help_on_topic_as_HTML <-
function(file, topic)
{
    ## Uwe Ligges reported this as needed on his system (PR#7269)
    browseURL(chartr("/", "\\", file))
    writeLines(paste("Help for", sQuote(topic), "is shown in the browser"))
    return(invisible())
}

.show_help_on_topic_offline <-
function(file, topic)
{
    FILE <- "Rdoc" # must be in the current dir
    con <- paste(FILE, ".tex", sep = "")
    cat("\\documentclass[",
        getOption("papersize"),
        "paper]{article}",
        "\n",
        "\\usepackage[",
        if(nzchar(opt <- Sys.getenv("R_RD4DVI"))) opt else "ae",
        "]{Rd}",
        "\n",
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\begin{document}\n",
        file = con, sep = "")
    file.append(con, file)
    cat("\\end{document}\n", file = con, append = TRUE)
    cmd <- paste('"',
                 paste(R.home("bin"), "helpPRINT", sep="/"),
                 '"', sep="")
    texpath <- chartr("\\", "/",
                      file.path(R.home("share"), "texmf"))
    system(paste(cmd, FILE, topic, texpath), wait = FALSE)
    return(invisible())
}
