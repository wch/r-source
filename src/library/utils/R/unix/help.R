#  File src/library/utils/R/unix/help.R
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
    ofile <- file
    ## We need to use the version in per-session dir if we can.
    third_base_name <-
        file.path(basename(dirname(dirname(file))),
                  basename(dirname(file)),
                  basename(file))
    ## (Ouch.)
    lnkfile <-
        file.path(tempdir(), ".R", "library", third_base_name)
    if(any(ex <- file.exists(lnkfile))) {
        file <- lnkfile[ex][1L]          # could be more than one
    }
    if(file == ofile) {
        msg <- gettext("Using non-linked HTML file: hyperlinks may be incorrect")
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    file <- paste("file://", URLencode(file), sep = "")
    if(is.null(browser <- getOption("browser")))
        stop("options(\"browser\") not set")
    browseURL(file)
    if (is.character(browser)) {
        writeLines(c(paste("Help for", sQuote(topic),
                           "is shown in browser", browser, "..."),
                     "Use",
                     paste("\thelp(\"", topic, "\", htmlhelp = FALSE)",
                           sep = ""),
                     "or\n\toptions(htmlhelp = FALSE)\nto revert."))
    }
    return(invisible())
}

.show_help_on_topic_offline <-
function(file, topic)
{
    con <- tempfile()
    on.exit(unlink(con))
    cat("\\documentclass[",
        getOption("papersize"),
        "paper]{article}",
        "\n",
        "\\usepackage[",
        Sys.getenv("R_RD4DVI"),
        "]{Rd}",
        "\n",
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\begin{document}\n",
        file = con, sep = "")
    file.append(con, file)
    cat("\\end{document}\n",
        file = con, append = TRUE)
    ## FIXME: should we try 'latex' and 'dvips' here?
    if(!nzchar(getOption("latexcmd")))
        stop("'latexcmd' is empty")
    if(!nzchar(getOption("dvipscmd")))
        stop("'dvipscmd' is empty")
    Rtexmf <- file.path(R.home("share"), "texmf")
    system(paste("/bin/sh",
                 shQuote(file.path(R.home("share"), "sh", "help-print.sh")),
                 con,
                 topic,
                 shQuote(getOption("latexcmd")),
                 shQuote(getOption("dvipscmd")),
                 Rtexmf))
    return(invisible())
}
