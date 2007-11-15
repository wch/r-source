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
        file <- lnkfile[ex][1]          # could be more than one
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
    ## <FIXME>
    ## We now have help-print.sh in share/sh but we do not use the
    ## .Script mechanism because we play with the TEXINPUTS environment
    ## variable and the code goes back to a time when not all systems
    ## could be assumed to support Sys.setenv().
    ## Seems that now we can---rewrite this along the lines of
    ## tools:::.install_package_vignettes().
    system(paste(paste("TEXINPUTS=",
                       file.path(R.home("share"), "texmf"),
                       ":",
                       "$TEXINPUTS",
                       sep = ""),
                 "/bin/sh",
                 shQuote(file.path(R.home("share"), "sh", "help-print.sh")),
                 con,
                 topic,
                 getOption("latexcmd"),
                 getOption("dvipscmd")))
    ## </FIXME>
    return(invisible())
}
