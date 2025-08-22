#  File src/library/utils/R/code2html.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2022 The R Core Team
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

## Similar in spirit to example() and demo(), but instead of running
## in the console, run it through knitr to produce HTML output.


## Decisions to be made:
## - knitr opts to use; being minimal lets users decide
## - whether to evaluate globally or locally

.code2html_payload_console <- function(type, topic, package, enhancedHTML = TRUE, Rhome = "")
{
    snippet <- sprintf("%s(%s, package = \"%s\")", type, topic, package)
    msg <- c(gettextf("<p><code class='language-R'>%s</code> was run in the console.</p>", snippet),
             if (enhancedHTML)
                 gettext("<p>To view output in the browser, the <a href='https://CRAN.R-project.org/package=knitr'>knitr</a> package must be installed.</p>")
             else
                 gettext("<p>To view output in the browser, the <a href='https://CRAN.R-project.org/package=knitr'>knitr</a> package must be installed and the environment variable <code>_R_HELP_ENABLE_ENHANCED_HTML_</code> must be set to TRUE.</p>"))
    list(payload = paste(c(HTMLheader("R example", Rhome = Rhome,
                                      logo = FALSE, up = NULL, top = NULL),
                           msg, "\n</div></body></html>"),
                         collapse = "\n"))
}

.code2html_payload_browser <- function(type, codelines, topic, package,
                                       Rhome = "", header.info = NULL,
                                       env = NULL)
{
    if (type == "example" && package != "base") library(package, character.only = TRUE)
    header.lines <-
        if (is.null(header.info)) character(0)
        else # Assumes Name, Title, Aliases present without checking (which should be OK)
        {
            header.info$Aliases <- strsplit(header.info$Aliases, "[[:space:]]+")[[1]]
            header.info$Keywords <- if (is.null(header.info$Keywords)) character(0)
                                    else strsplit(header.info$Keywords, "[[:space:]]+")[[1]]
            with(header.info,
            {
                ## Note: sprintf() return 0-length output with 0-length input
                c(sprintf("<h2>%s</h2>", Title),
                  sprintf("<p>Aliases: %s</p>",
                          paste(sprintf("<a href='../help/%s'>%s</a>",
                                        vapply(Aliases, urlify, "",
                                               reserved = TRUE),
                                        vapply(Aliases, shtmlify, "")),
                                collapse = " ")),
                  sprintf("<p>Keywords: %s</p>",
                          paste(sprintf("<a href='/doc/html/Search?category=%s'>%s</a>",
                                        Keywords, Keywords),
                                collapse = " "))
                  )
            })
        }
    ## Not really important, but to be consistent with help pages
    pkgversion <- utils::packageDescription(package, fields = "Version")
    footer.lines <-
        sprintf('<hr><div style="text-align: center;">[Package <em>%s</em> version %s <a href="../html/00Index.html">Index</a>]</div>', package, pkgversion)
    rhtml <-
        c(HTMLheader(title = sprintf("%s '%s::%s'",
                                     switch(type, demo = "Demo for", example = "Examples for"),
                                     package, topic),
                     Rhome = Rhome,
                     logo = FALSE, up = NULL, top = NULL),
          header.lines,
          "\n\n<!--begin.rcode\n",
          codelines,
          "\nend.rcode-->\n\n",
          footer.lines,
          "</div></body></html>")
    figdir <- tempfile(pattern = package, fileext = topic)
    on.exit(unlink(figdir, recursive = TRUE), add = TRUE)
    ## Record old knitr / chunk options and restore on exit
    old_opts_knit <- knitr::opts_knit$get()
    old_opts_chunk <- knitr::opts_chunk$get()
    on.exit(knitr::opts_knit$restore(old_opts_knit), add = TRUE)
    on.exit(knitr::opts_chunk$restore(old_opts_chunk), add = TRUE)
    knitr::opts_knit$set(upload.fun = function(x) paste0("data:", mime_type(x), ";base64,", xfun::base64_encode(x)),
                         unnamed.chunk.label = sprintf("%s-%s-%s", type, package, topic))
    knitr::opts_chunk$set(comment = "", warning = TRUE, message = TRUE, error = TRUE,
                          fig.path = file.path(figdir, "fig-"),
                          fig.width = 9, fig.height = 7,
                          dpi = 96)
    out <- knitr::knit(text = rhtml, quiet = TRUE,
                       envir = env %||% new.env(parent = .GlobalEnv))
    ## the paste() doesn't seem necessary, but just to be safe
    list(payload = paste(out, collapse = "\n"))
}

example2html <- function(topic, package, Rhome = "", env = NULL)
{
    ## topic must be character (no NSE), and package must be specified
    enhancedHTML <-
        config_val_to_logical(Sys.getenv("_R_HELP_ENABLE_ENHANCED_HTML_", "TRUE"))
    if (!enhancedHTML || !requireNamespace("knitr", quietly = TRUE)) {
        ## Don't display in HTML (run in console instead)
        utils::example(topic, package = package, character.only = TRUE, ask = FALSE)
        .code2html_payload_console("example", topic, package,
                                   enhancedHTML = enhancedHTML, Rhome = Rhome)
    }
    else {
        ecode <- utils::example(topic, package = package, character.only = TRUE, give.lines = TRUE)
        ## Parse initial lines starting with ###
        hlines <- grep("^###[ ][^*]", ecode)
        wskip <- which(diff(hlines) != 1)
        if (length(wskip)) hlines <- hlines[seq_len(wskip[1])]
        if (length(hlines))
        {
            header.info <-
                as.list(read.dcf(textConnection(substring(ecode[hlines], 5)))[1, , drop = TRUE])
            ecode <- ecode[-hlines]
        }
        else header.info <- NULL
        .code2html_payload_browser("example", ecode, topic, package,
                                   Rhome = Rhome, header.info = header.info,
                                   env = env)
    }
}

demo2html <- function(topic, package, Rhome = "", env = NULL)
{
    enhancedHTML <-
        config_val_to_logical(Sys.getenv("_R_HELP_ENABLE_ENHANCED_HTML_", "TRUE"))
    if (!enhancedHTML || !requireNamespace("knitr", quietly = TRUE)) {
        ## Don't display in HTML (run in console instead)
        utils::demo(topic, package = package, character.only = TRUE, ask = FALSE)
        .code2html_payload_console("demo", topic, package, enhancedHTML = enhancedHTML, Rhome = Rhome)
    }
    else {
        ## Assumes that demo file is names topic.R
        dcode <- readLines(system.file("demo", paste0(topic, ".R"), package = package))
        .code2html_payload_browser("demo", dcode, topic, package, Rhome = Rhome, env = env)
    }
}

