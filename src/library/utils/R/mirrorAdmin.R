#  File src/library/utils/R/mirrorAdmin.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

mirror2html <-
function(mirrors = NULL, file = "mirrors.html",
         head = "mirrors-head.html", foot = "mirrors-foot.html")
{
    if(is.null(mirrors)){
        mirrors <- getCRANmirrors(all = FALSE, local.only = TRUE)
    }
    mirrors$Host <- gsub("&", "&amp;", mirrors$Host)
    z <- NULL
    if(file.exists(head)) z <- readLines(head)
    z <- c(z, "<dl>")
    for(country in unique(mirrors$Country)) {
        m <- mirrors[mirrors$Country == country,]
        z <- c(z, paste0("<dt>", country, "</dt>"),
               "<dd>",
               sprintf("<table border=0 width=\"90%%\" summary=\"%s\">",
                       country))
        for(k in seq_len(nrow(m))) {
            z <- c(z, "<tr>",
                   "<td width=\"45%\">",
                   sprintf("<a href=\"%s\" target=\"_top\">%s</a>",
                           m$URL[k], m$URL[k]),
                   "</td>\n",
                   "<td>", m$Host[k], "</td>",
                   "</tr>")
        }
        z <- c(z, "</table>", "</dd>")
    }
    z <- c(z, "</dl>")
    if(file.exists(foot)) z <- c(z, readLines(foot))
    if(file!="") writeLines(z, file)
    invisible(z)
}

checkCRAN <-
function(method)
{
    master <- available.packages(contrib.url("http://CRAN.R-project.org"),
                                 method = method)
    m <- getCRANmirrors()
    z <- list()
    for(url in as.character(m$URL))
        z[[url]] <- available.packages(contrib.url(url), method = method)
    lapply(z, function(a) all.equal(a, master))
}
