#  File src/library/utils/R/mirrorAdmin.R
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

mirror2html <- function(mirrors = NULL, file="mirrors.html",
                        head = "mirrors-head.html",
                        foot = "mirrors-foot.html")
{
    if(is.null(mirrors)){
        mirrors <- getCRANmirrors(all=FALSE, local.only=TRUE)
    }

    z <- NULL
    if(file.exists(head))
        z <- readLines(head)

    z <- c(z, "<dl>")

    for(country in unique(mirrors$Country)){
        m = mirrors[mirrors$Country==country,]
        z <- c(z, paste("<dt>", country, "</dt>", sep=""),
               "<dd>",
               "<table border=0 width=90%>")

        for(k in 1:nrow(m)){
            z <- c(z, "<tr>",
                   "<td width=45%>",
                   "<a href=\"", m$URL[k], "\" target=\"_top\">",
                   m$URL[k], "</a>",
                   "</td>\n",
                   "<td>", m$Host[k], "</td>",
                   "</tr>")
        }
        z <- c(z, "</table>", "</dd>")
    }

    z <- c(z, "</dl>")

    if(file.exists(foot))
        z <- c(z, readLines(foot))

    if(file!="")
        writeLines(z, file)

    invisible(z)
}

checkCRAN <-function(method)
{
    master <- available.packages(contrib.url("http://cran.R-project.org"), method=method)
    m <- getCRANmirrors()

    z <- list()
    for(url in as.character(m$URL))
        z[[url]] = available.packages(contrib.url(url), method=method)

    lapply(z, function(a) all.equal(a, master))
}






