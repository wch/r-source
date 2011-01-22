#  File src/library/tools/R/packageshtml.R
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

## FIXME: merge with utils:make.packages.html (unix version)
## called from
# src/library/Makefile.in (install target)
# and via .vinstall_package_indices (metadata target)
# R CMD INSTALL (unix and windows), if pre-compiled help is used.
unix.packages.html <-
    function(lib.loc=.libPaths(), docdir = R.home("doc"), libdir = .Library)
{
    f.tg <- file.path(docdir, "html", "packages.html")
    if(!file.create(f.tg)) {
        warning("cannot create HTML package index")
        return(FALSE)
    }
    file.append(f.tg, file.path(docdir, "html", "packages-head-utf8.html"))
    out <- file(f.tg, open="a")
    for (lib in lib.loc) {
        pg <- .packages(all.available = TRUE, lib.loc = lib)
        pg <- pg[order(toupper(pg), pg)]
        ## use relative indexing for .Library
        if(lib != libdir) {
            libname <- lib
            lib0 <- paste("file:///", lib, sep="")
        } else {
            lib0 <- "../../library"
            libname <- "the standard library"
        }
        cat("<p><h3>Packages in ", libname, "\n", sep = "", file=out)
        use_alpha <- (length(pg) > 100)
        first <- toupper(substr(pg, 1, 1))
        nm <- sort(names(table(first)))
        if(use_alpha) {
            writeLines("<p align=\"center\">", out)
            writeLines(paste("<a href=\"#pkgs-", nm, "\">", nm, "</a>",
                             sep = ""), out)
            writeLines("</p>\n", out)
        }
        cat('</h3>\n<p>\n<table width="100%" summary="R Package list">\n',
            sep = "", file=out)
        for (a in nm) {
            if(use_alpha)
                cat("<tr id=\"pkgs-", a, "\"/>\n", sep = "", file = out)
            for (i in pg[first == a]) {
                title <- utils::packageDescription(i, lib.loc = lib,
                                                   fields = "Title",
                                                   encoding = "UTF-8")
                if (is.na(title)) title <- "-- Title is missing --"
                cat('<tr align="left" valign="top">\n',
                    '<td width="25%"><a href="', lib0, '/', i,
                    '/html/00Index.html">', i, "</a></td><td>", title,
                    "</td></tr>\n", file=out, sep="")
            }
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}
