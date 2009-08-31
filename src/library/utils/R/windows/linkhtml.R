#  File src/library/utils/R/windows/linkhtml.R
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

link.html.help <- function(verbose=FALSE, lib.loc=.libPaths())
{
    if(!file.exists(file.path(R.home("doc"), "html", "search")))
       return(invisible(NULL))
    if(verbose) {
        cat(gettext("updating HTML package descriptions\n"))
        flush.console()
    }
    make.packages.html(lib.loc)
}

make.packages.html <- function(lib.loc=.libPaths(), outfile = NULL)
{
    dynamic <- is.character(outfile)
    f.tg <- if(dynamic) outfile
    else file.path(R.home("doc"), "html", "packages.html")
    f.hd <- file.path(R.home("doc"), "html", "packages-head-utf8.html")
    if(!file.create(f.tg)) {
        # warning("cannot update HTML package index")
        return(FALSE)
    }
    file.append(f.tg, f.hd)
    out <- file(f.tg, open="a")
    rh <- chartr("\\", "/", R.home())
    drive <- substring(rh, 1L, 2L)
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        lib0 <- "../../library"
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            libname <- chartr("/", "\\", lib)
            if(!dynamic) {
                lib0 <- if(substring(lib, 2L, 2L) != ":")
                    paste(drive, lib, sep="") else lib
                lib0 <- paste("file:///", URLencode(lib0), sep="")
            }
        } else
            libname <- "the standard library"
        if(length(lib.loc) > 1L)
            cat("<p><h3>Packages in ", libname, "</h3>\n",
                sep = "", file = out)
        if(!dynamic && libname != "the standard library")
            cat("<p>Cross-links from this library to other libraries may not work.\n\n", file = out)
        cat("<p>\n<table width=\"100%\" summary=\"R Package list\">\n",
            file = out)
        for (i in  pg) {
            title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                        encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}

