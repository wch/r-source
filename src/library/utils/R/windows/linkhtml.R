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
    make.packages.html(lib.loc, verbose = verbose)
}

make.packages.html <-
    function(lib.loc = .libPaths(), temp = FALSE, verbose = TRUE)
{
    f.tg <- if (temp) {
        dir.create(file.path(tempdir(), ".R/doc/html"), recursive = TRUE,
                   showWarnings = FALSE)
        file.path(tempdir(), ".R/doc/html/packages.html")
    } else file.path(R.home("doc"), "html", "packages.html")
    op <- file.path(tempdir(), ".R/doc/html/libPaths.rds")
    if (temp && file.exists(f.tg) && file.exists(op)) {
        ## check if we can avoid remaking it.
        old <- .readRDS(op)$libs
        if(identical(lib.loc, old)) {
            dates <- file.info(c(f.tg, lib.loc))$mtime
            if(which.max(dates) == 1L) return(TRUE)
        }
    }
    if (!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    message("Making packages.html", " ... ", appendLF = FALSE)
    flush.console()
    file.append(f.tg,
                file.path(R.home("doc"), "html", "packages-head-utf8.html"))
    out <- file(f.tg, open = "a")
    on.exit(close(out))
    rh <- chartr("\\", "/", R.home())
    drive <- substring(rh, 1L, 2L)
    ## find out how many
    pkgs <- vector("list", length(lib.loc))
    names(pkgs) <- lib.loc
    for (lib in lib.loc) {
        pg <- Sys.glob(file.path(lib, "*", "DESCRIPTION"))
        pkgs[[lib]] <- sort(sub(".*[\\/]", "", sub(".DESCRIPTION$", "", pg)))
    }
    tot <- sum(sapply(pkgs, length))
    if(verbose) {
        pb <- winProgressBar("R: creating packages.html", max = tot)
        on.exit(close(pb), add = TRUE)
    }
    npkgs <- 0L
    for (lib in lib.loc) {
        lib0 <- "../../library"
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            libname <- chartr("/", "\\", lib)
            if(!temp) {
                lib0 <- if(substring(lib, 2L, 2L) != ":")
                    paste(drive, lib, sep="") else lib
                lib0 <- paste("file:///", URLencode(lib0), sep="")
            }
        } else
        libname <- "the standard library"
        if(length(lib.loc) > 1L)
            cat("<p><h3>Packages in ", libname, "</h3>\n",
                sep = "", file = out)
        cat('<p>\n<table width="100%" summary="R Package list">\n', file = out)
        for (i in pkgs[[lib]]) {
            title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                        encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
            npkgs <- npkgs + 1L
            if(verbose) setWinProgressBar(pb, npkgs)
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    message("done")
    if (temp) .saveRDS(list(libs=lib.loc, npkgs=npkgs), op)
    invisible(TRUE)
}
