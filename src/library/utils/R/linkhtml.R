#  File src/library/utils/R/linkhtml.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


make.packages.html <-
    function(lib.loc = .libPaths(), temp = FALSE, verbose = TRUE,
             docdir = R.home("doc"))
{
    add_lib_index <- function(libs)
    {
        cat('<div align="left">\n<ul>\n', file = out)
        for (i in seq_along(libs)) {
            nm <- libs[i]
            if (nm == .Library) {
                cat('<li>Contents of the <a href="#lib-', i, '">',
                    'standard</a> library</li>\n', sep = "", file = out)
            } else {
                cat('<li>Contents of <a href="#lib-', i, '">', nm,
                    '</a></li>\n', sep = "", file = out)
            }
        }
        cat("</ul>\n</div>\n", file = out)
    }

    WINDOWS <- .Platform$OS.type == "windows"
    f.tg <- if (temp) {
        dir.create(file.path(tempdir(), ".R/doc/html"), recursive = TRUE,
                   showWarnings = FALSE)
        file.path(tempdir(), ".R/doc/html/packages.html")
    } else file.path(docdir, "html", "packages.html")
    op <- file.path(tempdir(), ".R/doc/html/libPaths.rds")
    if (temp && file.exists(f.tg) && file.exists(op)) {
        ## check if we can avoid remaking it.
        if(identical(lib.loc, readRDS(op))) {
            dates <- file.mtime(c(f.tg, lib.loc))
            if(which.max(dates) == 1L) return(TRUE)
        }
    }
    if (!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    if (verbose) {
        message("Making 'packages.html' ...", appendLF = FALSE, domain = NA)
        flush.console()
    }
    file.append(f.tg,
                file.path(R.home("doc"), "html", "packages-head-utf8.html"))
    out <- file(f.tg, open = "a")
    on.exit(close(out))
    if(WINDOWS) {
        rh <- chartr("\\", "/", R.home())
        drive <- substr(rh, 1L, 2L)
    }
    ## find out how many
    pkgs <- vector("list", length(lib.loc))
    names(pkgs) <- lib.loc
    for (lib in lib.loc) {
        pg <- .packages(all.available = TRUE, lib.loc = lib)
        pkgs[[lib]] <- pg[order(toupper(pg), pg)]
    }
    if (WINDOWS) {
        tot <- sum(lengths(pkgs))
        if(verbose) {
            pb <- winProgressBar("R: creating packages.html", max = tot)
            on.exit(close(pb), add = TRUE)
        }
        npkgs <- 0L
    }
    ## If there is more than one lib, have an index at the top and bottom
    if (length(lib.loc) > 1L) add_lib_index(lib.loc)
    for (ii in seq_along(lib.loc)) {
        lib <- lib.loc[ii]
        libname <-
            if (identical(lib, .Library)) "the standard library" else if (WINDOWS) chartr("/", "\\", lib) else lib
        cat("<p><h3 id=\"lib-",ii,"\">Packages in ", libname, "</h3>\n", sep = "", file = out)
        lib0 <- "../../library"
        if (!temp) {
            if (WINDOWS) {
                ## use relative indexing for .Library
                ## perhaps other site libraries
                if (is.na(pmatch(rh, lib))) {
                    lib0 <- if(substr(lib, 2L, 2L) != ":")
                        paste0(drive, lib) else lib
                    lib0 <- paste0("file:///", URLencode(lib0))
                }
            } else {
                if (lib != .Library)
                    lib0 <- paste0("file:///", URLencode(lib))
            }
        }
        pg <- pkgs[[lib]]
        use_alpha <- (length(pg) > 100)
        first <- toupper(substr(pg, 1, 1))
        nm <- sort(names(table(first)))
        if(use_alpha) {
            writeLines("<p align=\"center\">", out)
            writeLines(paste0("<a href=\"#pkgs-", nm, "\">", nm, "</a>"), out)
            writeLines("</p>\n", out)
        }
        cat('<p><table width="100%" summary="R Package list">\n', file = out)
        for (a in nm) {
            if(use_alpha)
                cat("<tr id=\"pkgs-", a, "\"> <td></td>\n", sep = "", file = out)
            for (i in pg[first == a]) {
                title <- packageDescription(i, lib.loc = lib, fields = "Title",
                                            encoding = "UTF-8")
                if (is.na(title)) title <- "-- Title is missing --"
                cat('<tr align="left" valign="top" id="lib-', i, '">\n',
                    '<td width="25%"><a href="', lib0, '/', i,
                    '/html/00Index.html">', i, "</a></td><td>", title,
                    "</td></tr>\n", file = out, sep = "")
                if (WINDOWS) {
                    npkgs <- npkgs + 1L
                    if(verbose) setWinProgressBar(pb, npkgs)
                }
            }
        }
        cat("</table>\n\n", file=out)
    }
    if (length(lib.loc) > 1L) add_lib_index(lib.loc)
    cat("</body></html>\n", file=out)
    if (verbose) { message(" ", "done"); flush.console() }
    if (temp) saveRDS(lib.loc, op)
    invisible(TRUE)
}
