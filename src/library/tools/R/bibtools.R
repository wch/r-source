#  File src/library/tools/R/bibtools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2025 The R Core Team
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

R_bibliographies_dir <- 
function()
    file.path(R.home("share"), "bibliographies")

R_bibentries <-
function()
{
    readRDS(file.path(R_bibliographies_dir(), "R.rds"))
}

update_R_bibentries <-
function(dir = NULL)
{
    if(is.null(dir))
        dir <- file.path(.R_top_srcdir_from_Rd(), 
                         "share", "bibliographies")
    bibfiles <- Sys.glob(file.path(dir, "*.R"))
    bibentries <-
        do.call(c, lapply(bibfiles,
                          utils::readCitationFile,
                          list(Encoding = "UTF-8")))
    saveRDS(bibentries, file.path(dir, "R.rds"))
}

## utils:::.bibentry_get_key
.bibentry_get_key <-
function (x) 
{
    if(!length(x)) 
        return(character())
    keys <- lapply(unclass(x), attr, "key")
    keys[!lengths(keys)] <- ""
    unlist(keys, use.names = FALSE)
}

.bibentries_from_keys <-
function(keys)
{
    keys <- keys[nzchar(keys)]
    bad <- character()
    if(!any(ind <- grepl("::", keys, fixed = TRUE))) {
        ## Special-case for efficiency.
        bib <- R_bibentries()
        pos <- match(keys, .bibentry_get_key(bib),
                     nomatch = 0L)
        bad <- keys[pos == 0L]
        y <- bib[pos]
    } else {
        n <- length(keys)
        pkgs <- character(n)
        pkgs[ind] <- sub("::.*", "", keys[ind])
        i <- split(seq_len(n), pkgs)
        y <- vector("list", length(i))
        for(j in seq_along(i)) {
            pj <- names(i)[j]
            bib <- if(!nzchar(pj))
                       R_bibentries()
                   else if(nzchar(path <- system.file("REFERENCES.rds",
                                                      package = pj)))
                       readRDS(path)
                   else if(nzchar(path <- system.file("REFERENCES.R",
                                                      package = pj)))
                       utils::readCitationFile(path,
                                               list(Encoding = "UTF-8"))
                   else if(nzchar(path <- system.file("REFERENCES.bib",
                                                      package = pj)))
                       `names<-`(bibtex::read.bib(path), NULL)
                   else
                       utils::bibentry()
            kj <- keys[i[[j]]]
            pos <- match(sub(".*::", "", kj),
                         .bibentry_get_key(bib),
                         nomatch = 0L)
            bib <- bib[pos]
            bib$key <- as.list(kj[pos > 0L])
            y[[j]] <- bib
        }
        y <- do.call(c, y)
        pos <- match(keys, .bibentry_get_key(y), nomatch = 0L)
        bad <- keys[pos == 0L]
        y <- y[pos]
    }
    if(length(bad)) {
        msg <- paste(c("Could not find bibentries for the following keys:\n%s", 
                       .strwrap22(sQuote(bad))),
                     collapse = "\n")
        warning(msg, call. = FALSE)
    }
    y
}

