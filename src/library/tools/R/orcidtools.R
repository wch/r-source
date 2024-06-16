#  File src/library/tools/R/utils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2024 The R Core Team
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

### ** .ORCID_iD_regexp

.ORCID_iD_regexp <-
    "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[X0-9]$"

### ** .ORCID_iD_variants_regexp

.ORCID_iD_variants_regexp <-
    sprintf("^<?((https?://|)orcid.org/)?(%s)>?$", .ORCID_iD_regexp)

.ORCID_iD_db_from_package_sources <-
function(dir, add = FALSE)
{
    meta <- .get_package_metadata(dir, FALSE)
    ids1 <- ids2 <- character()
    if(!is.na(aar <- meta["Authors@R"])) {
        aar <- tryCatch(utils:::.read_authors_at_R_field(aar),
                        error = identity)
        if(!inherits(aar, "error")) {
            ids1 <- unlist(lapply(aar,
                                  function(e) {
                                      e <- e$comment
                                      e[names(e) == "ORCID"]
                                  }),
                           use.names = FALSE)
        }
    }
    if(file.exists(cfile <- file.path(dir, "inst", "CITATION"))) {
        cinfo <- .read_citation_quietly(cfile, meta)
        if(!inherits(cinfo, "error"))
            ids2 <- unlist(lapply(cinfo$author,
                                  function(e) {
                                      e <- e$comment
                                      e[names(e) == "ORCID"]
                                  }),
                           use.names = FALSE)
    }

    db  <- data.frame(ID = c(ids1, ids2),
                      Parent = c(rep_len("DESCRIPTION",
                                         length(ids1)),
                                 rep_len("inst/CITATION",
                                         length(ids2))))
    if(add)
        db$Parent <- file.path(basename(dir), db$Parent)
    db
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
