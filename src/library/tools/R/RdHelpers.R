#  File src/library/tools/R/RdHelpers.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2019-2023 The R Core Team
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

Rd_escape_specials <-
function(x)
{
    gsub("([%{}\\])", "\\\\\\1", x)
}

Rd_expr_PR <-
function(x)
{
    baseurl <- "https://bugs.R-project.org/show_bug.cgi?id"
    sprintf("\\href{%s=%s}{PR#%s}", baseurl, x, x)
}

## These following functions are to take information from the package
## DESCRIPTION file at build time.  During a build, the current
## directory holds the DESCRIPTION file; set dir to something else if
## used in a different context.

Rd_macros_package_dir <-
function()
    Sys.getenv("_R_RD_MACROS_PACKAGE_DIR_", ".")

Rd_package_title <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    Rd_escape_specials(desc["Title"])
}

Rd_package_description <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    Rd_escape_specials(desc["Description"])
}

Rd_package_author <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc <- c(desc, .expand_package_description_db_R_fields(desc))
    Rd_escape_specials(desc["Author"])
}

Rd_package_maintainer <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc <- c(desc, .expand_package_description_db_R_fields(desc))
    Rd_escape_specials(desc["Maintainer"])
}

Rd_package_DESCRIPTION <-
function(pkg, lib.loc = Sys.getenv("R_BUILD_TEMPLIB"))
{
    if (!length(find.package(pkg, lib.loc = lib.loc, quiet=TRUE)))
        "This package was not yet installed at build time.\\cr"
    else {
	tabular <- function(col1, col2)
	    c("\\tabular{ll}{", paste0(col1, " \\tab ", col2, "\\cr"), "}")

	desc <- utils::packageDescription(pkg, lib.loc = lib.loc)
	if (pkg != desc[["Package"]])
	    stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
	desc <- desc[names(desc) != "Built"] # Probably a stale value
	tabular(paste0(names(desc), ":"), Rd_escape_specials(unlist(desc)))
    }
}

Rd_package_indices <-
function(pkg, lib.loc = Sys.getenv("R_BUILD_TEMPLIB"))
{
    if (!length(find.package(pkg, lib.loc = lib.loc, quiet=TRUE)))
        result <- c("", "Index:  This package was not yet installed at build time.\\cr")
    else {
    	tabular <- function(col1, col2)
    	    c("\\tabular{ll}{", paste0(col1, " \\tab ", col2, "\\cr"), "}")

        info <- library(help = pkg, lib.loc = lib.loc,
	  	    character.only = TRUE)

	result <- NULL
	# FIXME:  these indices should contain links...
	if (!is.null(info$info[[2L]]))
	    ## this is readLines(system.file("INDEX", package = pkg, lib.loc = lib.loc))
	    result <- c("", "Index of help topics:", "\\preformatted{",
				  info$info[[2L]], "}")
	if (!is.null(info$info[[3L]]))
	    ## FIXME: unreachable in build stage as vignettes get only built after partial.rdb
	    result <- c(result, "",
			"Further information is available in the following vignettes:\\cr\\cr",
			tabular(paste0("\\code{", info$info[[3L]][,1], "}"),
			      info$info[[3L]][,2]))
    }
    result
}

Rd_expr_doi <-
function(x)
{
    ## Be nice ...
    x <- .canonicalize_doi(x)

    u <- Rd_escape_specials(urlify_doi(x))
    x <- Rd_escape_specials(x)
    ## Poor person's way to allow LaTeX to break lines at slashes and
    ## dashes:
    y <- gsub("/", "\\out{\\slash{}}", fixed = TRUE,
              gsub("-", "\\out{\\-}", x, fixed = TRUE))

    sprintf("\\ifelse{text}{%s}{\\ifelse{latex}{%s}{%s}}",
            sprintf("doi:%s <https://doi.org/%s>",  # same format as showURLs=TRUE
                    x, u),
            sprintf("\\href{https://doi.org/%s}{doi:%s}",
                    u, y),
            sprintf("\\href{https://doi.org/%s}{doi:%s}",
                    u, x)
            )
}

R_bibliographies_dir <- 
function()
    file.path(R.home("share"), "bibliographies")

R_bibentries <-
function()
{
    bib <- readRDS(file.path(R_bibliographies_dir(), "R.rds"))
    bib[lengths(bib$key) > 0L]
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

Rd_expr_bibshow <-
function(x)
{
    x <- trimws(x)
    if(!nzchar(x)) {
        ## Provide a way to clear the keys cited cache.
        Rd_expr_bibcite_keys_cited(NULL)
        return(x)
    }
    bib <- R_bibentries()
    ## <FIXME>
    ## Would be nice to have a common reader for possibly multi-line
    ## comma separated values ...
    keys <- strsplit(x, ",[[:space:]]*")[[1L]]
    if(any(keys == "*")) {
        keys <- c(keys, Rd_expr_bibcite_keys_cited())
        Rd_expr_bibcite_keys_cited(NULL)
    }
    y <- sort(unique(bib[bib$key %in% keys]))
    paste(sprintf("\\if{html}{\u2060\\out{<span id=\"reference+%s\">}}%s\\if{html}{\\out{</span>}}",
                  string2id(unlist(y$key, use.names = FALSE)),
                  toRd(y)),
          collapse = "\n\n")
}

Rd_expr_bibcite_keys_cited <- local({
    .keys <- NULL
    function(new, add = FALSE) {
        if(!missing(new)) 
            .keys <<- unique(c(if(add) .keys, new))
        else
            .keys
    }
})

Rd_expr_bibcite <-
function(x, textual = FALSE)
{
    x <- trimws(x)
    bib <- R_bibentries()
    keys <- strsplit(x, ",[[:space:]]*")[[1L]]
    ## Allow b<k>a to specify before b and after a.
    ## Could also use
    ##   regmatches(keys, regexec("(.*<)?(.*)(>.*)?", keys))
    before <- after <- rep_len("", length(keys))
    if(any(ind <- grepl("<", keys))) {
        before[ind] <- sub("<.*", "", keys[ind])
        keys[ind] <- sub(".*<", "", keys[ind])
    }
    if(any(ind <- grepl(">", keys))) {
        after[ind] <- sub(".*>", "", keys[ind])
        keys[ind] <- sub(">.*", "", keys[ind])
    }
    ind <- keys %in% unlist(bib$key)
    if(!all(ind)) {
        ## <FIXME>
        ## Should warn about keys not in the bibentries
        before <- before[ind]
        after <- after[ind]
        keys <- keys[ind]
    }
    Rd_expr_bibcite_keys_cited(keys, TRUE)
    ## <FIXME>
    ## This really needs a vectorized version of cite() ...
    before <- sprintf("\\if{html}{\\out{<a href=\"#reference+%s\">}}%s",
                      string2id(keys), before)
    after <- sprintf("%s\\if{html}{\\out{</a>}}", after)
    utils::cite(keys, bib, textual, before, after)
}

