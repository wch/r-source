#  File src/library/tools/R/RdHelpers.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2019-2026 The R Core Team
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
        stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg),
             call. = FALSE, domain = NA)
    Rd_escape_specials(desc["Title"])
}

Rd_package_description <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
        stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg),
             call. = FALSE, domain = NA)
    Rd_escape_specials(desc["Description"])
}

Rd_package_author <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
        stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg),
             call. = FALSE, domain = NA)
    desc <- c(desc, .expand_package_description_db_R_fields(desc))
    Rd_escape_specials(desc["Author"])
}

Rd_package_maintainer <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
        stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg),
             call. = FALSE, domain = NA)
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
	    stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg),
	         call. = FALSE, domain = NA)
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

Rd_expr_manual <-
function(name = "R-exts", node = "Top")
{
    if (name %notin% rownames(utils:::R_manuals))
        stop(sprintf("\\manual must refer to one of:\n  %s",
                     paste(sQuote(rownames(utils:::R_manuals)), collapse = " ")),
             call. = FALSE, domain = NA)
    baseurl <- switch(name,
                      "rw-FAQ" = "https://cloud.R-project.org/bin/windows/base/",
                      "https://cloud.R-project.org/doc/manuals/")
    title <- utils:::R_manuals[name,2L]
    if (length(node) == 1L && node %in% c("", "Top"))
        sprintf("\\href{%s%s.html}{\\cite{%s}}", baseurl, name, title)
    else
        sprintf("\\href{%s%s.html#%s}{\\dQuote{%s}} in \\cite{%s}",
                baseurl, name, .texinfo_node_to_id(node), trimws(node), title)
}

Rd_expr_bibshow_bibstyle <- local({
    .bibstyle <- "R"
    function(new) {
        if(!missing(new))
            .bibstyle <<- new
        else
            .bibstyle
    }
})

Rd_expr_bibshow <-
function(x)
{
    x <- trimws(x)
    if(!nzchar(x)) {
        ## Provide a way to clear the keys cited cache.
        Rd_expr_bibcite_keys_cited(NULL)
        return(x)
    }
    cited <- Rd_expr_bibcite_keys_cited()    
    ## Would be nice to have a common reader for possibly multi-line
    ## comma separated values ...
    given <- strsplit(x, ",[[:space:]]*")[[1L]]
    if(any(given == "*"))
        given <- c(given[given != "*"], cited)
    Rd_expr_bibcite_keys_cited(setdiff(cited, given))
    y <- sort(unique(.bibentries_from_keys(given)))
    ## Merge bibinfo data.
    keys <- .bibentry_get_key(y)
    store <- Rd_expr_bibinfo_data_store()
    for(k in intersect(keys, names(store))) {
        entry <- store[[k]]
        for(f in names(entry))
            y[k, f] <- entry[[f]]
    }
    Rd_expr_bibinfo_data_store(store[setdiff(names(store), keys)])
    ## Typically the bibinfo data would give headers or footers, but
    ## these only get shown when printing bibenties in citation style,
    ## so we have to add them ourselves.
    headers <- y[, "header"]
    headers <- unlist(ifelse(vapply(headers, is.null, NA), "", headers),
                      use.names = FALSE)
    if(any(ind <- nzchar(headers)))
        headers[ind] <- paste(headers[ind], "\\cr")
    footers <- y[, "footer"]
    footers <- unlist(ifelse(vapply(footers, is.null, NA), "", footers),
                      use.names = FALSE)
    if(any(ind <- nzchar(footers)))
        footers[ind] <- paste("\\cr", footers[ind])
    rdfile <- processRdChunk_data_store()$Rdfile
    rdpath <- if(length(rdfile)) basename(rdfile) else ""
    paste(sprintf("%s\\if{html}{\\out{<span id=\"reference+%s+%s\"></span>}}%s%s",
                  headers,
                  rdpath,
                  string2id(.bibentry_get_key(y)),
                  toRd(y, style = Rd_expr_bibshow_bibstyle()),
                  footers),
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
    given <- strsplit(x, "(?<!\\\\),[[:space:]]*", perl = TRUE)[[1L]]
    ## We used to extract parts based on
    ##   parts <- strsplit(given, "|", fixed = TRUE)
    ## but that does not work as per ?strsplit
    ##   if there is a match at the end of the string, the output is the
    ##   same as with the match removed.
    ## Argh.
    parts <- regmatches(given,
                        gregexpr("|", given, fixed = TRUE),
                        invert = TRUE)
    if(!all(ind <- (lengths(parts) %in% c(1L, 3L)))) {
        msg <- paste(c("Found the following invalid citespecs:", 
                       .strwrap22(sQuote(given[!ind]))),
                     collapse = "\n")
        warning(msg, call. = FALSE)
        parts <- parts[ind]
    }
    keys <- after <- before <- rep_len("", length(parts))
    if(any(ind <- (lengths(parts) == 1L))) {
        keys[ind] <- unlist(parts[ind], use.names = FALSE)
    }
    if(any(ind <- (lengths(parts) == 3L))) {
        parts <- parts[ind]
        keys[ind] <- vapply(parts, `[`, "", 2L)
        after[ind] <- gsub("\\,", ",",
                           vapply(parts, `[`, "", 3L),
                           fixed = TRUE)
        before[ind] <- gsub("\\,", ",",
                            vapply(parts, `[`, "", 1L),
                            fixed = TRUE)
    }
    bib <- .bibentries_from_keys(keys)
    ind <- keys %in% .bibentry_get_key(bib)
    if(!all(ind)) {
        keys <- keys[ind]
        after <- after[ind]
        before <- before[ind]
    }
    store <- Rd_expr_bibinfo_data_store()
    for(k in intersect(keys, names(store))) {
        entry <- store[[k]]
        for(f in names(entry)) bib[k, f] <- entry[[f]]
    }
    n <- length(keys)
    if(n == 0L)
        return("")
    y <- character(n)
    prev <- Rd_expr_bibcite_keys_cited()
    rdfile <- processRdChunk_data_store()$Rdfile
    rdpath <- if(length(rdfile)) basename(rdfile) else ""
    if(textual) {
        for(i in seq_len(n)) {
            key <- keys[i]
            y[i] <- utils::citeNatbib(key, bib[key], after = after[i],
                                      previous = prev, textual = TRUE)
            prev <- c(prev, key)
        }
        if(any(ind <- nzchar(before)))
            before[ind] <- paste0(before[ind], " ")
        y <- paste0(before,
                    ## Empty \cite{} here is a kludge to 'enterPara' in Rd2HTML.
                    sprintf("\\if{html}{\\cite{}\\out{<a href=\"#reference+%s+%s\" class=\"citation\">}}",
                            rdpath,
                            string2id(keys)),
                    y,
                    rep_len("\\if{html}{\\out{</a>}}", n),
                    collapse = "; ")
    } else {
        bibp <- c("", "", ";", "a", "",  ",")
        for(i in seq_len(n)) {
            key <- keys[i]
            y[i] <- utils::citeNatbib(key, bib[key],
                                      previous = prev, textual = FALSE,
                                      bibpunct = bibp)
            prev <- c(prev, key)
        }
        if(any(ind <- nzchar(before)))
            before[ind] <- paste0(before[ind], " ")
        if(any(ind <- nzchar(after)))
            after[ind] <- paste0(", ", after[ind])
        y <- paste0("(",
                    paste0(before,
                           sprintf("\\if{html}{\\out{<a href=\"#reference+%s+%s\" class=\"citation\">}}",
                                   rdpath,
                                   string2id(keys)),
                           y,
                           rep_len("\\if{html}{\\out{</a>}}", n),
                           after,
                           collapse = ";"),
                    ")")
    }
    Rd_expr_bibcite_keys_cited(keys, TRUE)
    y
}

Rd_expr_bibinfo_data_store <- local({
    .store <- NULL
    function(new, add = FALSE) {
        if(!missing(new)) {
            if(add) {
                key <- new[[1L]]
                val <- `names<-`(list(new[[3L]]), new[[2L]])
                .store[[key]] <<- c(.store[[key]], val)
            } else
                .store <<- new
        }
        else
            .store
    }
})

Rd_expr_bibinfo <-
function(key, field, value)
{
    Rd_expr_bibinfo_data_store(list(trimws(key),
                                    trimws(field),
                                    trimws(value)),
                               add = TRUE)
}

