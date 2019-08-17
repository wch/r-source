#  File src/library/utils/R/citation.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

## What a silly name ...
.is_not_nonempty_text <-
function(x)
    is.null(x) || anyNA(x) || all(grepl("^[[:space:]]*$", x))

person <-
function(given = NULL, family = NULL, middle = NULL,
         email = NULL, role = NULL, comment = NULL,
         first = NULL, last = NULL)
{
    ## Arrange all arguments in lists of equal length.
    args <- list(given = given, family = family, middle = middle,
                 email = email, role = role, comment = comment,
		 first = first, last = last)
    if(all(vapply(args, is.null, NA)))
        return(structure(list(), class = "person"))

    args <- lapply(args, .listify)
    args_length <- lengths(args)
    if(!all(args_length_ok <- args_length %in% c(1L, max(args_length))))
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s",
                         paste(names(args)[!args_length_ok],
                               collapse = ", ")),
                domain = NA)
    args <- lapply(args, function(x) rep_len(x, max(args_length)))

    ## <COMMENT Z>
    ## We could do this more elegantly, but let's just go through the
    ## list processing person by person.  I'm just recycling the old
    ## person() code for this.
    ## </COMMENT>
    person1 <-
    function(given = NULL, family = NULL, middle = NULL,
             email = NULL, role = NULL, comment = NULL,
             first = NULL, last = NULL)
    {
        if(!.is_not_nonempty_text(first)) {
            if(!.is_not_nonempty_text(given))
                stop(gettextf("Use either %s or %s/%s but not both.",
                              sQuote("given"),
                              sQuote("first"), sQuote("middle")),
                     domain = NA)
            ## <FIXME>
            ## Start warning eventually ... maybe use message() for now?
            message(gettextf("It is recommended to use %s instead of %s.",
                             sQuote("given"), sQuote("first")),
                    domain = NA)
            ## </FIXME>
            given <- first
        }
        if(!.is_not_nonempty_text(middle)) {
            ## <FIXME>
            ## Start warning eventually ... maybe use message() for now?
            message(gettextf("It is recommended to use %s instead of %s.",
                             sQuote("given"), sQuote("middle")),
                    domain = NA)
            ## </FIXME>
            given <- c(given, unlist(strsplit(middle, "[[:space:]]+")))
        }

        if(!.is_not_nonempty_text(last)) {
            if(!.is_not_nonempty_text(family))
                stop(gettextf("Use either %s or %s but not both.",
                              sQuote("family"), sQuote("last")),
                     domain = NA)
            ## <FIXME>
            ## Start warning eventually ... maybe use message() for now?
            message(gettextf("It is recommended to use %s instead of %s.",
                             sQuote("family"), sQuote("last")),
                    domain = NA)
            ## </FIXME>
            family <- last
        }

        ## Set all empty arguments to NULL.
        if(.is_not_nonempty_text(given)) given <- NULL
        if(.is_not_nonempty_text(family)) family <- NULL
        if(.is_not_nonempty_text(email)) email <- NULL
        if(.is_not_nonempty_text(role)) {
            if(!is.null(role))
                warning(sprintf(ngettext(length(role),
                                         "Invalid role specification: %s.",
                                         "Invalid role specifications: %s."),
                                paste(sQuote(role), collapse = ", ")),
                        domain = NA)
            role <- NULL
        }
        if(.is_not_nonempty_text(comment)) comment <- NULL

        ## <FIXME>
        ## Use something along the lines of
        ##   tools:::.valid_maintainer_field_regexp
        ## to validate given email addresses.
        ## </FIXME>

        if(length(role))
            role <- .canonicalize_person_role(role)

        if(length(comment)) {
            ## Be nice and recognize ORCID identifiers given as URLs
            ## but perhaps without an ORCID name.
            ind <- grepl(paste0("^https?://orcid.org/",
                                "([[:digit:]]{4}[-]){3}[[:digit:]]{3}[[:alnum:]]$"),
                         comment)
            if(any(ind)) {
                if(is.null(names(comment)))
                    names(comment) <- ifelse(ind, "ORCID", "")
                else
                    names(comment)[ind] <- "ORCID"
            }
        }

        rval <- list(given = given, family = family, role = role,
                     email = email, comment = comment)
        ## Canonicalize 0-length character arguments to NULL.
        if(any(ind <- (lengths(rval) == 0L)))
            rval[ind] <- vector("list", length = sum(ind))
        ## Give nothing if there is nothing.
        if(all(vapply(rval, is.null, NA)))
            NULL
        else
            rval
    } ## end{ person1 }
    force(person1)# {codetools}
    rval <-
        lapply(seq_along(args$given),
               function(i)
               with(args,
                    person1(given = given[[i]], family = family[[i]],
                            middle = middle[[i]], email = email[[i]],
                            role = role[[i]], comment = comment[[i]],
                            first = first[[i]], last = last[[i]])))

    ## <COMMENT Z>
    ## Should we check that for each person there is at least one
    ## non-NULL entry?
    ## </COMMENT>
    ## Yes!

    structure(rval[!vapply(rval, is.null, NA)],
              class = "person")
}

.canonicalize_person_role <-
function(role)
{
    ## Be nice.  Given roles must either exactly match the role code,
    ## or be uniquely pmatchable modulo case against the role terms.
    pos <- which(is.na(match(role, MARC_relator_db$code)))
    if(length(pos)) {
        ind <- pmatch(tolower(role[pos]),
                      tolower(MARC_relator_db$name),
                      0L)
        role[pos[ind > 0L]] <- MARC_relator_db$code[ind]
        if(any(ind <- (ind == 0L))) {
            warning(sprintf(ngettext(length(pos[ind]),
                                     "Invalid role specification: %s.",
                                     "Invalid role specifications: %s."),
                            paste(sQuote(role[pos[ind]]), collapse = ", ")),
                    domain = NA)
            role <- role[-pos[ind]]
        }
    }
    role
}

`[[.person` <-
`[.person` <-
function(x, i)
{
    rval <- unclass(x)[i]
    class(rval) <- class(x)
    return(rval)
}

print.person <-
function(x, ...)
{
    if(length(x)) print(format(x, ...))
    invisible(x)
}

`$.person` <-
function(x, name)
{
    ## <COMMENT Z>
    ## extract internal list elements, return list if length > 1, vector
    ## otherwise (to mirror the behaviur of the input format for
    ## person())
    ## </COMMENT>
    name <- match.arg(name,
                      c("given", "family", "role", "email", "comment",
                        "first", "last", "middle")) # for now ...
    ## <COMMENT Z>
    ## Let's be nice and support first/middle/last for now.
    ## </COMMENT>
    if(name %in% c("first", "last", "middle")) {
        message(gettextf("It is recommended to use %s/%s instead of %s/%s/%s.",
                         sQuote("given"), sQuote("family"),
                         sQuote("first"), sQuote("middle"), sQuote("last")),
                domain = NA)
        oname <- name
	name <- switch(name,
	    "first" = "given",
	    "middle" = "given",
	    "last" = "family"
	)
    } else {
        oname <- name
    }

    rval <- lapply(unclass(x), function(p) p[[name]])

    if(oname == "first") rval <- lapply(rval, head, 1L)
    if(oname == "middle") {
        rval <- lapply(rval, tail, -1L)
        if(any(ind <- (lengths(rval) == 0L)))
            rval[ind] <- vector("list", length = sum(ind))
    }

    if(length(rval) == 1L) rval <- rval[[1L]]
    rval
}

`$<-.person` <-
function(x, name, value)
{
    name <- match.arg(name, c("given", "family", "role", "email", "comment"))
    x <- .listify(unclass(x))
    value <- rep_len(value, length(x))

    if(name == "role")
        value <- lapply(value, .canonicalize_person_role)

    for(i in seq_along(x)) {
        x[[i]][[name]] <- if(.is_not_nonempty_text(value[[i]]))
            NULL
        else as.character(value[[i]])
    }

    class(x) <- "person"
    x
}

c.person <-
function(..., recursive = FALSE)
{
    args <- list(...)
    if(!all(vapply(args, inherits, NA, "person")))
        warning(gettextf("method is only applicable to %s objects",
                         sQuote("person")),
                domain = NA)
    args <- lapply(args, unclass)
    rval <- do.call("c", args)
    class(rval) <- "person"
    rval
}

as.person <-
function(x)
    UseMethod("as.person")

as.person.default <-
function(x)
{
    if(inherits(x, "person")) return(x)

    x <- trimws(as.character(x))

    if(!length(x)) return(person())

    ## Need to split the strings into individual person components.
    ## We used to split at ',' and 'and', but of course these could be
    ## contained in roles or comments as well.
    ## Hence, try the following.
    ## A. Replace all comment, role and email substrings by all-z
    ##    substrings of the same length.
    ## B. Tokenize the strings according to the split regexp matches in
    ##    the corresponding z-ified strings.
    ## C. Extract the persons from the thus obtained tokens.

    ## Create strings consisting of a given character c with given
    ## numbers n of characters.
    strings <- function(n, c = "z") {
        vapply(Map(rep.int, rep.int(c, length(n)), n,
                   USE.NAMES = FALSE),
               paste, "", collapse = "")
    }

    ## Replace matches of pattern in x by all-z substrings of the same
    ## length.
    zify <- function(pattern, x) {
        if(!length(x)) return(character())
        m <- gregexpr(pattern, x)
        regmatches(x, m) <-
            Map(strings, lapply(regmatches(x, m), nchar))
        x
    }

    ## Step A.
    y <- zify("\\([^)]*\\)", x)
    y <- zify("\\[[^]]*\\]", y)
    y <- zify("<[^>]*>", y)

    ## Step B.
    pattern <- "[[:space:]]?(,|,?[[:space:]]and)[[:space:]]+"
    x <- do.call("c",
                 regmatches(x, gregexpr(pattern, y), invert = TRUE))
    x <- x[!vapply(x, .is_not_nonempty_text, NA)]

    ## don't expect Jr. to be a person
    jr <- which(!is.na(match(x, c("Jr", "Jr.", "jr", "jr."))))
    if(length(jr)) {
        jr <- jr[jr > 1L]
        x[jr - 1L] <- paste(x[jr - 1L], x[jr], sep = ", ")
        x <- x[-jr]
    }

    if(!length(x)) return(person())

    ## Step C.
    as_person1 <- function(x) {
        comment <- if(grepl("\\(.*\\)", x))
            sub(".*\\(([^)]*)\\).*", "\\1", x)
        else NULL
        x <- sub("[[:space:]]*\\([^)]*\\)", "", x)
        email <- if(grepl("<.*>", x))
            unlist(strsplit(gsub("[[:space:]]*", "",
                                 sub(".*<([^>]*)>.*", "\\1", x)),
                            ",", fixed = TRUE))
        else NULL
        x <- sub("[[:space:]]*<[^>]*>", "", x)
        role <- if(grepl("\\[.*\\]", x))
            unlist(strsplit(gsub("[[:space:]]*", "",
                                 sub(".*\\[([^]]*)\\].*", "\\1", x)),
                            ",", fixed = TRUE))
        else NULL
        x <- sub("[[:space:]]*\\[[^)]*\\]", "", x)
        x <- unlist(strsplit(x, "[[:space:]]+"))

	## try to correctly guess von/van/de, Jr., etc.
	jr <- c("Jr", "Jr.")
	von <- c("De", "Den", "Der", "La", "Le", "Ten", "Van", "Von")
	family <- x[length(x)]
	given <- x[-length(x)]
	if(length(family) &&
           !is.na(match(family, c(jr, tolower(jr))))) {
            family <- paste(given[length(given)], family)
            given <- given[-length(given)]
	}
	if((ng <- length(given)) &&
           !is.na(match(gng <- given[ng], c(von, tolower(von))))) {
            family <- paste(gng, family)
            given <- given[-ng]
	}
	if((ng <- length(given)) &&
           !is.na(match(gng <- given[ng], c(von, tolower(von))))) {
            family <- paste(gng, family)
            given <- given[-ng]
	}

        z <- person(given = given, family = family,
                    email = email, role = role, comment = comment)
        return(z)
    }

    as.list(do.call("c", lapply(x, as_person1)))
}

personList <-
function(...)
{
    z <- list(...)
    if(!all(vapply(z, inherits, NA, "person")))
        stop(gettextf("all arguments must be of class %s",
                      dQuote("person")),
             domain = NA)
    do.call("c", z)
}

as.personList <-
function(x)
    UseMethod("as.personList")

as.personList.person <-
function(x)
    x

as.personList.default <-
function(x)
{
    if(inherits(x, "person")) return(x)
    do.call("c", lapply(x, as.person))
}

format.person <-
function(x,
         include = c("given", "family", "email", "role", "comment"),
         braces =
         list(given = "", family = "", email = c("<", ">"),
              role = c("[", "]"), comment = c("(", ")")),
         collapse =
         list(given = " ", family = " ", email = ", ",
              role = ", ", comment = ", "),
         ...,
         style = c("text", "R")
         )
{
    if(!length(x)) return(character())

    style <- match.arg(style)

    if(style == "R") return(.format_person_as_R_code(x))

    args <- c("given", "family", "email", "role", "comment")
    include <- sapply(include, match.arg, args)

    ## process defaults
    braces <- braces[args]
    collapse <- collapse[args]
    names(braces) <- names(collapse) <- args
    if(is.null(braces$given)) braces$given <- ""
    if(is.null(braces$family)) braces$family <- ""
    if(is.null(braces$email)) braces$email <- c("<", ">")
    if(is.null(braces$role)) braces$role <- c("[", "]")
    if(is.null(braces$comment)) braces$comment <- c("(", ")")
    braces <- lapply(braces, rep_len, 2L)
    if(is.null(collapse$given)) collapse$given <- " "
    if(is.null(collapse$family)) collapse$family <- " "
    if(is.null(collapse$email)) collapse$email <- ", "
    if(is.null(collapse$role)) collapse$role <- ", "
    if(is.null(collapse$comment)) collapse$comment <- ", "
    collapse <- lapply(collapse, rep_len, 1L)

    ## extract selected elements
    x <- lapply(unclass(x), "[", include)
    braces <- braces[include]
    collapse <- collapse[include]

    if(any(include == "comment"))
        x <- lapply(x,
                    function(e) {
                        e$comment <-
                            .expand_ORCID_identifier(e$comment)
                        e
                    })

    paste_collapse <- function(x, collapse) {
        if(is.na(collapse) || isFALSE(collapse)) {
 	    x[1L]
 	} else {
 	    paste(x, collapse = collapse)
 	}
    }

    ## format 1 person
    format_person1 <- function(p) {
	rval <- lapply(seq_along(p),
                       function(i) {
                           if(is.null(p[[i]]))
                               NULL
                           else
                               paste0(braces[[i]][1L],
                                      paste_collapse(p[[i]],
                                                     collapse[[i]]),
                                      braces[[i]][2L])
                       })
	paste(do.call("c", rval), collapse = " ")
    }

    sapply(x, format_person1)
}

as.character.person <-
function(x, ...)
    format(x, ...)

toBibtex.person <-
function(object, ...)
{
    object <- sapply(object, function(p) {
         br <- if(is.null(p$family)) c("{", "}") else c("", "")
         format(p, include = c("family", "given"),
                braces = list(given = br, family = c("", ",")))
    })
    paste(object[nzchar(object)], collapse = " and ")
}

.canonicalize_ORCID_identifier <-
function(x)
{
    paste0("https://orcid.org/", sub(".*/", "", x))
}

.expand_ORCID_identifier <-
function(x)
{
    if(any(ind <- (names(x) == "ORCID")))
        x[ind] <- paste0("<",
                         .canonicalize_ORCID_identifier(x[ind]),
                         ">")
    x
}

######################################################################

bibentry <-
function(bibtype, textVersion = NULL, header = NULL, footer = NULL, key = NULL,
         ...,
         other = list(), mheader = NULL, mfooter = NULL)
{
    BibTeX_names <- names(tools:::BibTeX_entry_field_db)

    args <- c(list(...), other)
    if(!length(args))
        return(structure(list(), class = "bibentry"))
    if(any(vapply(names(args), .is_not_nonempty_text, NA)))
        stop("all fields have to be named")

    ## arrange all arguments in lists of equal length
    args <- c(list(bibtype = bibtype, textVersion = textVersion,
              header = header, footer = footer, key = key), list(...))
    args <- lapply(args, .listify)
    other <- lapply(other, .listify)
    max_length <- max(lengths(c(args, other)))

    args_length <- lengths(args)
    if(!all(args_length_ok <- args_length %in% c(1L, max_length)))
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s",
                         paste(names(args)[!args_length_ok],
                               collapse = ", ")),
                domain = NA)
    args <- lapply(args, function(x) rep_len(x, max_length))

    other_length <- lengths(other)
    if(!all(other_length_ok <- other_length %in% c(1L, max_length)))
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s",
                         paste(names(other)[!other_length_ok],
                               collapse = ", ")),
                domain = NA)
    other <- lapply(other, function(x) rep_len(x, max_length))

    bibentry1 <-
    function(bibtype, textVersion, header = NULL, footer = NULL, key = NULL, ..., other = list())
    {
        ## process bibtype
	bibtype <- as.character(bibtype)
	stopifnot(length(bibtype) == 1L)
        pos <- match(tolower(bibtype), tolower(BibTeX_names))
	if(is.na(pos))
            stop(gettextf("%s has to be one of %s",
                          sQuote("bibtype"),
                          paste(BibTeX_names, collapse = ", ")),
                 domain = NA)
	bibtype <- BibTeX_names[pos]

        ## process fields
        rval <- c(list(...), other)
        rval <- rval[!vapply(rval, .is_not_nonempty_text, NA)]
	fields <- tolower(names(rval))
        names(rval) <- fields
        attr(rval, "bibtype") <- bibtype

        ## check required fields
        .bibentry_check_bibentry1(rval)

        ## canonicalize
        pos <- fields %in% c("author", "editor")
	if(any(pos)) {
            for(i in which(pos)) rval[[i]] <- as.person(rval[[i]])
	}
	if(any(!pos)) {
            for(i in which(!pos)) rval[[i]] <- as.character(rval[[i]])
	}

        ## set attributes
        attr(rval, "key") <-
            if(is.null(key)) NULL else as.character(key)
        if(!is.null(textVersion))
            attr(rval, "textVersion") <- as.character(textVersion)
        if(!.is_not_nonempty_text(header))
            attr(rval, "header") <- paste(header, collapse = "\n")
        if(!.is_not_nonempty_text(footer))
            attr(rval, "footer") <- paste(footer, collapse = "\n")

        return(rval)
    }

    rval <- lapply(seq_along(args$bibtype),
                   function(i)
                   do.call(bibentry1,
                           c(lapply(args, "[[", i),
                             list(other = lapply(other, "[[", i)))))

    ## add main header/footer for overall bibentry vector
    if(!.is_not_nonempty_text(mheader))
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if(!.is_not_nonempty_text(mfooter))
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")

    class(rval) <- "bibentry"
    rval
}

.bibentry_check_bibentry1 <-
function(x, force = FALSE)
{
    fields <- names(x)
    if(!force && !.is_not_nonempty_text(x$crossref)) return(NULL)
    bibtype <- attr(x, "bibtype")
    rfields <-
        strsplit(tools:::BibTeX_entry_field_db[[bibtype]], "|",
                 fixed = TRUE)
    if(length(rfields) > 0L) {
        ok <- vapply(rfields, function(f) any(f %in% fields), NA)
        if(any(!ok))
            stop(sprintf(ngettext(sum(!ok),
                                  "A bibentry of bibtype %s has to specify the field: %s",
                                  "A bibentry of bibtype %s has to specify the fields: %s"),
                          sQuote(bibtype), paste(rfields[!ok], collapse = ", ")),
                 domain = NA)
    }
}

bibentry_attribute_names <-
    c("bibtype", "textVersion", "header", "footer", "key")

bibentry_list_attribute_names <-
    c("mheader", "mfooter")

.bibentry_get_key <-
function(x)
{
    if(!length(x)) return(character())
    keys <- lapply(unclass(x), attr, "key")
    keys[!vapply(keys, length, 0L)] <- ""
    unlist(keys)
}

`[[.bibentry` <-
`[.bibentry` <-
function(x, i, drop = TRUE)
{
    if(!length(x)) return(x)

    cl <- class(x)
    class(x) <- NULL
    ## For character subscripting, use keys if there are no names.
    ## Note that creating bibentries does not add the keys as names:
    ## assuming that both can independently be set, we would need to
    ## track whether names were auto-generated or not.
    ## (We could consider providing a names() getter which returns given
    ## names or keys as used for character subscripting, though).
    if(is.character(i) && is.null(names(x)))
        names(x) <- .bibentry_get_key(x)
    y <- x[i]
    if(!all(ok <- lengths(y) > 0L)) {
        warning("subscript out of bounds")
        y <- y[ok]
    }
    if(!drop)
        attributes(y) <- attributes(x)[bibentry_list_attribute_names]
    class(y) <- cl
    y
}

bibentry_format_styles <-
    c("text", "Bibtex", "citation", "html", "latex", "textVersion", "R")

.bibentry_match_format_style <-
function(style)
{
    ind <- pmatch(tolower(style), tolower(bibentry_format_styles),
                  nomatch = 0L)
    if(all(ind == 0L))
        stop(gettextf("%s should be one of %s",
                      sQuote("style"),
                      paste(dQuote(bibentry_format_styles),
                            collapse = ", ")),
             domain = NA)
    bibentry_format_styles[ind]
}

format.bibentry <-
function(x, style = "text", .bibstyle = NULL,
         citation.bibtex.max = getOption("citation.bibtex.max", 1),
         bibtex = length(x) <= citation.bibtex.max,
         sort = FALSE,
         macros = NULL,
         ...)
{
    if(!length(x)) return(character())

    style <- .bibentry_match_format_style(style)

    if(sort) x <- sort(x, .bibstyle = .bibstyle)
    x$.index <- as.list(seq_along(x))
    if(!missing(citation.bibtex.max))
	warning(gettextf("Argument '%s' is deprecated; rather set '%s' instead.",
			 "citation.bibtex.max", "bibtex=*"),
                domain = NA)

    format_via_Rd <- function(f) {
        out <- file()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({tools::Rd2txt_options(saveopt); close(out)})
        permissive <-
            Sys.getenv("_R_UTILS_FORMAT_BIBENTRY_VIA_RD_PERMISSIVE_",
                       "TRUE")
        permissive <- tools:::config_val_to_logical(permissive)
        macros <- if(is.null(macros))
		      tools:::initialRdMacros()
                  else if(is.character(macros))
		      tools::loadRdMacros(macros,
                                          tools:::initialRdMacros())
        sapply(.bibentry_expand_crossrefs(x),
               function(y) {
                   txt <- tools::toRd(y, style = .bibstyle)
                   ## <FIXME>
                   ## Ensure a closing </p> via a final empty line for
                   ## now (PR #15692).
                   if(style == "html") txt <- paste(txt, "\n")
                   ## </FIXME>
                   con <- textConnection(txt)
                   on.exit(close(con))
                   rd <- tools::parse_Rd(con,
                                         fragment = TRUE,
                                         permissive = permissive,
                                         macros = macros)
                   rd <- tools:::processRdSexprs(rd,
                                                 "build",
                                                 macros = attr(rd, "macros"))
                   f(rd, fragment = TRUE, out = out, ...)
                   paste(readLines(out), collapse = "\n")
               })
    }

    format_as_citation <- function(x, msg) {
         c(paste(strwrap(attr(x, "mheader")), collapse = "\n"),
          unlist(lapply(x, function(y) {
              paste(c(if(!is.null(y$header))
                      c(strwrap(y$header), ""),
                      if(!is.null(y$textVersion)) {
                          strwrap(y$textVersion, prefix = "  ")
                      } else {
                          format(y)
                      },
                      if(bibtex) {
                          c(gettext("\nA BibTeX entry for LaTeX users is\n"),
			    paste0("  ", unclass(toBibtex(y))))
                      },
                      if(!is.null(y$footer))
                      c("", strwrap(y$footer))),
                    collapse = "\n")
          })),
	  paste(strwrap(c(attr(x, "mfooter"),
			  if(!bibtex && msg)
  "To see these entries in BibTeX format, use 'print(<citation>, bibtex=TRUE)',
  'toBibtex(.)', or set 'options(citation.bibtex.max=999)'."
		)), collapse = "\n")
          )
    }

    out <-
        switch(style,
               "text" = format_via_Rd(tools::Rd2txt),
               "html" = format_via_Rd(tools::Rd2HTML),
               "latex" = format_via_Rd(tools::Rd2latex),
               "Bibtex" = {
                   unlist(lapply(x,
                                 function(y)
                                 paste(toBibtex(y), collapse = "\n")))
               },
               "textVersion" = {
                   out <- lapply(unclass(x), attr, "textVersion")
                   out[!lengths(out)] <- ""
                   unlist(out)
               },
               "citation" = format_as_citation(x,
                                               msg = missing(bibtex) &&
                                                   missing(citation.bibtex.max)),
               "R" = .format_bibentry_as_R_code(x, ...)
               )
    as.character(out)
}

.bibentry_expand_crossrefs <-
function(x, more = list())
{
    y <- if(length(more))
        do.call(c, c(list(x), more))
    else
        x

    x <- unclass(x)
    y <- unclass(y)

    crossrefs <- lapply(x, `[[`, "crossref")
    pc <- which(vapply(crossrefs, length, 0L) > 0L)

    if(length(pc)) {
        pk <- match(unlist(crossrefs[pc]), .bibentry_get_key(y))
        ## If an entry has a crossref we cannot resolve it might still
        ## be complete: we could warn about the bad crossref ...
        ok <- !is.na(pk)
        ## Merge entries: note that InCollection and InProceedings need
        ## to remap title to booktitle as needed.
        x[pc[ok]] <-
            Map(function(u, v) {
                add <- setdiff(names(v), names(u))
                u[add] <- v[add]
                if(!is.na(match(tolower(attr(u, "bibtype")),
                                c("incollection", "inproceedings"))) &&
                   is.null(u$booktitle))
                    u$booktitle <- v$title
                u
            },
                x[pc[ok]],
                y[pk[ok]])
        ## Now check entries with crossrefs for completeness.
        ## Ignore bad entries with a warning.
        status <- lapply(x[pc],
                         function(e)
                         tryCatch(.bibentry_check_bibentry1(e, TRUE),
                                  error = identity))
        bad <- which(vapply(status, inherits, NA, "error"))
        if(length(bad)) {
            for(b in bad) {
                warning(gettextf("Dropping invalid entry %d:\n%s",
                                 pc[b],
                                 conditionMessage(status[[b]])))
            }
            x[pc[bad]] <- NULL
        }
    }

    class(x) <- "bibentry"
    x
}

print.bibentry <-
function(x, style = "text", .bibstyle = NULL, ...)
{
    style <- .bibentry_match_format_style(style)

    if(style == "R") {
	writeLines(format(x, "R", collapse = TRUE, ...))
    } else if(length(x)) {
	y <- format(x, style, .bibstyle, ...)
        if(style == "citation") {
            ## Printing in citation style does extra headers/footers
            ## (which however may be empty), so it is handled
            ## differently.
            n <- length(y)
            if(nzchar(header <- y[1L]))
                header <- c("", header, "")
            if(nzchar(footer <- y[n]))
                footer <- c("", footer, "")
            writeLines(c(header,
                         paste(y[-c(1L, n)], collapse = "\n\n"),
                         footer))
        } else {
            writeLines(paste(y, collapse = "\n\n"))
        }
    }

    invisible(x)
}

.format_call_RR <-
function(cname, cargs)
{
    ## Format call with ragged right argument list (one arg per line).
    cargs <- as.list(cargs)
    n <- length(cargs)
    lens <- lengths(cargs)
    sums <- cumsum(lens)
    starters <- c(sprintf("%s(", cname),
                  rep.int(strrep(" ", nchar(cname) + 1L), sums[n] - 1L))
    trailers <- c(rep.int("", sums[n] - 1L), ")")
    trailers[sums[-n]] <- ","
    sprintf("%s%s%s", starters, unlist(cargs), trailers)
}

.format_bibentry_as_R_code <-
function(x, collapse = FALSE)
{
    if(!length(x)) return("bibentry()")

    x$.index <- NULL

    ## There are two subleties for constructing R calls giving a given
    ## bibentry object.
    ## * There can be mheader and mfooter entries.
    ##   If there are, we put them into the first bibentry.
    ## * There could be field names which clash with the names of the
    ##   bibentry() formals: these would need to be put as a list into
    ##   the 'other' formal.

    ## The following make it into the attributes of an entry.
    anames <- bibentry_attribute_names
    ## The following make it into the attributes of the object.
    manames <- c("mheader", "mfooter")

    ## Format a single element (person or string, at least for now).
    f <- function(e) {
        if(inherits(e, "person"))
            .format_person_as_R_code(e)
        else
            deparse(e)
    }

    g <- function(u, v) {
        prefix <- sprintf("%s = ", u)
        n <- length(v)
        if(n > 1L)
            prefix <- c(prefix,
                        rep.int(strrep(" ", nchar(prefix)), n - 1L))
        sprintf("%s%s", prefix, v)
    }

    s <- lapply(unclass(x),
                function(e) {
                    a <- Filter(length, attributes(e)[anames])
                    e <- e[!vapply(e, is.null, NA)]
                    ind <- !is.na(match(names(e),
                                       c(anames, manames, "other")))
                    if(any(ind)) {
                        other <- paste(names(e[ind]),
                                       sapply(e[ind], f),
                                       sep = " = ")

                        other <- Map(g,
                                     names(e[ind]),
                                     sapply(e[ind], f))
                        other <- .format_call_RR("list", other)
                        e <- e[!ind]
                    } else {
                        other <- NULL
                    }
                    c(Map(g, names(a), sapply(a, deparse)),
                      Map(g, names(e), sapply(e, f)),
                      if(length(other)) list(g("other", other)))

                })

    if(!is.null(mheader <- attr(x, "mheader")))
        s[[1L]] <- c(s[[1L]],
                     paste("mheader =", deparse(mheader)))
    if(!is.null(mfooter <- attr(x, "mfooter")))
        s[[1L]] <- c(s[[1L]],
                     paste("mfooter =", deparse(mfooter)))

    s <- Map(.format_call_RR, "bibentry", s)
    if(collapse && (length(s) > 1L))
        paste(.format_call_RR("c", s), collapse = "\n")
    else
        unlist(lapply(s, paste, collapse = "\n"), use.names = FALSE)

}

.format_person_as_R_code <-
function(x)
{
    s <- lapply(unclass(x),
                function(e) {
                    e <- e[!vapply(e, is.null, NA)]
                    cargs <-
                        sprintf("%s = %s", names(e), sapply(e, deparse))
                    .format_call_RR("person", cargs)
                })
    if(length(s) > 1L)
        .format_call_RR("c", s)
    else
        unlist(s, use.names = FALSE)
}

`$.bibentry` <-
function(x, name)
{
    if(!length(x)) return(NULL)

    ## <COMMENT Z>
    ## Extract internal list elements, return list if length > 1, vector
    ## otherwise (to mirror the behaviour of the input format for
    ## bibentry())
    ## </COMMENT>
    is_attribute <- name %in% bibentry_attribute_names
    rval <- if(is_attribute) lapply(unclass(x), attr, name)
        else lapply(unclass(x), "[[", name)
    if(length(rval) == 1L) rval <- rval[[1L]]
    rval
}

`$<-.bibentry` <-
function(x, name, value)
{
    is_attribute <- name %in% bibentry_attribute_names

    x <- unclass(x)
    name <- tolower(name)

    ## recycle value
    value <- rep_len(.listify(value), length(x))

    ## check bibtype
    if(name == "bibtype") {
        stopifnot(all(lengths(value) == 1L))
        BibTeX_names <- names(tools:::BibTeX_entry_field_db)
        value <- unlist(value)
        pos <- match(tolower(value), tolower(BibTeX_names))
        if(anyNA(pos))
            stop(gettextf("%s has to be one of %s",
                          sQuote("bibtype"),
                          paste(BibTeX_names, collapse = ", ")),
                 domain = NA)
        value <- as.list(BibTeX_names[pos])
    }

    ## replace all values
    for(i in seq_along(x)) {
        if(is_attribute) {
	    attr(x[[i]], name) <-
                if(is.null(value[[i]])) NULL else paste(value[[i]])
	} else {
	    x[[i]][[name]] <-
                if(is.null(value[[i]])) NULL else {
                    if(name %in% c("author", "editor"))
                        as.person(value[[i]])
                    else paste(value[[i]])
                }
        }
    }

    ## check whether all elements still have their required fields
    for(i in seq_along(x)) .bibentry_check_bibentry1(x[[i]])

    class(x) <- "bibentry"
    x
}

c.bibentry <-
function(..., recursive = FALSE)
{
    args <- list(...)
    if(!all(vapply(args, inherits, NA, "bibentry")))
        warning(gettextf("method is only applicable to %s objects",
                         sQuote("bibentry")),
                domain = NA)
    args <- lapply(args, unclass)
    rval <- do.call("c", args)
    class(rval) <- "bibentry"
    rval
}

toBibtex.bibentry <-
function(object, ...)
{
    format_author <- function(author) paste(sapply(author, function(p) {
	fnms <- p$family
	only_given_or_family <-
            (is.null(fnms) || is.null(p$given)) &&
            !(identical(fnms, "others") || identical(p$given, "others"))
	fbrc <- if(length(fnms) > 1L ||
                   any(grepl("[[:space:]]", fnms)) ||
                   only_given_or_family) c("{", "}") else ""
	gbrc <- if(only_given_or_family) c("{", "}") else ""
        format(p, include = c("given", "family"),
               braces = list(given = gbrc, family = fbrc))
    }), collapse = " and ")

    format_bibentry1 <- function(object) {
	object <- unclass(object)[[1L]]
        rval <- paste0("@", attr(object, "bibtype"), "{", attr(object, "key"), ",")
        if("author" %in% names(object))
            object$author <- format_author(object$author)
        if("editor" %in% names(object))
            object$editor <- format_author(object$editor)

        rval <- c(rval,
                  sapply(names(object), function (n)
                         paste0("  ", n, " = {", object[[n]], "},")),
                  "}", "")
        return(rval)
    }

    if(length(object)) {
        object$.index <- NULL
        rval <- head(unlist(lapply(object, format_bibentry1)), -1L)
    } else
        rval <- character()
    class(rval) <- "Bibtex"
    rval
}

sort.bibentry <-
function(x, decreasing = FALSE, .bibstyle = NULL, drop = FALSE, ...)
{
    x[order(tools::bibstyle(.bibstyle, .default = FALSE)$sortKeys(x),
            decreasing = decreasing),
      drop = drop]
}

rep.bibentry <-
function(x, ...)
{
    y <- NextMethod("rep")
    class(y) <- class(x)
    y
}

unique.bibentry <-
function(x, ...)
{
    y <- NextMethod("unique")
    class(y) <- class(x)
    y
}

######################################################################

citEntry <-
function(entry, textVersion, header = NULL, footer = NULL, ...)
    bibentry(bibtype = entry, textVersion = textVersion,
             header = header, footer = footer, ...)

citHeader <-
function(...)
{
    rval <- paste(...)
    class(rval) <- "citationHeader"
    rval
}

citFooter <-
function(...)
{
    rval <- paste(...)
    class(rval) <- "citationFooter"
    rval
}

readCitationFile <-
function(file, meta = NULL)
{
    meta <- as.list(meta)
    exprs <- tools:::.parse_CITATION_file(file, meta$Encoding)

    rval <- list()
    mheader <- NULL
    mfooter <- NULL
    envir <- new.env(hash = TRUE)
    ## Make the package metadata available to the citation entries.
    assign("meta", meta, envir = envir)

    for(expr in exprs) {
        x <- eval(expr, envir = envir)
        if(inherits(x, "bibentry"))
            rval <- c(rval, list(x))
        else if(identical(class(x), "citationHeader"))
            mheader <- c(mheader, x)
        else if(identical(class(x), "citationFooter"))
            mfooter <- c(mfooter, x)
    }

    rlen <- length(rval)
    if(rlen == 1L)
        rval <- rval[[1L]]
    else if(rlen > 1L)
        rval <- do.call("c", rval)
    if(!.is_not_nonempty_text(mheader))
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if(!.is_not_nonempty_text(mfooter))
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")

    .citation(rval, meta$Package)
}

######################################################################

citation <-
function(package = "base", lib.loc = NULL, auto = NULL)
{
    ## Allow citation(auto = meta) in CITATION files to include
    ## auto-generated package citation.
    if(!is.null(auto) &&
       !is.logical(auto) &&
       !any(is.na(match(c("Package", "Version", "Title"),
                        names(meta <- as.list(auto))))) &&
       !all(is.na(match(c("Authors@R", "Author"),
                        names(meta))))
       ) {
        auto_was_meta <- TRUE
        package <- meta$Package
    } else {
        auto_was_meta <- FALSE
        dir <- system.file(package = package, lib.loc = lib.loc)
        if(dir == "")
            stop(packageNotFoundError(package, lib.loc, sys.call()))
        meta <- packageDescription(pkg = package,
                                   lib.loc = dirname(dir))
        ## if(is.null(auto)): Use default auto-citation if no CITATION
        ## available.
        citfile <- file.path(dir, "CITATION")
        test <- file_test("-f", citfile)
        if(!test) {                     # allow package source
            citfile <- file.path(dir, "inst", "CITATION")
            test <- file_test("-f", citfile)
        }
        if(is.null(auto)) auto <- !test
        ## if CITATION is available
        if(!auto) {
            return(readCitationFile(citfile, meta))
        }
    }

    ## Auto-generate citation info.

    ## Base packages without a CITATION file use the base citation.
    if((!is.null(meta$Priority)) && (meta$Priority == "base")) {
    	cit <- citation("base", auto = FALSE)
    	attr(cit, "mheader")[1L] <-
	    paste0("The ", sQuote(package), " package is part of R.  ",
		   attr(cit, "mheader")[1L])
        return(.citation(cit, package))
    }

    year <- sub("-.*", "", meta$`Date/Publication`)
    if(!length(year)) {
        if(is.null(meta$Date)) {
            warning(gettextf("no date field in DESCRIPTION file of package %s",
                             sQuote(package)),
                    domain = NA)
        } else {
            date <- trimws(as.vector(meta$Date))[1L]
            date <- strptime(date, "%Y-%m-%d", tz = "GMT")
            if(!is.na(date)) year <- format(date, "%Y")
        }
    }
    ## If neither Date/Publication nor Date work, try Packaged (build
    ## time stamp): if this fails too, use NA (PR #16550).
    if(!length(year)) {
        date <- as.POSIXlt(sub(";.*", "", trimws(meta$Packaged)[1L]))
        if(!is.na(date)) year <- format(date, "%Y")
    }
    if(!length(year)) {
        warning(gettextf("could not determine year for %s from package DESCRIPTION file",
                         sQuote(package)),
                domain = NA)
        year <- NA_character_
    }

    author <- meta$`Authors@R`
    ## <NOTE>
    ## Older versions took persons with no roles as "implied" authors.
    ## Now we only use persons with a name and a 'aut' role.  If there
    ## are none, we use persons with a name and a 'cre' role.
    ## If this still gives nothing (which really should not happen), we
    ## fall back to the plain text Author field.
    ## Checking will at least note the cases where there are no persons
    ## with names and 'aut' or 'cre' roles.
    if(length(author)) {
        aar <- .read_authors_at_R_field(author)
        author <- Filter(function(e) {
                             !(is.null(e$given) &&
                               is.null(e$family)) &&
                                 !is.na(match("aut", e$role))
                         },
                         aar)
        if(!length(author))
            author <- Filter(function(e) {
                                 !(is.null(e$given) &&
                                   is.null(e$family)) &&
                                     !is.na(match("cre", e$role))
                             },
                             aar)
    }
    if(length(author)) {
        has_authors_at_R_field <- TRUE
    } else {
        has_authors_at_R_field <- FALSE
        author <- as.personList(meta$Author)
    }
    ## </NOTE>

    z <- list(title = paste0(package, ": ", meta$Title),
              author = author,
              year = year,
              note = paste("R package version", meta$Version)
              )

    if(identical(meta$Repository, "CRAN"))
        z$url <-
            sprintf("https://CRAN.R-project.org/package=%s", package)

    if(identical(meta$Repository, "R-Forge")) {
        z$url <- if(!is.null(rfp <- meta$"Repository/R-Forge/Project"))
            sprintf("https://R-Forge.R-project.org/projects/%s/", rfp)
        else
            "https://R-Forge.R-project.org/"
        if(!is.null(rfr <- meta$"Repository/R-Forge/Revision"))
            z$note <- paste(z$note, rfr, sep = "/r")
    }

    if(!length(z$url) && !is.null(url <- meta$URL)) {
        ## Cannot have several URLs in BibTeX and bibentry object URL
        ## fields (PR #16240).
        if(grepl("[, ]", url))
            z$note <- url
        else
            z$url <- url
    }

    header <- if(!auto_was_meta) {
        gettextf("To cite package %s in publications use:",
                 sQuote(package))
    } else NULL


    ## No auto-generation message for auto was meta so that maintainers
    ## can safely use citation(auto = meta) in their CITATION without
    ## getting notified about possible needs for editing.
    footer <- if(!has_authors_at_R_field && !auto_was_meta) {
        gettextf("ATTENTION: This citation information has been auto-generated from the package DESCRIPTION file and may need manual editing, see %s.",
                 sQuote("help(\"citation\")"))
    } else NULL

    author <- format(z$author, include = c("given", "family"))
    if(length(author) > 1L)
        author <- paste(paste(head(author, -1L), collapse = ", "),
                        tail(author, 1L), sep = " and ")

    rval <- bibentry(bibtype = "Manual",
                     textVersion =
                     paste0(author, " (", z$year, "). ", z$title, ". ",
                            z$note, ". ", z$url),
                     header = header,
                     footer = footer,
                     other = z
                     )
    .citation(rval, package)
}

.citation <-
function(x, package = NULL)
    structure(x, package = package, class = c("citation", "bibentry"))

.read_authors_at_R_field <-
function(x)
{
    out <- if((Encoding(x) == "UTF-8") && !l10n_info()$"UTF-8") {
        con <- file()
        on.exit(close(con))
        writeLines(x, con, useBytes = TRUE)
        eval(parse(con, encoding = "UTF-8"))
    } else {
        eval(str2expression(x))
    }

    ## Let's by nice ...
    ## Alternatively, we could throw an error.
    if(!inherits(out, "person"))
        out <- do.call("c", lapply(x, as.person))

    out
}

.person_has_author_role <-
function(x)
{
    ## <NOTE>
    ## Earlier versions used
    ##    is.null(r <- x$role) || "aut" %in% r
    ## using author roles by default.
    ## </NOTE>
    "aut" %in% x$role
}

format.citation <-
function(x, style = "citation", ...) format.bibentry(x, style = style, ...)
print.citation <-
function(x, style = "citation", ...) print.bibentry(x, style = style, ...)

as.bibentry <-
function(x)
    UseMethod("as.bibentry")

as.bibentry.bibentry <- identity

as.bibentry.citation <-
function(x)
{
    class(x) <- "bibentry"
    x
}

.listify <-
function(x)
    if(inherits(x, "list")) x else list(x)

.format_person_for_plain_author_spec <-
function(x)
{
    ## Single person only.
    ## Give empty if person has no name or no role.
    if((is.null(x$given) && is.null(x$family)) || is.null(x$role))
        return("")
    format(x, include = c("given", "family", "role", "comment"))
}

.format_authors_at_R_field_for_author <-
function(x)
{
    if(is.character(x))
        x <- .read_authors_at_R_field(x)
    header <- attr(x, "header")
    footer <- attr(x, "footer")
    x <- sapply(x, .format_person_for_plain_author_spec)
    ## Drop persons with irrelevant roles.
    x <- x[nzchar(x)]
    ## And format.
    if(!length(x)) return("")
    ## We need to ensure that the first line has no indentation, whereas
    ## all subsequent lines are indented (as .write_description avoids
    ## folding for Author fields).  We use a common indentation of 2,
    ## with an extra indentation of 2 within single author descriptions.
    out <- paste(lapply(strwrap(x, indent = 0L, exdent = 4L,
                                simplify = FALSE),
                        paste, collapse = "\n"),
                 collapse = ",\n  ")
    if(!is.null(header)) {
        header <- paste(strwrap(header, indent = 0L, exdent = 2L),
                        collapse = "\n")
        out <- paste(header, out, sep = "\n  ")
    }
    if(!is.null(footer)) {
        footer <- paste(strwrap(footer, indent = 2L, exdent = 2L),
                        collapse = "\n")
        out <- paste(out, footer, sep = ".\n")
    }
    out
}

## preserves encoding if marked.
.format_authors_at_R_field_for_maintainer <-
function(x)
{
    if(is.character(x))
        x <- .read_authors_at_R_field(x)
    ## Maintainers need cre roles, valid email addresses and non-empty
    ## names.
    ## <FIXME>
    ## Check validity of email addresses.
    x <- Filter(function(e)
                (!is.null(e$given) || !is.null(e$family)) && !is.null(e$email) && ("cre" %in% e$role),
                x)
    ## </FIXME>
    ## If this leaves nothing or more than one ...
    if(length(x) != 1L) return("")
    display <- format(x, include = c("given", "family"))
    address <- format(x, include = c("email"),
                      collapse = list(email = FALSE))
    ## Need to quote display names at least when they contain commas
    ## (RFC 5322 <https://tools.ietf.org/html/rfc5322>).
    if(any(ind <- grepl(",", display))) {
        display[ind] <- sprintf("\"%s\"",
                                gsub("\"", "\\\"", display[ind], fixed=TRUE))
    }
    paste(display, address)
}

## Cite using the default style (which is usually citeNatbib)

cite <-
function(keys, bib, ...)
{
    fn <- tools::bibstyle()$cite
    if (is.null(fn))
    	fn <- citeNatbib
    fn(keys, bib, ...)
}

## Cite using natbib-like options.  A bibstyle would normally
## choose some of these options and just have a cite(keys, bib, previous)
## function within it.

citeNatbib <-
local({
    cited <- c()

    function(keys, bib, textual = FALSE, before = NULL, after = NULL,
             mode = c("authoryear", "numbers", "super"),
             abbreviate = TRUE, longnamesfirst = TRUE,
             bibpunct = c("(", ")", ";", "a", "", ","),
             previous) {

	shortName <- function(person) {
	    if (length(person$family))
		paste(tools:::cleanupLatex(person$family), collapse = " ")
	    else
		paste(tools:::cleanupLatex(person$given), collapse = " ")
	}

	authorList <- function(paper)
	    sapply(paper$author, shortName)

	if (!missing(previous))
	    cited <<- previous

	if (!missing(mode))
	    mode <- match.arg(mode)
	else
	    mode <- switch(bibpunct[4L],
	    	n = "numbers",
	    	s = "super",
	    	"authoryear")
        numeric <- mode %in% c('numbers', 'super')

	if (numeric)
	    bib <- sort(bib)

	keys <- unlist(strsplit(keys, " *, *"))
	if (!length(keys)) return("")

        n <- length(keys)
	first <- !(keys %in% cited)
	cited <<- unique(c(cited, keys))

	bibkeys <- unlist(bib$key)
	# Use year to hold numeric entry; makes things
	# simpler below
	year <- match(keys, bibkeys)
	papers <- bib[year]

        if (textual || !numeric) {
	    auth <- character(n)
	    if (!numeric)
	    	year <- unlist(papers$year)
	    authorLists <- lapply(papers, authorList)
	    lastAuthors <- NULL
	    for (i in seq_along(keys)) {
		authors <- authorLists[[i]]
		if (identical(lastAuthors, authors))
		    auth[i] <- ""
		else {
		    if (length(authors) > 1L)
			authors[length(authors)] <- paste("and", authors[length(authors)])
		    if (length(authors) > 2L) {
			if (!abbreviate || (first[i] && longnamesfirst))
			    auth[i] <- paste(authors, collapse=", ")
			else
			    auth[i] <- paste(authors[1L], "et al.")
		    } else
			auth[i] <- paste(authors, collapse=" ")
            	}
            	lastAuthors <- authors
            }
            suppressauth <- which(!nzchar(auth))
            if (length(suppressauth)) {
                for (i in suppressauth)
                    year[i - 1L] <-
                        paste0(year[i - 1L], bibpunct[6L], " ", year[i])
                auth <- auth[-suppressauth]
                year <- year[-suppressauth]
            }
        }
        if (!is.null(before))
            before <- paste0(before, " ")
        if (!is.null(after))
            after <- paste0(" ", after)
        if (textual) {
            result <- paste0(bibpunct[1L], before, year, after, bibpunct[2L])
            if (mode == "super")
            	result <- paste0(auth, "^{", result, "}")
            else
            	result <- paste0(auth, " ", result)
            result <- paste(result, collapse = paste0(bibpunct[3L], " "))
        } else if (numeric) {
            result <- paste(year, collapse=paste0(bibpunct[3L], " "))
            result <- paste0(bibpunct[1L], before, result, after, bibpunct[2L])
            if (mode == "super")
            	result <- paste0("^{", result, "}")
        } else {
            result <- paste0(auth, bibpunct[5L], " ", year)
            result <- paste(result, collapse = paste0(bibpunct[3L], " "))
            result <- paste0(bibpunct[1L], before, result, after, bibpunct[2L])
        }
        result
    }
})
