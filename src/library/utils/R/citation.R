#  File src/library/utils/R/citation.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

## What a silly name ...
.is_not_nonempty_text <-
function(x)
    is.null(x) || any(is.na(x)) || all(grepl("^[[:space:]]*$", x))

person <-
function(given = NULL, family = NULL, middle = NULL,
         email = NULL, role = NULL, comment = NULL,
         first = NULL, last = NULL)
{
    ## Arrange all arguments in lists of equal length.
    args <- list(given = given, family = family, middle = middle,
                 email = email, role = role, comment = comment,
		 first = first, last = last)
    args <- lapply(args, .listify)
    args_length <- sapply(args, length)
    if(!all(args_length_ok <- args_length %in% c(1L, max(args_length))))
        warning(sprintf("Not all arguments are of the same length, the following need to be recycled: %s",
	    paste(names(args)[!args_length_ok], collapse = ", ")))
    args <- lapply(args, function(x) rep(x, length.out = max(args_length)))

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
                warning(gettextf("Invalid role specification(s): %s.",
                                 paste(sQuote(role), collapse = ", ")))
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

        rval <- list(given = given, family = family, role = role,
                     email = email, comment = comment)
        ## Canonicalize 0-length character arguments to NULL.
        if(any(ind <- (sapply(rval, length) == 0L)))
            rval[ind] <- vector("list", length = sum(ind))

        return(rval)
    }

    rval <-
        lapply(seq_along(args$given),
               function(i)
               with(args,
                    person1(given = given[[i]], family = family[[i]],
                            middle = middle[[i]], email = email[[i]],
                            role = role[[i]], comment = comment[[i]],
                            first = first[[i]], last = last[[i]])))
    class(rval) <- "person"

    ## <COMMENT Z>
    ## Should we check that for each person there is at least one
    ## non-NULL entry?
    ## </COMMENT>

    rval
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
            warning(gettextf("Invalid role specification(s): %s.",
                             paste(sQuote(role[pos[ind]]),
                                   collapse = ", ")))
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
    x_char <- sapply(X = x, FUN = format, ...)
    print(x_char)
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
        if(any(ind <- (sapply(rval, length) == 0L)))
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
    value <- rep(value, length.out = length(x))

    if(name == "role")
        value <- lapply(value, .canonicalize_person_role)

    for(i in seq_along(x))
        x[[i]][[name]] <- if(.is_not_nonempty_text(value[[i]])) NULL else as.character(value[[i]])

    class(x) <- "person"
    x
}

c.person <-
function(..., recursive = FALSE)
{
    args <- list(...)
    if(!all(sapply(args, inherits, "person")))
        warning(gettextf("method is just applicable to %s objects",
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

    x <- as.character(x)

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
    x <- x[!sapply(x, .is_not_nonempty_text)]

    ## Step C.
    as_person1 <- function(x) {
        comment <- if(grepl("\\(.*\\)", x))
            sub(".*\\(([^)]*)\\).*", "\\1", x)
        else NULL
        x <- sub("[[:space:]]*\\([^)]*\\)", "", x)
        email <- if(grepl("<.*>", x))
            sub(".*<([^>]*)>.*", "\\1", x)
        else NULL
        x <- sub("[[:space:]]*<[^>]*>", "", x)
        role <- if(grepl("\\[.*\\]", x))
            unlist(strsplit(gsub("[[:space:]]*", "",
                                 sub(".*\\[([^]]*)\\].*", "\\1", x)),
                            ",", fixed = TRUE))
        else NULL
        x <- sub("[[:space:]]*\\[[^)]*\\]", "", x)
        x <- unlist(strsplit(x, "[[:space:]]+"))
        z <- person(given = x[-length(x)], family = x[length(x)],
                    email = email, role = role, comment = comment)
        return(z)
    }

    as.list(do.call("c", lapply(x, as_person1)))
}

personList <-
function(...)
{
    z <- list(...)
    if(!all(sapply(z, inherits, "person")))
        stop("all arguments must be of class \"person\"")
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
         ...
         )
{
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
    braces <- lapply(braces, rep, length.out = 2L)
    if(is.null(collapse$given)) collapse$given <- " "
    if(is.null(collapse$family)) collapse$family <- " "
    if(is.null(collapse$email)) collapse$email <- ", "
    if(is.null(collapse$role)) collapse$role <- ", "
    if(is.null(collapse$comment)) collapse$comment <- ", "
    collapse <- lapply(collapse, rep, length.out = 1L)

    ## extract selected elements
    x <- lapply(unclass(x), "[", include)
    braces <- braces[include]
    collapse <- collapse[include]

    ## format 1 person
    format_person1 <- function(p) {
	rval <- lapply(seq_along(p), function(i) if(is.null(p[[i]])) NULL else
		       paste0(braces[[i]][1L], paste(p[[i]], collapse = collapse[[i]]),
			      braces[[i]][2L]))
	paste(do.call("c", rval), collapse = " ")
    }

    sapply(x, format_person1)
}

as.character.person <-
function(x, ...)
    format(x, ...)

toBibtex.person <-
function(object, ...)
    paste(format(object, include = c("given", "family")),
          collapse = " and ")

######################################################################

bibentry <-
function(bibtype, textVersion = NULL, header = NULL, footer = NULL, key = NULL,
         ...,
         other = list(), mheader = NULL, mfooter = NULL)
{
    BibTeX_names <- names(tools:::BibTeX_entry_field_db)

    args <- c(list(...), other)
    if(length(args) < 1L)
        stop("at least one field has to be specified")
    if(any(sapply(names(args), .is_not_nonempty_text))) stop("all fields have to be named")

    ## arrange all arguments in lists of equal length
    args <- c(list(bibtype = bibtype, textVersion = textVersion,
              header = header, footer = footer, key = key), list(...))
    args <- lapply(args, .listify)
    other <- lapply(other, .listify)
    max_length <- max(sapply(c(args, other), length))

    args_length <- sapply(args, length)
    if(!all(args_length_ok <- args_length %in% c(1L, max_length)))
        warning(sprintf("Not all arguments are of the same length, the following need to be recycled: %s",
	    paste(names(args)[!args_length_ok], collapse = ", ")))
    args <- lapply(args, function(x) rep(x, length.out = max_length))

    other_length <- sapply(other, length)
    if(!all(other_length_ok <- other_length %in% c(1L, max_length)))
        warning(sprintf("Not all arguments are of the same length, the following need to be recycled: %s",
	    paste(names(other)[!other_length_ok], collapse = ", ")))
    other <- lapply(other, function(x) rep(x, length.out = max_length))

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
        rval <- rval[!sapply(rval, .is_not_nonempty_text)]
	fields <- tolower(names(rval))
        names(rval) <- fields

        ## required fields
        rfields <- strsplit(tools:::BibTeX_entry_field_db[[pos]], "|",
                            fixed = TRUE)
        if(length(rfields) > 0L) {
            ok <- sapply(rfields, function(f) any(f %in% fields))
	    if(any(!ok))
                stop(gettextf("A bibentry of bibtype %s has to correctly specify the field(s): %s",
                              sQuote(bibtype),
                              paste(rfields[!ok], collapse = ", ")),
                     domain = NA)
        }
        pos <- fields %in% c("author", "editor")
	if(any(pos)) {
            for(i in which(pos)) rval[[i]] <- as.person(rval[[i]])
	}
	if(any(!pos)) {
            for(i in which(!pos)) rval[[i]] <- as.character(rval[[i]])
	}

        ## set attributes
        attr(rval, "bibtype") <- bibtype
        attr(rval, "key") <- if(is.null(key)) NULL else as.character(key)
        if(!is.null(textVersion)) attr(rval, "textVersion") <- as.character(textVersion)
        if(!.is_not_nonempty_text(header))
            attr(rval, "header") <- paste(header, collapse = "\n")
        if(!.is_not_nonempty_text(footer))
            attr(rval, "footer") <- paste(footer, collapse = "\n")

        return(rval)
    }

    rval <- lapply(seq_along(args$bibtype),
                   function(i)
                   do.call("bibentry1",
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

bibentry_attribute_names <-
    c("bibtype", "textVersion", "header", "footer", "key")

`[[.bibentry` <-
`[.bibentry` <-
function(x, i)
{
    rval <- unclass(x)[i]
    class(rval) <- class(x)
    rval
}

bibentry_format_styles <-
    c("text", "Bibtex", "citation", "html", "latex", "textVersion", "R")

.match_bibentry_format_style <-
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
function(x, style = "text", .bibstyle = "JSS", ...)
{
    style <- .match_bibentry_format_style(style)

    .format_bibentry_via_Rd <- function(f) {
        out <- file()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({tools::Rd2txt_options(saveopt); close(out)})
        sapply(x,
               function(y) {
                   rd <- tools::toRd(y, style = .bibstyle)
                   con <- textConnection(rd)
                   on.exit(close(con))
                   f(con, fragment = TRUE, out = out, ...)
                   paste(readLines(out), collapse = "\n")
               })
    }

    .format_bibentry_as_citation <- function(x) {
        bibtex <- length(x) < 2L

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
                          c("\nA BibTeX entry for LaTeX users is\n",
			    paste0("  ", unclass(toBibtex(y))))
                      },
                      if(!is.null(y$footer))
                      c("", strwrap(y$footer))),
                    collapse = "\n")
          })),
          paste(strwrap(attr(x, "mfooter")), collapse = "\n")
          )
    }

    switch(style,
           "text" = .format_bibentry_via_Rd(tools::Rd2txt),
           "html" = .format_bibentry_via_Rd(tools::Rd2HTML),
           "latex" = .format_bibentry_via_Rd(tools::Rd2latex),
           "Bibtex" = {
               unlist(lapply(x,
                             function(y)
                             paste(toBibtex(y), collapse = "\n")))
           },
           "textVersion" = {
               out <- lapply(unclass(x), attr, "textVersion")
               out[!sapply(out, length)] <- ""
               unlist(out)
           },
           "citation" = .format_bibentry_as_citation(x),
           "R" = .format_bibentry_as_R_code(x, ...)
           )
}

print.bibentry <-
function(x, style = "text", .bibstyle = "JSS", ...)
{
    style <- .match_bibentry_format_style(style)

    if(style == "R") {
        writeLines(format(x, "R", collapse = TRUE))
    } else {
        y <- format(x, style)
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

## Not vectorized for now: see ?regmatches for a vectorized version.
.blanks <-
function(n)
    paste(rep.int(" ", n), collapse = "")

.format_call_RR <-
function(cname, cargs)
{
    ## Format call with ragged right argument list (one arg per line).
    cargs <- as.list(cargs)
    n <- length(cargs)
    lens <- sapply(cargs, length)
    sums <- cumsum(lens)
    starters <- c(sprintf("%s(", cname),
                  rep.int(.blanks(nchar(cname) + 1L), sums[n] - 1L))
    trailers <- c(rep.int("", sums[n] - 1L), ")")
    trailers[sums[-n]] <- ","
    sprintf("%s%s%s", starters, unlist(cargs), trailers)
}

.format_bibentry_as_R_code <-
function(x, collapse = FALSE)
{
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
                        rep.int(.blanks(nchar(prefix)), n - 1L))
        sprintf("%s%s", prefix, v)
    }

    s <- lapply(unclass(x),
                function(e) {
                    a <- Filter(length, attributes(e)[anames])
                    e <- e[!sapply(e, is.null)]
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
                     paste("mheader = ", deparse(mheader)))
    if(!is.null(mfooter <- attr(x, "mfooter")))
        s[[1L]] <- c(s[[1L]],
                     paste("mfooter = ", deparse(mfooter)))

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
                    e <- e[!sapply(e, is.null)]
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
    value <- rep(.listify(value), length.out = length(x))

    ## check bibtype
    if(name == "bibtype") {
      stopifnot(all(sapply(value, length) == 1L))
      BibTeX_names <- names(tools:::BibTeX_entry_field_db)
      value <- unlist(value)
      pos <- match(tolower(value), tolower(BibTeX_names))
      if(any(is.na(pos)))
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
                    if(name %in% c("author", "editor")) as.person(value[[i]]) else paste(value[[i]])
                }
        }
    }

    ## check whether all elements still have their required fields
    check_bibentry1 <- function(x) {
	fields <- names(x)
	bibtype <- attr(x, "bibtype")
        rfields <-
            strsplit(tools:::BibTeX_entry_field_db[[bibtype]], "|",
                     fixed = TRUE)
        if(length(rfields) > 0L) {
            ok <- sapply(rfields, function(f) any(f %in% fields))
	    if(any(!ok))
                stop(gettextf("A bibentry of bibtype %s has to specify the field(s): %s",
                              sQuote(bibtype),
                              paste(rfields[!ok], collapse = ", ")),
                     domain = NA)
        }
    }
    for(i in seq_along(x)) check_bibentry1(x[[i]])

    class(x) <- "bibentry"
    x
}

c.bibentry <-
function(..., recursive = FALSE)
{
    args <- list(...)
    if(!all(sapply(args, inherits, "bibentry")))
        warning(gettextf("method is just applicable to %s objects",
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
	only_given_or_family <- is.null(fnms) || is.null(p$given)
	fbrc <- if(length(fnms) > 1L ||
                   any(grepl("[[:space:]]", fnms)) ||
                   only_given_or_family) c("{", "}") else ""
	gbrc <- if(only_given_or_family) c("{", "}") else ""
        format(p, include = c("given", "family"), braces = list(given = gbrc, family = fbrc))
    }), collapse = " and ")

    format_bibentry1 <- function(object) {
	object <- unclass(object)[[1L]]
        rval <- paste0("@", attr(object, "bibtype"), "{", attr(object, "key"), ",")
        if("author" %in% names(object)) object$author <- format_author(object$author)
        if("editor" %in% names(object)) object$editor <- format_author(object$editor)

        rval <- c(rval,
            sapply(names(object), function (n)
                   paste0("  ", n, " = {", object[[n]], "},")),
    	"}", "")
        return(rval)
    }

    rval <- head(unlist(lapply(object, format_bibentry1)), -1L)
    class(rval) <- "Bibtex"
    rval
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
    exprs <- tools:::.parse_CITATION_file(file, meta$Encoding)

    rval <- list()
    mheader <- NULL
    mfooter <- NULL
    k <- 0L
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

    rval <- if(length(rval) == 1L)
        rval[[1L]]
    else
        do.call("c", rval)
    if(!.is_not_nonempty_text(mheader))
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if(!.is_not_nonempty_text(mfooter))
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")

    .citation(rval)
}

######################################################################

citation <-
function(package = "base", lib.loc = NULL, auto = NULL)
{
    ## Allow citation(auto = meta) in CITATION files to include
    ## auto-generated package citation.
    if(inherits(auto, "packageDescription")) {
        auto_was_meta <- TRUE
        meta <- auto
        package <- meta$Package
    } else {
        auto_was_meta <- FALSE
        dir <- system.file(package = package, lib.loc = lib.loc)
        if(dir == "")
            stop(gettextf("package %s not found", sQuote(package)),
                 domain = NA)
        meta <- packageDescription(pkg = package,
                                   lib.loc = dirname(dir))
        ## if(is.null(auto)): Use default auto-citation if no CITATION
        ## available.
        citfile <- file.path(dir, "CITATION")
        if(is.null(auto)) auto <- !file_test("-f", citfile)
        ## if CITATION is available
        if(!auto) {
            return(readCitationFile(citfile, meta))
        } else if(package == "base") {
            ## Avoid infinite recursion for broken installation.
            stop("broken installation, no CITATION file in the base package.")
        }
    }

    ## Auto-generate citation info.

    ## Base packages without a CITATION file use the base citation.
    if((!is.null(meta$Priority)) && (meta$Priority == "base")) {
    	cit <- citation("base", auto = FALSE)
    	attr(cit, "mheader")[1L] <-
	    paste0("The ", sQuote(package), " package is part of R.  ",
		   attr(cit, "mheader")[1L])
        return(.citation(cit))
    }

    year <- sub("-.*", "", meta$`Date/Publication`)
    if(!length(year)) {
        year <- sub(".*((19|20)[[:digit:]]{2}).*", "\\1", meta$Date,
                    perl = TRUE) # may not be needed, but safer
        if(is.null(meta$Date)){
            warning(gettextf("no date field in DESCRIPTION file of package %s",
                             sQuote(package)),
                    domain = NA)
        }
        else if(!length(year)) {
            warning(gettextf("could not determine year for %s from package DESCRIPTION file",
                             sQuote(package)),
                    domain = NA)
        }
    }

    author <- meta$`Authors@R`
    ## <FIXME>
    ## Temporarily support Author@R fields ...
    if(is.null(author))
        author <- meta$`Author@R`
    ## </FIXME>
    ## <FIXME>
    ## Older versions took persons with no roles as "implied" authors.
    ## So for now check whether Authors@R gives any authors; if not fall
    ## back to the plain text Author field.
    if(length(author)) {
        author <- .read_authors_at_R_field(author)
        ## We only want those with author roles.
        author <- Filter(.person_has_author_role, author)
    }
    if(length(author)) {
        has_authors_at_R_field <- TRUE
    } else {
        has_authors_at_R_field <- FALSE
        author <- as.personList(meta$Author)
    }
    ## </FIXME>

    z <- list(title = paste0(package, ": ", meta$Title),
              author = author,
              year = year,
              note = paste("R package version", meta$Version)
              )

    z$url <- if(identical(meta$Repository, "CRAN"))
        sprintf("http://CRAN.R-project.org/package=%s", package)
    else
        meta$URL

    if(identical(meta$Repository, "R-Forge")) {
        z$url <- if(!is.null(rfp <- meta$"Repository/R-Forge/Project"))
            sprintf("http://R-Forge.R-project.org/projects/%s/", rfp)
        else
            "http://R-Forge.R-project.org/"
        if(!is.null(rfr <- meta$"Repository/R-Forge/Revision"))
            z$note <- paste(z$note, rfr, sep = "/r")
    }

    header <- if(!auto_was_meta) {
        paste("To cite package",
              sQuote(package),
              "in publications use:")
    } else NULL


    ## No auto-generation message for auto was meta so that maintainers
    ## can safely use citation(auto = meta) in their CITATION without
    ## getting notified about possible needs for editing.
    footer <- if(!has_authors_at_R_field && !auto_was_meta) {
        paste("ATTENTION: This citation information has been auto-generated",
              "from the package DESCRIPTION file and may need manual editing,",
              "see ", sQuote("help(\"citation\")"), ".")
    } else NULL

    author <- format(z$author, include = c("given", "family"))
    if(length(author) > 1L)
        author <- paste(paste(head(author, -1L), collapse = ", "),
                        tail(author, 1L), sep = " and ")

    rval <- bibentry(
        bibtype = "Manual",
	textVersion = paste0(author, " (",z$year,"). ", z$title,". ", z$note,". ", z$url),
        header = header,
	footer = footer,
	other = z
    )
    .citation(rval)
}

.citation <- function(x)
{
    class(x) <- c("citation", "bibentry")
    x
}

.read_authors_at_R_field <-
function(x)
{
    out <- eval(parse(text = x))

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

print.citation <-
function(x, style = "citation", ...)
{
    NextMethod("print", x, style = style, ...)
    invisible(x)
}

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
function(x) {
    ## Names first.
    out <- format(x, include = c("given", "family"))
    ## Only show roles recommended for usage with R.
    role <- x$role
    if(!length(role)) return("")
    role <- role[role %in% MARC_relator_db_codes_used_with_R]
    if(!length(role)) return("")
    out <- sprintf("%s [%s]", out, paste(role, collapse = ", "))
    if(!is.null(comment <- x$comment))
        out <- sprintf("%s (%s)", out,
                       paste(comment, collapse = "\n"))
    out
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
    x <- x[x != ""]
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

.format_authors_at_R_field_for_maintainer <-
function(x)
{
    if(is.character(x))
        x <- .read_authors_at_R_field(x)
    ## Maintainers need cre roles and email addresses.
    x <- Filter(function(e)
                !is.null(e$email) && ("cre" %in% e$role),
                x)
    ## If this leaves nothing ...
    if(!length(x)) return("")
    paste(format(x, include = c("given", "family", "email")),
          collapse = ",\n  ")
}
