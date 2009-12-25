#  File src/library/utils/R/citation.R
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


citEntry <- function(entry, textVersion, header=NULL, footer=NULL, ...)
{
    z <- list(...)
    names(z) <- tolower(names(z))

    if("author" %in% names(z))
        z$author <- as.personList(z$author)

    attr(z, "entry") <- entry
    attr(z, "textVersion") <- textVersion
    attr(z, "header") <- header
    attr(z, "footer") <- footer
    class(z) <- "citation"
    z
}

citHeader <- function(...)
{
    z <- paste(...)
    class(z) <- "citationHeader"
    z
}

citFooter <- function(...)
{
    z <- paste(...)
    class(z) <- "citationFooter"
    z
}

## Keep tools:::get_CITATION_entry_fields is step with this
readCitationFile <- function(file, meta = NULL)
{
    if(is.null(encoding <- meta$Encoding))
        ## Assume latin1 as a default for now, but maybe switch to
        ## "unknown" eventually ...
        encoding <- "latin1"

    ## The parser can only read valid strings, but single-byte locales
    ## can mark their encoding.  The following allows latin1 and UTF-8
    ## citation files to be read in UTF-8 and any single-byte locale
    ## (including C).
    ##
    ##
    ## FIXME: if parse() could be told to read strings bytewise,
    ## we could simply convert to UTF-8.
    if(encoding %in% c("latin1", "UTF-8") && !l10n_info()$MBCS) {
        pcf <- parse(file = file, encoding = encoding)
    } else {
        con <- file(file, encoding = encoding)
        on.exit(close(con))
        pcf <- parse(con)
    }
    z <- list()
    k <- 0L
    envir <- new.env()
    ## Make the package metadata available to the citation entries.
    assign("meta", meta, envir = envir)

    for(expr in pcf) {
        x <- eval(expr, envir = envir)
        if(class(x) == "citation")
            z[[k <- k+1L]] <- x
        else if(class(x) == "citationHeader")
            attr(z, "header") <- c(attr(z, "header"), x)
        else if(class(x) == "citationFooter")
            attr(z, "footer") <- c(attr(z, "footer"), x)
    }
    class(z) <- "citationList"
    z
}

###**********************************************************

print.citation <-
function(x, bibtex = TRUE, ...)
{
    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }

    if(!is.null(attr(x, "textVersion"))){
        writeLines(strwrap(attr(x, "textVersion"), prefix="  "))
        cat("\n")
    }

    if(bibtex){
        cat("A BibTeX entry for LaTeX users is\n\n")
        print(toBibtex(x), prefix="  ")
    }

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }

    invisible(x)
}

print.citationList <- function(x, bibtex=length(x)==1, ...)
{
    cat("\n")
    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }
    for(y in x)
        print(y, bibtex=bibtex)

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }
    cat("\n")
    invisible(x)
}

###**********************************************************

person <- function(first="", last="", middle="", email="")
{
    z <- list(name=c(first=first, middle=middle, last=last),
              email=email)
    class(z) <- "person"
    z
}

as.person <- function(x) UseMethod("as.person")

as.person.default <- function(x)
{
    if(class(x)=="person") return(x)

    x <- as.character(x)

    if(length(grep("<.*>", x)))
        email <- sub(".*<([^>]*)>.*", "\\1", x)
    else
        email <- NULL

    name <- sub("[[:space:]]*<[^>]*>", "", x)
    name = unlist(strsplit(name, "[[:space:]]"))

    ## fix for email address only
    if(length(name) == 0L) name = ""

    ## and now create appropriate person objects
    if(length(name) == 1L)
        z <- person(last = name, email = email)
    else if(length(name) == 2L)
        z <- person(first = name[1L], last = name[2L], email = email)
    else
        z <- person(first = name[1L],
                    last = name[length(name)],
                    middle = paste(name[-c(1L, length(name))],
                    collapse = " "),
                    email = email)
    z
}

personList <- function(...)
{
    z = list(...)
    if(any(sapply(z, function(x) class(x) != "person")))
        stop("all arguments must be of class \"person\"")

    class(z) <- "personList"
    z
}

as.personList <- function(x) UseMethod("as.personList")

as.personList.person <- function(x) personList(x)

as.personList.default <- function(x)
{
    if(class(x)=="personList") return(x)

    x <- as.character(x)

    ## first split into individual persons
    x <- unlist(strsplit(x,"[[:space:]]?(,|[[:space:]]and)[[:space:]]+"))
    x <- x[nzchar(x)]

    z <- list()
    for(k in seq_along(x)) z[[k]] <- as.person(x[k])
    class(z) <- "personList"
    z
}

as.character.person <- function(x, ...)
{
    paste(x$name[nzchar(x$name)], collapse=" ")
}

as.character.personList <- function(x, ...)
{
    sapply(x, as.character)
}

###**********************************************************

toBibtex.person <- function(object, ...)
{
    if(length(grep(" ", object$name["last"]))>0)
        object$name["last"] <- paste("{", object$name["last"], "}", sep="")
    as.character(object)
}

toBibtex.personList <- function(object, ...)
{
    z <- sapply(object, toBibtex)
    paste(z, collapse = " and ")
}

toBibtex.citation <- function(object, ...)
{
    z <- paste("@", attr(object, "entry"), "{,", sep="")

    if("author" %in% names(object)){
        object$author <- toBibtex(object$author)
    }

    for(n in names(object))
        z <- c(z, paste("  ", n, " = {", object[[n]], "},", sep=""))

    z <- c(z, "}")
    class(z) <- "Bibtex"
    z
}

toBibtex.citationList <- function(object, ...)
{
    lapply(object, toBibtex)
}

###**********************************************************

citation <-
function(package = "base", lib.loc = NULL)
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    if(dir == "")
        stop(gettextf("package '%s' not found", package), domain = NA)

    meta <- packageDescription(pkg = package, lib.loc = dirname(dir))
    citfile <- file.path(dir, "CITATION")
    if(file_test("-f", citfile)) return(readCitationFile(citfile, meta))
    else if(package == "base") {
        ## Avoid infinite recursion for broken installation.
        stop("broken installation, no CITATION file in the base package.")
    }

    ## Auto-generate citation info.

    ## Base packages without a CITATION file use the base citation.
    if((!is.null(meta$Priority)) && (meta$Priority == "base")) {
    	cit <- citation("base")
    	attr(cit, "header")[1L] <-
            paste("The '", package, "' package is part of R.  ",
                  attr(cit, "header")[1L], sep = "")
    	return(cit)
    }

    year <- sub("-.*", "", meta$`Date/Publication`)
    if(!length(year)) {
        year <- sub(".*((19|20)[[:digit:]]{2}).*", "\\1", meta$Date)
        if(is.null(meta$Date)){
            warning(gettextf("no date field in DESCRIPTION file of package '%s'",
                             package),
                    domain = NA)
        }
        else if(!length(year)) {
            warning(gettextf("could not determine year for '%s' from package DESCRIPTION file",
                             package),
                    domain = NA)
        }
    }

    z <- list(title = paste(package, ": ", meta$Title, sep=""),
              author = as.personList(meta$Author),
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

    class(z) <- "citation"
    attr(z, "entry") <- "Manual"
    attr(z, "package") <- package

    attr(z, "header") <-
        paste("To cite package", sQuote(package), "in publications use:")

    if(! "recommended" %in% meta$Priority) # we assume those are OK
        attr(z, "footer") <-
            paste("ATTENTION: This citation information has been auto-generated",
                  "from the package DESCRIPTION file and may need manual editing,",
                  "see ", sQuote("help(\"citation\")"), ".")

    author <- as.character(z$author)
    if(length(author) > 1L)
        author <- paste(paste(author[1L:(length(author)-1L)], collapse=", "),
                        author[length(author)], sep=" and ")

    attr(z, "textVersion") <-
        paste(author, " (", z$year, "). ",
              z$title, ". ", z$note, ". ", z$url, sep="")

    z
}
