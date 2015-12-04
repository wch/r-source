#  File src/library/utils/R/prompt.R
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

prompt <-
function(object, filename = NULL, name = NULL, ...)
    UseMethod("prompt")

prompt.default <-
function(object, filename = NULL, name = NULL,
         force.function = FALSE, ...)
{
    is.missing.arg <- function(arg)
        typeof(arg) == "symbol" && deparse(arg) == ""

    if(missing(name))
        name <- if(is.character(object))
            object
        else {
            name <- substitute(object)
            ## <FIXME>
            ## This used to be:
            ##     if(is.language(name) && !is.name(name))
            ##         name <- eval(name)
            ##     as.character(name)
            ## but what is this trying to do?
            ## It seems that the eval() will typically give the given
            ## object, and surely we cannot use that as the name (even
            ## if the subsequent as.character() does not fail ...)
            ## Better to be defensive about this, and handle only cases
            ## we know will make sense ...
            if(is.name(name))
                as.character(name)
            else if(is.call(name)
                    && (as.character(name[[1L]]) %in%
                        c("::", ":::", "getAnywhere"))) {
                name <- as.character(name)
                name[length(name)]
            }
            else
                stop("cannot determine a usable name")
            ## </FIXME>
        }

    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## <FIXME>
    ## If not a function or forced to document a function (?), always
    ## assume data set.
    if(!(is.function(x) || force.function))
        return(promptData(x, filename = filename, name = name))
    ## </FIXME>

    n <- length(argls <- formals(x))
    if(n) {
        arg.names <- arg.n <- names(argls)
        arg.n[arg.n == "..."] <- "\\dots"
    }
    ## Construct the 'call' for \usage.
    Call <- paste0(name, "(")
    for(i in seq_len(n)) {                       # i-th argument
        Call <- paste0(Call, arg.names[i],
                       if(!is.missing.arg(argls[[i]]))
                       paste0(" = ",
                              ## need to backtick symbols
                              paste(deparse(argls[[i]],
                                            backtick = TRUE,
                                            width.cutoff = 500L),
                                    collapse="\n")))
        if(i != n) Call <- paste0(Call, ", ")
    }

    ## Construct the definition for \examples.
    x.def <- deparse(x)
    if(any(br <- substr(x.def, 1L, 1L) == "}"))
        x.def[br] <- paste(" ", x.def[br])

    ## escape "%" :
    x.def <- gsub("%", "\\\\%", x.def)
    Call <- gsub("%", "\\\\%", Call)

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
#             version = "\\Rdversion{1.1}",
             aliases = c(paste0("\\alias{", name, "}"),
             paste("%- Also NEED an '\\alias' for EACH other topic",
                   "documented here.")),
             title = "\\title{\n%%  ~~function to do ... ~~\n}",
             description = c("\\description{",
             paste("%%  ~~ A concise (1-5 lines) description of what",
                   "the function does. ~~"),
             "}"),
             usage = c("\\usage{", paste0(Call, ")"), "}",
             paste("%- maybe also 'usage' for other objects",
                   "documented here.")),
             arguments = NULL,
             details = c("\\details{",
             paste("%%  ~~ If necessary, more details than the",
                   "description above ~~"),
             "}"),
             value = c("\\value{",
             "%%  ~Describe the value returned",
             "%%  If it is a LIST, use",
             "%%  \\item{comp1 }{Description of 'comp1'}",
             "%%  \\item{comp2 }{Description of 'comp2'}",
             "%% ...",
             "}"),
             references = paste("\\references{\n%% ~put references to the",
             "literature/web site here ~\n}"),
             author = "\\author{\n%%  ~~who you are~~\n}",
             note = c("\\note{\n%%  ~~further notes~~\n}",
             "",
             paste("%% ~Make other sections like Warning with",
                   "\\section{Warning }{....} ~"),
             ""),
             seealso = paste("\\seealso{\n%% ~~objects to See Also as",
             "\\code{\\link{help}}, ~~~\n}"),
             examples = c("\\examples{",
             "##---- Should be DIRECTLY executable !! ----",
             "##-- ==>  Define data, use random,",
             "##--	or do  help(data=index)  for the standard data sets.",
             "",
             "## The function is currently defined as",
             x.def,
             "}"),
             keywords = c(paste("% Add one or more standard keywords,",
             "see file 'KEYWORDS' in the"),
             "% R documentation directory.",
             "\\keyword{ ~kwd1 }% use one of  RShowDoc(\"KEYWORDS\")",
             "\\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line"))

    Rdtxt$arguments <- if(n)
        c("\\arguments{",
          paste0("  \\item{", arg.n, "}{",
                 "\n%%     ~~Describe \\code{", arg.n, "} here~~\n}"),
          "}") ## else NULL

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named %s.", sQuote(filename)),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}

prompt.data.frame <-
function(object, filename = NULL, name = NULL, ...)
{
    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.name(name))
                    as.character(name)
                else
                    stop("cannot determine a usable name")
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## <FIXME>
    ## Always assume data set ???
    promptData(x, filename = filename, name = name)
    ## </FIXME>
}

promptData <-
function(object, filename = NULL, name = NULL)
{
    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.name(name))
                    as.character(name)
                else
                    stop("cannot determine a usable name")
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    ## Construct the format.
    if(is.data.frame(x)) {

        make_item_tag <- function(s) {
            ## For syntactic names, use \code; otherwise, use \samp.
            if(grepl("^([[:alpha:]]|[.][[:alpha:]._])[[:alnum:]._]*$", s)) {
                paste0("\\code{", s, "}")
            } else {
                paste0("\\samp{", gsub("([%{}])", "\\\\\\1", s), "}")
            }
        }

        fmt <- c("\\format{",
                 paste("  A data frame with",
                       nrow(x),
                       "observations on the following",
                       ifelse(ncol(x) == 1,
                              "variable.",
                              paste(ncol(x), "variables."))),
                 "  \\describe{")
        for(i in names(x)) {
            xi <- x[[i]]
            fmt <-
                c(fmt,
                  paste0("    \\item{", make_item_tag(i), "}{",
                         if(inherits(xi, "ordered")) {
                             paste("an", data.class(xi),
                                   "factor with levels",
                                   paste0("\\code{", levels(xi), "}",
                                          collapse = " < "),
                                   collapse = " ")
                         } else if(inherits(xi, "factor")) {
                             paste("a factor with levels",
                                   paste0("\\code{", levels(xi), "}",
                                          collapse = " "),
                                   collapse = " ")
                         } else if(is.vector(xi)) {
                             paste("a", data.class(xi), "vector")
                         } else if(is.matrix(xi)) {
                             paste("a matrix with", ncol(xi), "columns")
                         } else {
                             paste("a", data.class(xi))
                         },
                         "}"))
        }
        fmt <- c(fmt, "  }", "}")
    }
    else {
        tf <- tempfile(); on.exit(unlink(tf))
        sink(tf) ; str(object) ; sink()
        fmt <- c("\\format{",
                 "  The format is:",
                 scan(tf, "", quiet = !getOption("verbose"), sep = "\n"),
                 "}")
    }

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
#             version = "\\Rdversion{1.1}",
             aliases = paste0("\\alias{", name, "}"),
             docType = "\\docType{data}",
             title = "\\title{\n%%   ~~ data name/kind ... ~~\n}",
             description = c("\\description{",
             "%%  ~~ A concise (1-5 lines) description of the dataset. ~~",
             "}"),
             usage = paste0("\\usage{data(\"", name, "\")}"),
             format = fmt,
             details = c("\\details{",
             paste("%%  ~~ If necessary, more details than the",
                   "__description__ above ~~"),
             "}"),
             source = c("\\source{",
             paste("%%  ~~ reference to a publication or URL",
                   "from which the data were obtained ~~"),
             "}"),
             references = c("\\references{",
             "%%  ~~ possibly secondary sources and usages ~~",
             "}"),
             examples = c("\\examples{",
             paste0("data(", name, ")"),
             paste0("## maybe str(", name, ") ; plot(", name, ") ..."),
             "}"),
             keywords = "\\keyword{datasets}")

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named %s.", sQuote(filename)),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}

promptPackage <-
function(package, lib.loc = NULL, filename = NULL, name = NULL, final = FALSE)
{
    ## Most of this should not be translated -- PR#11191

    ## lib.loc is not used any more
    ## if (is.null(lib.loc)) lib.loc <- .libPaths()

    insert1 <- function(field, new) {
    	prev <- Rdtxt[[field]]
    	Rdtxt[[field]] <<- c(prev[-length(prev)], new, prev[length(prev)])
    }
    insert2 <- function(field, new) insert1(field, paste("~~", new, "~~"))

    if(missing(name))
	name <- paste0(package, "-package")

    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    Rdtxt <-
    	    list(name = paste0("\\name{", name, "}"),
#                version = "\\Rdversion{1.1}",
    	         aliases = c(paste0("\\alias{", name, "}"),
                             c(paste0("\\alias{", package, "}"))),
    	         docType = "\\docType{package}",
    	         title = c("\\title{", "}"),
    	         description = c("\\description{","}"),
    	         details = c("\\details{","}"),
    	         author = c("\\author{","}"),
    	         references = character(0L),

    	         keywords = c("\\keyword{ package }")
    	     )

    insert1("title", paste0("\\packageTitle{", package,"}"))
    insert1("description", paste0("\\packageDescription{", package,"}"))
    insert1("author", c(paste0("\\packageAuthor{", package,"}"), "",
			paste("Maintainer:",paste0("\\packageMaintainer{", package,"}"))))
    insert1("details", c("", "The DESCRIPTION file:"))
    insert1("details", paste0("\\packageDESCRIPTION{", package, "}"))
    insert1("details", paste0("\\packageIndices{", package, "}"))

    if (!final) {
        insert2("details", strwrap(
	 "An overview of how to use the package, including the most important functions"))
        Rdtxt$references <-
            c("\\references{",
              paste("~~",
                    "Literature or other references for background information",
                    "~~"),
              "}")
        Rdtxt$seealso <- c("\\seealso{", "}")
        insert2("seealso",
                c("Optional links to other man pages, e.g.",
                  "\\code{\\link[<pkg>:<pkg>-package]{<pkg>}}"))
        Rdtxt$examples <- c("\\examples{","}")
        insert2("examples",
                "simple examples of the most important functions")
        insert2("keywords",
                strwrap("Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory"))
    }

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named %s.", sQuote(filename)),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)

    invisible(filename)
}

promptImport <- function(object, filename = NULL, name = NULL, importedFrom = NULL,
                         importPage = name, ...)
{
    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.name(name))
                    as.character(name)
                else if (is.language(name) && length(name) == 3 &&
                         identical(name[[1]], as.name("::")))
                    as.character(name[[3]])
                else
                    stop("cannot determine a usable name")
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- if(!missing(object))
        object
    else {
        ## Better than get(); works when called in fun :
        x <- get(name, envir = parent.frame())
    }

    if(is.null(importedFrom)) {
	if (is.function(x))
	    importedFrom <- getNamespaceName(environment(x))
	else
	    stop("cannot determine import name")
    }

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
             aliases = paste0("\\alias{", name, "}"),
             docType = "\\docType{import}",
             title = paste0("\\title{Import from package \\pkg{", importedFrom, "}}"),
             description =
                 c("\\description{",
                   paste0("The \\code{", name, "} object is imported from package \\pkg{",
                          importedFrom, "}."),
                   paste0("Help is available here:  \\code{\\link[", importedFrom, ":",
                          importPage, "]{", importedFrom, "::", importPage, "}}."),
                   "}"))

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")

    message(gettextf("Created file named %s.", sQuote(filename)),
            "\n",
            gettext("Edit the file and move it to the appropriate directory."),
            domain = NA)
}

