prompt <-
function(object, filename = NULL, name = NULL, ...)
    UseMethod("prompt")

prompt.default <-
function(object, filename = NULL, name = NULL,
         force.function = FALSE, ...)
{
    paste0 <- function(...) paste(..., sep = "")

    is.missing.arg <- function(arg)
        typeof(arg) == "symbol" && deparse(arg) == ""

    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.language(name) && !is.name(name))
                    name <- eval(name)
                as.character(name)
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    ## Better than get(); works when called in fun :
    x <- get(name, envir = parent.frame())

    ## <FIXME>
    ## If not a function or forced to document a function (?), always
    ## assume data set.
    if(!(is.function(x) || force.function))
        return(promptData(x, filename = filename, name = name))
    ## </FIXME>

    n <- length(argls <- formals(x))
    if(n > 0) {
        arg.names <- arg.n <- names(argls)
        arg.n[arg.n == "..."] <- "\\dots"
    }
    ## Construct the 'call' for \usage.
    call <- paste0(name, "(")
    for(i in seq(length = n)) {                       # i-th argument
        call <- paste0(call, arg.names[i],
                       if(!is.missing.arg(argls[[i]]))
                       paste0(" = ",
                              paste(deparse(argls[[i]], width.cutoff= 500),
                                    collapse="\n")))
        if(i != n) call <- paste0(call, ", ")
    }

    ## Construct the definition for \examples.
    x.def <- attr(x, "source")
    if(is.null(x.def))
        x.def <- deparse(x)
    if(any(br <- substr(x.def, 1, 1) == "}"))
        x.def[br] <- paste(" ", x.def[br])

    Rdtxt <-
        list(name = paste0("\\name{", name, "}"),
             aliases = c(paste0("\\alias{", name, "}"),
             paste("%- Also NEED an '\\alias' for EACH other topic",
                   "documented here.")),
             title = "\\title{ ~~function to do ... ~~ }",
             description = c("\\description{",
             paste("  ~~ A concise (1-5 lines) description of what",
                   "the function does. ~~"),
             "}"),
             usage = c("\\usage{", paste0(call, ")"), "}",
             paste("%- maybe also 'usage' for other objects",
                   "documented here.")),
             arguments = NULL,
             details = c("\\details{",
             paste("  ~~ If necessary, more details than the",
                   "__description__  above ~~"),
             "}"),
             value = c("\\value{",
             "  ~Describe the value returned",
             "  If it is a LIST, use",
             "  \\item{comp1 }{Description of 'comp1'}",
             "  \\item{comp2 }{Description of 'comp2'}",
             "  ...",
             "}"),
             references = paste("\\references{ ~put references to the",
             "literature/web site here ~ }"),
             author = "\\author{ ~~who you are~~ }",
             note = c("\\note{ ~~further notes~~ }",
             "",
             paste(" ~Make other sections like Warning with",
                   "\\section{Warning }{....} ~"),
             ""),
             seealso = paste("\\seealso{ ~~objects to See Also as",
             "\\code{\\link{~~fun~~}}, ~~~ }"),
             examples = c("\\examples{",
             "##---- Should be DIRECTLY executable !! ----",
             "##-- ==>  Define data, use random,",
             "##--	or do  help(data=index)  for the standard data sets.",
             "",
             "## The function is currently defined as",
             x.def,
             "}"),
             keywords = c(paste("\\keyword{ ~kwd1 }% at least one,",
             "from doc/KEYWORDS"),
             "\\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line"))

    Rdtxt$arguments <- if(n > 0)
        c("\\arguments{",
          paste0("  \\item{", arg.n, "}{",
                 " ~~Describe \\code{", arg.n, "} here~~ }"),
          "}") ## else NULL

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    cat(strwrap(c(paste(gettext("Created file named "), sQuote(filename),
                        ".", sep=""),
      gettext("Edit the file and move it to the appropriate directory."))),
        sep = "\n")

    invisible(filename)
}

prompt.data.frame <-
function(object, filename = NULL, name = NULL, ...)
{
    paste0 <- function(...) paste(..., sep = "")

    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.language(name) && !is.name(name))
                    name <- eval(name)
                as.character(name)
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    x <- get(name, envir = parent.frame())

    ## <FIXME>
    ## Always assume data set ???
    promptData(x, filename = filename, name = name)
    ## </FIXME>
}

promptData <-
function(object, filename = NULL, name = NULL)
{
    paste0 <- function(...) paste(..., sep = "")

    if(missing(name))
        name <-
            if(is.character(object))
                object
            else {
                name <- substitute(object)
                if(is.language(name) && !is.name(name))
                    name <- eval(name)
                as.character(name)
            }
    if(is.null(filename))
        filename <- paste0(name, ".Rd")

    ## Better than get(); works when called in fun :
    x <- get(name, envir = parent.frame())

    ## Construct the format.
    if(is.data.frame(x)) {
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
                  paste0("    \\item{\\code{", i, "}}{",
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
             aliases = paste0("\\alias{", name, "}"),
             docType = "\\docType{data}",
             title = "\\title{ ~~ data name/kind ... ~~}",
             description = c("\\description{",
             "  ~~ A concise (1-5 lines) description of the dataset. ~~",
             "}"),
             usage = paste0("\\usage{data(", name, ")}"),
             format = fmt,
             details = c("\\details{",
             paste("  ~~ If necessary, more details than the",
                   "__description__ above ~~"),
             "}"),
             source = c("\\source{",
             paste("  ~~ reference to a publication or URL",
                   "from which the data were obtained ~~"),
             "}"),
             references = c("\\references{",
             "  ~~ possibly secondary sources and usages ~~",
             "}"),
             examples = c("\\examples{",
             paste0("data(", name, ")"),
             paste0("## maybe str(", name, ") ; plot(", name, ") ..."),
             "}"),
             keywords = "\\keyword{datasets}")

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    cat(strwrap(c(paste(gettext("Created file named "), sQuote(filename),
                        ".", sep=""),
      gettext("Edit the file and move it to the appropriate directory."))),
        sep = "\n")

    invisible(filename)
}
