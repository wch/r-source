prompt <- function(object, ...) UseMethod("prompt")

## Fixme : Both methods share a lot of code;  really re-use with namespace
## -----   For now, often change *both*

prompt.default <-
    function(object, filename = paste0(name, ".Rd"),
             force.function = FALSE, ...)
{
    paste0 <- function(...) paste(..., sep = "")
    is.missing.arg <- function(arg)
        typeof(arg) == "symbol" && deparse(arg) == ""

    name <-
        if(is.character(object))
            object
        else {
            name <- substitute(object)
            if(is.language(name) && !is.name(name)) name <- eval(name)
            as.character(name)
        }
    fn <- get(name)
    if(is.data.frame(fn))
       return(prompt.data.frame(fn, filename = filename))

    ## `file' [character(NN)] will contain the lines to be put in the
    ## Rdoc file
    file <- paste0("\\name{", name, "}")
    if(is.function(fn) || force.function) {
        file <- c(file,
                  paste0("\\alias{", name, "}"),
                  "%- Also NEED an `\\alias' for EACH other topic documented here.",
                  "\\title{ ~~function to do ... ~~ }",
                  "\\description{",
		  "  ~~ A concise (1-5 lines) description of what the function does. ~~",
		  "}")
	s <- seq(length = n <- length(argls <- formals(fn)))
	if(n > 0) {
	    arg.names <- arg.n <- names(argls)
	    arg.n[arg.n == "..."] <- "\\dots"
	}
	##-- Construct the 'call' -- for USAGE :
	call <- paste0(name, "(")
	for(i in s) { # i-th argument :
	    call <- paste0(call, arg.names[i],
			   if(!is.missing.arg(argls[[i]]))
			   paste0(" = ",deparse(argls[[i]])))
	    if(i != n) call <- paste0(call, ", ")
	}
	file <- c(file, "\\usage{", paste0(call, ")"), "}",
		  "%- maybe also `usage' for other objects documented here.")
	if(length(s))
	    file <- c(file, "\\arguments{",
		      paste0("  \\item{", arg.n, "}{",
			     " ~~Describe \\code{", arg.n, "} here~~ }"),"}")
	fn.def <- attr(fn, "source")
	if(is.null(fn.def))
            fn.def <- deparse(fn)
	if(any(br <- substr(fn.def,1,1) == "}"))
	    fn.def[br] <- paste(" ", fn.def[br])
	file <- c(file,
		  "\\details{",
		  "  ~~ If necessary, more details than the __description__  above ~~",
		  "}",
		  "\\value{",
		  "  ~Describe the value returned",
		  "  If it is a LIST, use",
		  "  \\item{comp1 }{Description of `comp1'}",
		  "  \\item{comp2 }{Description of `comp2'}",
		  "  ...",
		  "}",

		  "\\references{ ~put references to the literature/web site here ~ }",
		  "\\author{ ~~who you are~~ }",
		  "\\note{ ~~further notes~~ }",
		  "",
		  " ~Make other sections like WARNING with \\section{WARNING }{....} ~",
		  "",
		  "\\seealso{ ~~objects to SEE ALSO as \\code{\\link{~~fun~~}}, ~~~ }",
		  "",
		  "\\examples{",
		  "##---- Should be DIRECTLY executable !! ----",
		  "##-- ==>  Define data, use random,",
		  "##--	     or do  help(data=index)  for the standard data sets.",
		  "", "## The function is currently defined as",
		  fn.def,
		  "}",
		  "\\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS",
		  "\\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line"
		  )
    } else {#-- not function, assume dataset --
        tf <- tempfile(); on.exit(unlink(tf))
        sink(tf) ; str(object) ; sink()
        str.txt <- scan(tf, "", quiet = !getOption("verbose"), sep = "\n")
	file <-
            c(file, paste0("\\alias{", name, "}"),
              "\\non_function{}",
              "\\title{ ~~data-name / kind ...  }",
              "\\description{",
		  "  ~~ A concise (1-5 lines) description of the dataset. ~~",
              "}",
              paste0("\\usage{data(", name, ")}"),
              "\\format{", "  The format is:", str.txt, "}",
              ## remaining lines are IDENTICAL to those in prompt.data.frame():
              "\\source{",
              " ~~ reference to a publication or URL from which the data were obtained ~~",
              "}",
              "\\references{","~~ possibly secondary sources and usages ~~","}",
              "\\examples{",
              paste0("data(", name, ")"),
              paste0("## maybe str(",name,") ; plot(",name,") ..."),
              "}",
              "\\keyword{datasets}")
    }
    cat(file, file = filename, sep = "\n")
    RHOME <- R.home()
    if(substr(RHOME, 1, 8) == "/tmp_mnt") RHOME <- substr(RHOME, 9, 1000)
    cat("created file named ", filename, " in the current directory.\n",
	" Edit the file and move it to the appropriate directory, possibly\n",
	paste(RHOME,"src/library/<pkg>/man/",sep="/"), "\n")
    invisible(file)
}

prompt.data.frame <- function (object, filename = paste0(name, ".Rd"), ...)
{
    paste0 <- function(...) paste(..., sep = "")
##    describe <- function(object) UseMethod()

    name <- substitute(object)
    if (is.language(name) && !is.name(name))
        name <- eval(name)
    name <- as.character(name)
    dat <- get(name)
    ## `file' [character(NN)] will contain the lines to be put in the
    ## Rdoc file
    file <- c(paste0("\\name{", name, "}"), paste0("\\alias{", name, "}"))
    file <- c(file, "\\non_function{}",
              "\\title{ ~~ 1-line description of the data frame ~~ }",
              paste0("\\usage{data(", name, ")}"),
              "\\description{",
              paste0("The \\code{", name, "} data frame has ", nrow(dat),
                     " rows and ", ncol(dat), " columns."),
              "~~ Give a concise description here ~~", "}",
              "\\format{",
              "  This data frame contains the following columns:",
              "  \\describe{")
    for (i in names(dat)) {
      file <- c(file,
                paste0("    \\item{", i, "}{",
                       if (inherits(dat[[i]], "ordered")) {
                           c(paste0("an ", data.class(dat[[i]]),
                                    " factor with levels"),
                             paste(paste0("\\code{", levels(dat[[i]]), "}"),
                                   collapse = " < "))
                       } else if (inherits(dat[[i]], "factor")) {
                           c("a factor with levels",
                             paste0("\\code{", levels(dat[[i]]), "} "))
                       } else if (is.vector(dat[[i]])) {
                           paste0("a ", data.class(dat[[i]]), " vector")
                       } else if (is.matrix(dat[[i]])) {
                           paste0("a matrix with ", ncol(dat[[i]]), " columns")
                       } else {
                           paste0("a ", data.class(dat[[i]]))
                       },
                       "}"))
    }
    file <- c(file, "  }\n}",
              "\\details{",
              " ~~ If necessary, more details than the _description_ above ~~",
              "}",
              "\\source{",
              " ~~ reference to a publication or URL from which the data were obtained ~~",
              "}",
              "\\references{","~~ possibly secondary sources and usages ~~","}",
              "\\examples{",
              paste0("data(", name, ")"),
              paste0("## maybe str(",name,") ; plot(",name,") ..."),
              "}",
              "\\keyword{datasets}")
    cat(file, file = filename, sep = "\n")
    RHOME <- R.home()
    if (substr(RHOME, 1, 8) == "/tmp_mnt")
        RHOME <- substr(RHOME, 9, 1000)
    cat("created file named ", filename, " in the current directory.\n",
	" Edit the file and move it to the appropriate directory, possibly\n",
        paste(RHOME, "src/library/<pkg>/man/", sep = "/"), "\n")
    invisible(file)
}
