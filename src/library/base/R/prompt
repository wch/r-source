prompt <- function(object, ...) UseMethod("prompt")

## Later, we may want  a data.frame method ..

prompt.default <-
function(object, filename = paste0(name, ".Rd"), force.function = FALSE)
{
 paste0 <- function(...) paste(..., sep = "")
 is.missing.arg <- function(arg) typeof(arg) == "symbol" && deparse(arg) == ""
 name <- substitute(object)
 if(is.language(name) && !is.name(name)) name <- eval(name)
 name <- as.character(name)
 fn <- get(name)
 ##-- 'file' [character(NN)] will contain the lines to be put in the Rdoc file
 file <- paste0("\\name{", name, "}")
 if(is.function(fn) || force.function) {
	file <- c(file, "\\title{ ~~function to do ... ~~}")
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
			 paste0("=",deparse(argls[[i]])))
	  if(i != n) call <- paste0(call, ", ")
	}
	file <- c(file, "\\usage{", paste0(call, ")"), "}",
	 "%- maybe also `usage' for other functions documented here.",
	 paste0("\\alias{", name, "}"),
	 "%- Also NEED an `\\alias' for EACH other function documented here."
	)

	if(length(s))
	  file <- c(file, "\\arguments{",
		    paste0(" \\item{", arg.n, "}{",
			   " ~~Describe \\code{", arg.n, "} here~~ }"),"}")
	fn.def <- deparse(fn)
	if(any(br <- substr(fn.def,1,1) == "}"))
	  fn.def[br] <- paste(" ", fn.def[br])
	file <- c(file,
	"\\description{",
	" ~~ A precise description of what the function does. ~~",
	"}",
	"\\value{",
	"  ~Describe the value returned",
	"  If it is a LIST, use",
	"  \\item{comp1}{Description of `comp1'}",
	"  \\item{comp2}{Description of `comp2'}",
	"  ...",
	"}",

	"\\references{ ~put references to the literature/web site here ~ }",
	"\\author{ ~~if you are not one of R & R ..~~ }",
	"\\note{ ~~further notes~~ }",
	"",
	" ~Make other sections like WARNING with \\section{WARNING}{....} ~",
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
	"\\keyword{ ~keyword }%-- one or more ..."
	)
} else {#-- not function --
	file <- c(file,"\\non_function{}",
		  paste("\\title{ ~~data-name / kind ...  }"),
		  "\\description{",
		  "~~ a precise description of what the function does. ~~",
		  "}")
      }
 cat(file, file = filename, sep = "\n")
 RHOME <- getenv("RHOME")
 if(substr(RHOME,1,8) == "/tmp_mnt") RHOME <- substr(RHOME,9,1000)
 cat("created file named ", filename, " in the current directory.\n",
     " Edit the file and move it to the appropriate directory,\n",
     paste(RHOME,"src/library/<pkg>/man/",sep="/"), "\n")
 invisible(file)
}
