\name{parse}
\title{Parse Expressions}
\usage{
parse(file = "", n = NULL, text = NULL, prompt = NULL, white = FALSE)
}
\alias{parse}
\arguments{
  \item{file}{the name of a file to read the expressions from.  Note
    that the ASCII file \emph{must} end with a newline (\code{"\n"}).
    If \code{file} is \code{""} and \code{text} is missing or \code{NULL}
    then input is taken from the keyboard.}
  \item{n}{the number of statements to parse.  If \code{n} is negative
    the file is parsed in its entirety.}
  \item{text}{character. The text to parse, quoted.}
  \item{prompt}{the prompt to print when parsing from the keyboard.  The
  default, \code{NULL}, is to use \R's prompt, \code{options("prompt")[[1]]}.}
  \item{white}{if \code{TRUE} then any white space separates expressions
    otherwise only newlines or semicolons do.}
}
\description{
  \code{parse} returns the parsed but unevaluated expressions in a
  list.  Each element of the list is of mode \code{expression}.
}
\seealso{
  \code{\link{scan}}, \code{\link{source}}, \code{\link{eval}},
  \code{\link{deparse}}.
}
\examples{
cat("x <- c(1,4)\n  x ^ 3 -10 ; outer(1:7,5:9)\n", file="xyz.Rdmped")
# parse 3 statements from the file "xyz.Rdmped"
parse(file = "xyz.Rdmped", n = 3)
}
\keyword{file}
\keyword{programming}
