\name{parse}
\title{Parse Expressions}
\usage{
parse(file = "", n = NULL, text = NULL, prompt = NULL, white = FALSE)
}
\alias{parse}
\arguments{
  \item{file}{the name of a file to read the expressions from.  If the
    \code{file} is \code{""} and \code{text} is missing or \code{NULL}
    then input is taken from the keyboard.}
  \item{n}{the number of statements to parse.  If \code{n} is negative
    the file is parsed in its entirety.
    When parsing takes place from the keyboard, \code{n} is always 1.}
  \item{text}{text to parse, quoted.}
  \item{prompt}{the prompt to print when parsing from the keyboard}
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
# parse 3 statements from the file "xyz.Rdmped"
parse(file = "xyz.Rdmped", n = 3)
}
\keyword{file}
\keyword{programming}
