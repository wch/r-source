\name{parse}
\alias{parse}
\title{Parse Expressions}
\description{
  \code{parse} returns the parsed but unevaluated expressions in a
  list.  Each element of the list is of mode \code{expression}.
}
\usage{
parse(file = "", n = NULL, text = NULL, prompt = "?", white = FALSE)
}
\arguments{
  \item{file}{a connection, or a character string giving the name of a
    file or a URL to read the expressions from.
    If \code{file} is \code{""} and \code{text} is missing or \code{NULL}
    then input is taken from the console.}
  \item{n}{the number of statements to parse.  If \code{n} is negative
    the file is parsed in its entirety.}
  \item{text}{character vector. The text to parse. Elements are treated
    as if they were lines of a file.}
  \item{prompt}{the prompt to print when parsing from the keyboard.
    \code{NULL} means to use \R's prompt, \code{getOption("prompt")}.}
  \item{white}{\emph{Unimplemented and deprecated}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{scan}}, \code{\link{source}}, \code{\link{eval}},
  \code{\link{deparse}}.
}
\details{
  All versions of \R accept input from a connection with end of line
  marked by LF (as used on Unix), CRLF (as used on DOS/Windows)
  or CR (as used on classic MacOS).  The final line can be incomplete,
  that is missing the final EOL marker.

  See \code{\link{source}} for the limits on the size of functions
  that can be parsed (by default).
}
\examples{
cat("x <- c(1,4)\n  x ^ 3 -10 ; outer(1:7,5:9)\n", file="xyz.Rdmped")
# parse 3 statements from the file "xyz.Rdmped"
parse(file = "xyz.Rdmped", n = 3)
unlink("xyz.Rdmped")
}
\keyword{file}
\keyword{programming}
\keyword{connection}
