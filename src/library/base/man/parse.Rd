\name{parse}
\alias{parse}
\title{Parse Expressions}
\description{
  \code{parse} returns the parsed but unevaluated expressions in a
  list.
}
\usage{
parse(file = "", n = NULL, text = NULL, prompt = "?", srcfile,
      encoding = "unknown")
}
\arguments{
  \item{file}{a connection, or a character string giving the name of a
    file or a URL to read the expressions from.
    If \code{file} is \code{""} and \code{text} is missing or \code{NULL}
    then input is taken from the console.}
  \item{n}{integer (or coerced to integer).  The maximum number of
    expressions to parse.  If \code{n} is \code{NULL} or negative or
    \code{NA} the input is parsed in its entirety.}
  \item{text}{character vector.  The text to parse.  Elements are treated
    as if they were lines of a file.  Other \R objects will be coerced
    to character (without method dispatch) if possible.}
  \item{prompt}{the prompt to print when parsing from the keyboard.
    \code{NULL} means to use \R's prompt, \code{getOption("prompt")}.}
  \item{srcfile}{\code{NULL}, or a \code{\link{srcfile}} object.  See
    the Details section.}
  \item{encoding}{encoding to be assumed for input strings.  It is
    used to mark character strings as known to be in
    Latin-1 or UTF-8: it is not used to re-encode the input.}
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
  If \code{text} has length greater than zero (after coercion) it is used in
  preference to \code{file}.
  
  All versions of \R accept input from a connection with end of line
  marked by LF (as used on Unix), CRLF (as used on DOS/Windows)
  or CR (as used on classic MacOS).  The final line can be incomplete,
  that is missing the final EOL marker.

  See \code{\link{source}} for the limits on the size of functions
  that can be parsed (by default).  There is also a limit of 8192 bytes
  on the size of strings which can be parsed.

  When input is taken from the console, \code{n = NULL} is equivalent to
  \code{n = 1}, and \code{n < 0} will read until an EOF character is read.
#ifdef windows
  (The EOF character is Ctrl-Z for the Windows front-ends.)
#endif

  The default for \code{srcfile} is set as follows.  If
  \code{options("keep.source")} is \code{FALSE}, \code{srcfile}
  defaults to \code{NULL}. Otherwise, if \code{text} is used,
  \code{srcfile} will be set to a \code{\link{srcfilecopy}} containing
  the text.  If a character string is used for \code{file}, a
  \code{\link{srcfile}} object referring to that file will be used.
}
\value{
  An object of type \code{"\link{expression}"}, with up to \code{n}
  elements if specified as a non-negative integer.

  When \code{srcfile} is non-\code{NULL}, a \code{"srcref"} attribute
  will be attached to the result containing a list of \code{\link{srcref}}
  records corresponding to each element, and a \code{"srcfile"} attribute
  will be attached containing a copy of \code{srcfile}.
  
  A syntax error (including an incomplete expression) will throw an error.

  Character strings in the result will have a declared encoding if
  \code{encoding} is \code{"latin1"} or \code{"UTF-8"}.
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
