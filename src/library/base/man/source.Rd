\name{source}
\title{Read R Code from a File or a Connection}
\usage{
source(file, local = FALSE, echo = verbose, print.eval = echo,
       verbose = getOption("verbose"),
       prompt.echo = getOption("prompt"),
       max.deparse.length = 150, chdir = FALSE,
       encoding = getOption("encoding"))
}
\alias{source}
\arguments{
  \item{file}{a connection or a character string giving the pathname
    of the file or URL to read from.}
  \item{local}{if \code{local} is \code{FALSE}, the statements scanned
    are evaluated in the user's workspace (the global environment),
    otherwise in the environment calling \code{source}.}
  \item{echo}{logical; if \code{TRUE}, each expression is printed
    after parsing, before evaluation.}
  \item{print.eval}{logical; if \code{TRUE}, the result of
    \code{eval(i)} is printed for each expression \code{i}; defaults
    to \code{echo}.}
  \item{verbose}{if \code{TRUE}, more diagnostics (than just
    \code{echo = TRUE}) are printed during parsing and evaluation of
    input, including extra info for \bold{each} expression.}
  \item{prompt.echo}{character; gives the prompt to be used if
    \code{echo = TRUE}.}
  \item{max.deparse.length}{integer; is used only if \code{echo} is
    \code{TRUE} and gives the maximal length of the \dQuote{echo} of a
    single expression.}
  \item{chdir}{logical; if \code{TRUE} and \code{file} is a pathname,
    the \R working directory is temporarily changed to the directory
    containing \code{file} for evaluating.}
  \item{encoding}{character string.  The encoding to be assumed when
    \code{file} is a character string: see \code{\link{file}}.  A
    possible value is \code{"unknown"}: see the Details.}
}
\description{
  \code{source} causes \R to accept its input from the named file or URL
  (the name must be quoted) or connection.  Input is read and
  \code{\link{parse}}d by from that file until the end of the file is
  reached, then the parsed expressions are evaluated sequentially in the
  chosen environment.
}
\details{
  All versions of \R accept input from a connection with end of line
  marked by LF (as used on Unix), CRLF (as used on DOS/Windows)
  or CR (as used on classic MacOS).  The final line can be incomplete, that
  is missing the final EOL marker.

  If \code{\link{options}}("keep.source") is true (the default), the
  source of functions is kept so they can be listed exactly as input.
  This imposes a limit of 128K chars on the function size and a nesting
  limit of 265.  Use \code{option(keep.source = FALSE)} when these
  limits might take effect: if exceeded they generate an error.

  This paragraph applies if \code{file} is a filename (rather than a
  connection).  If \code{encoding = "unknown"}, an attempt is made to
  guess the encoding.  The result of \code{\link{localeToCharset}()} is
  used a guide.  If \code{encoding} has two or more elements, they are
  tried in turn until the file/URL can be read without error in the
  trial encoding.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{demo}} which uses \code{source};
  \code{\link{eval}}, \code{\link{parse}} and \code{\link{scan}};
  \code{\link{options}("keep.source")}.
}
\keyword{file}
\keyword{programming}
\keyword{connection}
