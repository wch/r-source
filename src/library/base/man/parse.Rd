% File src/library/base/man/parse.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{parse}
\title{Parse R Expressions}
\alias{parse}
\alias{str2lang}
\alias{str2expression}
\description{
  \code{parse()} returns the parsed but unevaluated expressions in an
  \code{\link{expression}}, a \dQuote{list} of \code{\link{call}}s.

  \code{str2expression(s)} and \code{str2lang(s)} return special versions
  of \code{parse(text=s, keep.source=FALSE)} and can therefore be regarded as
  transforming character strings \code{s} to expressions, calls, etc.
}
\usage{
parse(file = "", n = NULL, text = NULL, prompt = "?",
      keep.source = getOption("keep.source"), srcfile,
      encoding = "unknown")

str2lang(s)
str2expression(text)
}
\arguments{
  \item{file}{a \link{connection}, or a character string giving the name of a
    file or a URL to read the expressions from.
    If \code{file} is \code{""} and \code{text} is missing or \code{NULL}
    then input is taken from the console.}
  \item{n}{integer (or coerced to integer).  The maximum number of
    expressions to parse.  If \code{n} is \code{NULL} or negative or
    \code{NA} the input is parsed in its entirety.}
  \item{text}{character vector.  The text to parse.  Elements are treated
    as if they were lines of a file.  Other \R objects will be coerced
    to character if possible.}
  \item{prompt}{the prompt to print when parsing from the keyboard.
    \code{NULL} means to use \R's prompt, \code{getOption("prompt")}.}
  \item{keep.source}{a logical value; if \code{TRUE}, keep
    source reference information.}
  \item{srcfile}{\code{NULL}, a character vector, or a
    \code{\link{srcfile}} object.  See the \sQuote{Details} section.}
  \item{encoding}{encoding to be assumed for input strings.  If the
    value is \code{"latin1"} or \code{"UTF-8"} it is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used
    to re-encode the input.  To do the latter, specify the encoding as
    part of the connection \code{con} or \emph{via}
    \code{\link{options}(encoding=)}: see the example under
    \code{\link{file}}.}
  \item{s}{a \code{\link{character}} vector of length \code{1}, i.e., a
    \dQuote{string}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Murdoch, D. (2010).
  \href{https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Murdoch.pdf}{Source
  References}.  \emph{The R Journal} 2/2, 16-19.
}
\seealso{
  \code{\link{scan}}, \code{\link{source}}, \code{\link{eval}},
  \code{\link{deparse}}.

  The source reference information can be used for debugging (see
  e.g.\sspace{}\code{\link{setBreakpoint}}) and profiling (see
  \code{\link{Rprof}}).  It can be examined by \code{\link{getSrcref}}
  and related functions.  More detailed information is available through
  \code{\link{getParseData}}.
}
\details{
  \describe{
    \item{\code{parse(....)}: }{

  If \code{text} has length greater than zero (after coercion) it is used in
  preference to \code{file}.

  All versions of \R accept input from a connection with end of line
  marked by LF (as used on Unix), CRLF (as used on DOS/Windows)
  or CR (as used on classic Mac OS).  The final line can be incomplete,
  that is missing the final EOL marker.

  When input is taken from the console, \code{n = NULL} is equivalent to
  \code{n = 1}, and \code{n < 0} will read until an EOF character is
  read.  (The EOF character is Ctrl-Z for the Windows front-ends.)  The
  line-length limit is 4095 bytes when reading from the console (which
  may impose a lower limit: see \sQuote{An Introduction to R}).

  The default for \code{srcfile} is set as follows.  If
  \code{keep.source} is not \code{TRUE}, \code{srcfile}
  defaults to a character string, either \code{"<text>"} or one
  derived from \code{file}.  When \code{keep.source} is
  \code{TRUE}, if \code{text} is used, \code{srcfile} will be set to a
  \code{\link{srcfilecopy}} containing the text.  If a character
  string is used for \code{file}, a \code{\link{srcfile}} object
  referring to that file will be used.

  When \code{srcfile} is a character string, error messages will
  include the name, but source reference information will not be added
  to the result.  When \code{srcfile} is a \code{\link{srcfile}}
  object, source reference information will be retained.
  }
    \item{\code{str2expression(s)}: }{for a \code{\link{character}} vector
      \code{s}, \code{str2expression(s)} corresponds to
      \code{\link{parse}(text = s, keep.source=FALSE)}, which is always of
      type (\code{\link{typeof}}) and \code{\link{class}} \code{expression}.
    }
    \item{\code{str2lang(s)}: }{for a \code{\link{character}} string
      \code{s}, \code{str2lang(s)} corresponds to
      \code{\link{parse}(text = s, keep.source=FALSE)[[1]]} (plus a check
      that both \code{s} and the \code{parse(*)} result are of length one)
      which is typically a \code{call} but may also be a \code{symbol} aka
      \code{\link{name}}, \code{\link{NULL}} or an atomic constant such as
      \code{2}, \code{1L}, or \code{TRUE}.  Put differently, the value of
      \code{str2lang(.)} is a call or one of its parts, in short
      \dQuote{a call or simpler}.
    }
  }% describe
  Currently, encoding is not handled in \code{str2lang()} and
  \code{str2expression()}.
}
\section{Partial parsing}{
  When a syntax error occurs during parsing, \code{parse}
  signals an error.  The partial parse data will be stored in the
  \code{srcfile} argument if it is a \code{\link{srcfile}} object
  and the \code{text} argument was used to supply the text.  In other
  cases it will be lost when the error is triggered.

  The partial parse data can be retrieved using
  \code{\link{getParseData}} applied to the \code{srcfile} object.
  Because parsing was incomplete, it will typically include references
  to \code{"parent"} entries that are not present.
}
\value{
  \code{parse()} and \code{str2expression()} return an object of type
  \code{"\link{expression}"}, for \code{parse()} with up to \code{n}
  elements if specified as a non-negative integer.

  \code{str2lang(s)}, \code{s} a string, returns \dQuote{a
  \code{\link{call}} or simpler}, see the \sQuote{Details:} section.

  When \code{srcfile} is non-\code{NULL}, a \code{"srcref"} attribute
  will be attached to the result containing a list of
  \code{\link{srcref}} records corresponding to each element, a
  \code{"srcfile"} attribute will be attached containing a copy of
  \code{srcfile}, and a \code{"wholeSrcref"} attribute will be
  attached containing a \code{\link{srcref}} record corresponding to
  all of the parsed text. Detailed parse information will be stored in
  the \code{"srcfile"} attribute, to be retrieved by
  \code{\link{getParseData}}.

  A syntax error (including an incomplete expression) will throw an error.

  Character strings in the result will have a declared encoding if
  \code{encoding} is \code{"latin1"} or \code{"UTF-8"}, or if
  \code{text} is supplied with every element of known encoding in a
  Latin-1 or UTF-8 locale.
}
\note{
  Using \code{parse(text = *, ..)} or its simplified and hence more
  efficient versions \code{str2lang()} or \code{str2expression()} is at
  least an order of magnitude less efficient than \code{\link{call}(..)} or
  \code{\link{as.call}()}.
}
\examples{
fil <- tempfile(fileext = ".Rdmped")
cat("x <- c(1, 4)\n  x ^ 3 -10 ; outer(1:7, 5:9)\n", file = fil)
# parse 3 statements from our temp file
parse(file = fil, n = 3)
unlink(fil)

## str2lang(<string>)  || str2expression(<character>) :
stopifnot(exprs = {
  identical( str2lang("x[3] <- 1+4"), quote(x[3] <- 1+4))
  identical( str2lang("log(y)"),      quote(log(y)) )
  identical( str2lang("abc"   ),      quote(abc) -> qa)
  is.symbol(qa) & !is.call(qa)           # a symbol/name, not a call
  identical( str2lang("1.375" ), 1.375)  # just a number, not a call
})

# A partial parse with a syntax error
txt <- "
x <- 1
an error
"
sf <- srcfile("txt")
try(parse(text = txt, srcfile = sf))
getParseData(sf)
}
\keyword{file}
\keyword{programming}
\keyword{connection}
