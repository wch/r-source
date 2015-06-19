% File src/library/base/man/pushBack.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{pushBack}
\alias{pushBack}
\alias{pushBackLength}
\alias{clearPushBack}
\title{Push Text Back on to a Connection}
\description{
  Functions to push back text lines onto a \link{connection}, and to enquire
  how many lines are currently pushed back.
}
\usage{
pushBack(data, connection, newLine = TRUE,
         encoding = c("", "bytes", "UTF-8"))
pushBackLength(connection)
clearPushBack(connection)
}
\arguments{
  \item{data}{a character vector.}
  \item{connection}{A \link{connection}.}
  \item{newLine}{logical.  If true, a newline is appended to each string
    pushed back.}
  \item{encoding}{character.  See details.}
}
\details{
  Several character strings can be pushed back on one or more occasions.
  The occasions form a stack, so the first line to be retrieved will be
  the first string from the last call to \code{pushBack}.  Lines which
  are pushed back are read prior to the normal input from the
  connection, by the normal text-reading functions such as
  \code{\link{readLines}} and \code{\link{scan}}.

  Pushback is only allowed for readable connections in text mode.

  Not all uses of connections respect pushbacks, in particular the input
  connection is still wired directly, so for example parsing
  commands from the console and \code{\link{scan}("")} ignore pushbacks on
  \code{\link{stdin}}.

  When character strings with a marked encoding (see
  \code{\link{Encoding}}) are pushed back they are converted to the
  current encoding if \code{encoding = ""}.  This may involve
  representing characters as \samp{<U+xxxx>} if they cannot be
  converted.  They will be converted to UTF-8 if \code{encoding =
  "UTF-8"} or left as-is if \code{encoding = "bytes"}.
}
\value{
  \code{pushBack} and \code{clearPushBack()} return nothing, invisibly.

  \code{pushBackLength} returns the number of lines currently pushed back.
}
\seealso{\code{\link{connections}}, \code{\link{readLines}}.}

\examples{
zz <- textConnection(LETTERS)
readLines(zz, 2)
pushBack(c("aa", "bb"), zz)
pushBackLength(zz)
readLines(zz, 1)
pushBackLength(zz)
readLines(zz, 1)
readLines(zz, 1)
close(zz)
}
\keyword{connection}
