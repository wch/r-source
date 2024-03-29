% File src/library/base/man/textconnections.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{textConnection}
\alias{textConnection}
\alias{textConnectionValue}
\title{Text Connections}
\description{
  Input and output text connections.
}
\usage{
textConnection(object, open = "r", local = FALSE,
               name = deparse1(substitute(object)),
               encoding = c("", "bytes", "UTF-8"))

textConnectionValue(con)
}
\arguments{
  \item{object}{character.  A description of the \link{connection}.
    For an input this is an \R character vector object, and for an output
    connection the name for the \R character vector to receive the
    output, or \code{NULL} (for none).
  }
  \item{open}{character string.  Either \code{"r"} (or equivalently \code{""})
    for an input connection or \code{"w"} or \code{"a"} for an output
    connection.}
  \item{local}{logical.  Used only for output connections.  If \code{TRUE},
    output is assigned to a variable in the calling environment.  Otherwise
    the global environment is used.}
  \item{name}{a \code{\link{character}} string specifying the connection name.}
  \item{encoding}{character string, partially matched.  Used only for input connections.  How
    marked strings in \code{object} should be handled: converted to the
    current locale, used byte-by-byte or translated to UTF-8.}
  \item{con}{an output text connection.}
}
\details{
  An input text connection is opened and the character vector is copied
  at time the connection object is created, and \code{close} destroys
  the copy.  \code{object} should be the name of a character vector:
  however, short expressions will be accepted provided they \link{deparse} to
  less than 60 bytes.

  An output text connection is opened and creates an \R character vector
  of the given name in the user's workspace or in the calling environment,
  depending on the value of the \code{local} argument.  This object will at all
  times hold the completed lines of output to the connection, and
  \code{\link{isIncomplete}} will indicate if there is an incomplete
  final line.  Closing the connection will output the final line,
  complete or not.  (A line is complete once it has been terminated by
  end-of-line, represented by \code{"\\n"} in \R.)   The output character
  vector has locked bindings (see \code{\link{lockBinding}}) until
  \code{close} is called on the connection.  The character vector can
  also be retrieved \emph{via} \code{textConnectionValue}, which is the
  only way to do so if \code{object = NULL}.  If the current locale is
  detected as Latin-1 or UTF-8, non-ASCII elements of the character vector
  will be marked accordingly (see \code{Encoding}).

  Opening a text connection with \code{mode = "a"} will attempt to
  append to an existing character vector with the given name in the
  user's workspace or the calling environment.  If none is found (even
  if an object exists of the right name but the wrong type) a new
  character vector will be created, with a warning.

  You cannot \code{seek} on a text connection, and \code{seek} will
  always return zero as the position.

  Text connections have slightly unusual semantics: they are always
  open, and throwing away an input text connection without closing it
  (so it get garbage-collected) does not give a warning.
}

\value{
  For \code{textConnection}, a connection object of class
  \code{"textConnection"} which inherits from class \code{"connection"}.

  For \code{textConnectionValue}, a character vector.
}

\note{
  As output text connections keep the character vector up to date
  line-by-line, they are relatively expensive to use, and it is often
  better to use an anonymous \code{\link{file}()} connection to collect
  output.

  On (rare) platforms where \code{vsnprintf} does not return the needed
  length of output there is a 100,000 character limit on the length of
  line for output connections: longer lines will be truncated with a
  warning.
}

\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language.} Springer.\cr
[S has input text connections only.]
}

\seealso{
  \code{\link{connections}}, \code{\link{showConnections}},
  \code{\link{pushBack}}, \code{\link{capture.output}}.
}

\examples{
zz <- textConnection(LETTERS)
readLines(zz, 2)
scan(zz, "", 4)
pushBack(c("aa", "bb"), zz)
scan(zz, "", 4)
close(zz)

zz <- textConnection("foo", "w")
writeLines(c("testit1", "testit2"), zz)
cat("testit3 ", file = zz)
isIncomplete(zz)
cat("testit4\n", file = zz)
isIncomplete(zz)
close(zz)
foo

\donttest{# capture R output: use part of example from help(lm)
zz <- textConnection("foo", "w")
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.5, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
sink(zz)
anova(lm.D9 <- lm(weight ~ group))
cat("\nSummary of Residuals:\n\n")
summary(resid(lm.D9))
sink()
close(zz)
cat(foo, sep = "\n")
}
}
\keyword{file}
\keyword{connection}
