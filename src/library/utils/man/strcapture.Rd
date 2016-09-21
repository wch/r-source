\name{strcapture}
\alias{strcapture}
\title{
Capture string tokens into a data.frame
}
\description{
  Given a character vector and a regular expression containing capture
  expressions, \code{strcapture} will extract the captured tokens into a
  tabular data structure, such as a data.frame, the type and structure of
  which is specified by a prototype object. The assumption is that the
  same number of tokens are captured from every input string. 
}
\usage{
strcapture(pattern, x, proto, perl = FALSE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{
    The regular expression with the capture expressions.
  }
  \item{x}{
    A character vector in which to capture the tokens.
  }
  \item{proto}{
    A \code{data.frame} or S4 object that behaves like one. See details.
  }
  \item{perl,useBytes}{
    Arguments passed to \code{\link{regexec}}.
  }
}
\details{
  The \code{proto} argument is typically a \code{data.frame}, with a
  column corresponding to each capture expression, in order. The
  captured character vector is coerced to the type of the column, and
  the column names are carried over to the return value. Any data in the
  prototype are ignored. See the examples. 
}
\value{
  A tabular data structure of the same type as \code{proto}, so
  typically a \code{data.frame}, containing a column for each capture
  expression. The column types and names are inherited from
  \code{proto}. Cases in \code{x} that do not match \code{pattern} have
  \code{NA} in every column.
}
\seealso{
  \code{\link{regexec}} and \code{\link{regmatches}} for related
  low-level utilities.
}
\examples{
x <- "chr1:1-1000"
pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
proto <- data.frame(chr=character(), start=integer(), end=integer())
strcapture(pattern, x, proto)
}
\keyword{utilities}
