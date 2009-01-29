\name{Rdiff}
\alias{Rdiff}
\title{Difference \R Output Files}
\description{
  Given two \R output files, compute differences ignoring headers,
  footers and some encoding differences.
}
\usage{
Rdiff(from, to, useDiff = FALSE)
}
\arguments{
  \item{from, to}{filepaths to be compared}
  \item{useDiff}{should \command{diff} always be used to compare results?}
}
\details{
  The \R startup banner and any timing information from \command{R CMD
    BATCH} are removed from both files, together with lines about
  loading packages.  UTF-8 fancy quotes (see \code{\link{sQuote}} and on
  Windows, Windows so-called \sQuote{smart quotes} are mapped to a
  simple quote.  The files are then compared line-by-line.  If there are
  the same number of lines and \code{useDiff} is false, a simple
  \command{diff}-like display of differences is printed, otherwise
  \command{diff -bw} is called on the edited files.
}
\value{
  \code{0L} if no differences were found, otherwise \code{1L}
}

\seealso{
  The shell script run as \command{R CMD Rdiff}.
}

\keyword{utilities}

