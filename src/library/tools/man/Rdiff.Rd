% File src/library/tools/man/Rdiff.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010-1 R Core Development Team
% Distributed under GPL 2 or later

\name{Rdiff}
\alias{Rdiff}
\title{Difference R Output Files}
\description{
  Given two \R output files, compute differences ignoring headers,
  footers and some encoding differences.
}
\usage{
Rdiff(from, to, useDiff = FALSE, forEx = FALSE, Log = FALSE)
}
\arguments{
  \item{from, to}{filepaths to be compared}
  \item{useDiff}{should \command{diff} always be used to compare
    results?}
  \item{forEx}{logical: extra pruning for \file{-Ex.Rout} files to
    exclude the header.}
  \item{Log}{logical: should be value include a log of differences found?}
}
\details{
  The \R startup banner and any timing information from \command{R CMD
    BATCH} are removed from both files, together with lines about
  loading packages.  UTF-8 fancy quotes (see \code{\link{sQuote}}) and on
  Windows, Windows so-called \sQuote{smart quotes} are mapped to a
  simple quote.  The files are then compared line-by-line.  If there are
  the same number of lines and \code{useDiff} is false, a simple
  \command{diff}-like display of differences is printed, otherwise
  \command{diff -bw} is called on the edited files.
}
\value{
  If \code{Log} is true, a list with components \code{status} (see
  below) and \code{out}, a character vector of descriptions of
  differences, possibly of zero length.

  Otherwise, a status indicator, \code{0L} if and only if no differences
  were found.
}

\seealso{
  The shell script run as \command{R CMD Rdiff}.
}

\keyword{utilities}

