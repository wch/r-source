% File src/library/tools/man/Rdiff.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010-13 R Core Team
% Distributed under GPL 2 or later

\name{Rdiff}
\alias{Rdiff}
\title{Difference R Output Files}
\description{
  Given two \R output files, compute differences ignoring headers,
  footers and some other differences.
}
\usage{
Rdiff(from, to, useDiff = FALSE, forEx = FALSE,
      nullPointers = TRUE, Log = FALSE)
}
\arguments{
  \item{from, to}{filepaths to be compared}
  \item{useDiff}{should \command{diff} always be used to compare
    results?}
  \item{forEx}{logical: extra pruning for \file{-Ex.Rout} files to
    exclude the header.}
  \item{nullPointers}{logical: should the displayed addresses of
    pointers be set to \code{0x00000000} before comparison?}
  \item{Log}{logical: should the returned value include a log of
    differences found?}
}
\details{
  The \R startup banner and any timing information from \command{R CMD
    BATCH} are removed from both files, together with lines about
  loading packages.  UTF-8 fancy quotes (see \code{\link{sQuote}}) and
  on Windows, Windows' so-called \sQuote{smart quotes}, are mapped to
  a simple quote.  Addresses of environments, compiled bytecode and
  other exotic types expressed as hex addresses
  (e.g. \code{<environment: 0x12345678>}) are mapped to
  \code{0x00000000}.  The files are then compared line-by-line.  If
  there are the same number of lines and \code{useDiff} is false, a
  simple \command{diff}-like display of differences is printed (which
  ignores differences in consecutive spaces), otherwise \command{diff
    -bw} is called on the edited files.  (This tries to ignore all
  differences in whitespace: note that flag \option{-w} is not
  required by POSIX but is supported by GNU, Solaris and FreeBSD
  versions.)

  This can compare uncompressed PDF files, ignoring differences in
  creation and modification dates.
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

