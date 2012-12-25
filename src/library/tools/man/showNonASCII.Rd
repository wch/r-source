% File src/library/tools/man/showNonASCII.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-11 R Core Team
% Distributed under GPL 2 or later

\name{showNonASCII}
\alias{showNonASCII}
\alias{showNonASCIIfile}
\title{Pick Out Non-ASCII Characters}
\description{
  This function prints elements of a character vector which contain
  non-ASCII bytes, printing such bytes as a escape like \samp{<fc>}.
}
\usage{
showNonASCII(x)

showNonASCIIfile(file)
}
\arguments{
  \item{x}{a character vector.}
  \item{file}{path to a file.}
}
\details{
  This was originally written to help detect non-portable text in
  files in packages.

  It prints all element of \code{x} which contain non-ASCII characters,
  preceded by the element number and with non-ASCII bytes highlighted
  \emph{via} \code{\link{iconv}(sub = "byte")}.
}
\value{
  The elements of \code{x} containing non-ASCII characters will be returned
  invisibly.
}
\examples{
out <- c(
"fa\xE7ile test of showNonASCII():",
"\\\\details{",
"   This is a good line",
"   This has an \xfcmlaut in it.",
"   OK again.",
"}")
f <- tempfile()
cat(out, file = f, sep = "\n")

showNonASCIIfile(f)
unlink(f)
}
\keyword{utilities}
