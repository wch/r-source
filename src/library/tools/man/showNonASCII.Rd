\name{showNonASCII}
\alias{showNonASCII}
\title{Highlight non-ASCII characters}
\description{
  This function prints elements of a character vector which contain
  non-ASCII bytes, printing such bytes as a escape like \code{<fc>}.
}
\usage{
showNonASCII(x)
}
\arguments{
  \item{x}{a character vector.}
}
\details{
  This was originally written to help detect non-portable text in
  files in packages.

  It prints all lines of the files which contain non-ACII characters,
  preceded by the line number and with non-ASCII bytes highlighted
  \emph{via} \code{\link{iconv}(sub = "byte")}.
}

\examples{
out <- c(
"fa\xE7ile test of showNonASCII():",
"\\\\details{",
"   This is a good line",
"   This has an \xfcmlaut in it.",
"   OK again.",
"}")
cat(out, file = "my.Rd", sep = "\n")

showNonASCII(readLines("my.Rd"))
unlink("my.Rd")
}
\keyword{utilities}
