\name{shQuote}
\alias{shQuote}
\title{Quote Strings for Use in OS Shells}
\description{
  Quote a string to be passed to an operating system shell.
}
\usage{
shQuote(string, type = "sh")
}
\arguments{
  \item{string}{a character vector, usually of length one.}
  \item{type}{character: the type of shell.}
}
\details{
  Currently the only type of quoting supported is that for the Bourne
  shell \code{sh}.  Each string is surrounded in double quotes, which
  suppresses all special meanings of metacharacters except dollar,
  backquote and backslash, so these (and of course double quote) are
  preceded by backslash.
}
\references{
  Loukides, M. et al (2002)
  \emph{Unix Power Tools} Third Edition.  O'Reilly.  Section 27.12.
}
\seealso{
  \code{\link{sQuote}} for quoting in text.
}
\examples{
test <- "abc$def`gh`i\\j"
cat(shQuote(test), "\n")
\dontrun{system(paste("echo", shQuote(test)))}
}
\keyword{utilities}
