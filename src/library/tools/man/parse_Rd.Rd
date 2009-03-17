% File src/library/tools/man/parse_Rd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{parse_Rd}
\alias{parse_Rd}
\title{ Parse an Rd file }
\description{
  This function reads an R documentation (Rd) file and parses it, for
  processing by other functions.

  It is \bold{experimental}.
}
\usage{
parse_Rd(file, srcfile = NULL, encoding = "unknown", verbose = FALSE)
}
\arguments{
  \item{file}{A filename or text-mode connection.  At present filenames
    work best.}
  \item{srcfile}{\code{NULL}, or a \code{"srcfile"} object.  See the
    \sQuote{Details} section.}
  \item{encoding}{Encoding to be assumed for input strings.}
  \item{verbose}{Logical indicating whether detailed parsing
    information should be printed.} 
}
\details{
  This experimental function parses Rd files according to the
  specification given in
  \url{http://developer.r-project.org/parseRd.pdf}.  At the current
  writing, this is not identical to the rules used by other tools:  it
  is somewhat stricter. 
}
\value{
  An object of class \code{"Rd"}.  The internal format of this object is 
  subject to change.
  
  At present files which are marked as Latin-1 or UTF-8 or where
  \code{encoding} has one of those values result in text marked in the
  encoding. In a non-UTF-8 MBCS locale the result will be marked as
  UTF-8.  In all other cases files are assumed to be in the native encoding.
}
\section{Warning}{
  These functions are still experimental.  Names, interfaces and values
  might change in future versions.
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\author{ Duncan Murdoch }
\seealso{
  \code{\link{Rd2HTML}}.
  
  \code{\link{Rd_parse}}, the function used for parsing 
  Rd files by \command{R CMD check}.
}
\examples{
baseRd <- Rd_db("base")
con <- textConnection(baseRd[[1]], "rt")
parse_Rd(con)
close(con)
}
\keyword{utilities}
\keyword{documentation}
