% File src/library/tools/man/SweaveTexFilter.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2013 R Core Team
% Distributed under GPL 2 or later

\name{SweaveTeXFilter}
\alias{SweaveTeXFilter}
\title{
  Strip R code out of Sweave file
}
\description{
  This function blanks out code chunks and Noweb markup in an Sweave
  input file, for spell checking or other uses.
}
\usage{
SweaveTeXFilter(ifile, encoding = "unknown")
}
\arguments{
  \item{ifile}{
    Input file or connection.
  }
  \item{encoding}{
    Text encoding to pass to \code{\link{readLines}}.
  }
}
\details{
  This function blanks out all Noweb markup and code chunks from an
  Sweave input file, leaving behind the LaTeX source, so that a
  LaTeX-aware spelling checker can check it and report errors in their
  original locations.
}
\value{
  A character vector which if written to a file, one element per line,
  would duplicate the text elements of the original Sweave input file.
}
\author{
Duncan Murdoch
}
\seealso{
  \code{\link{aspell}}, for which this is used with \code{filter = "Sweave"}.
}
\keyword{ utilities }
\keyword{ documentation }
