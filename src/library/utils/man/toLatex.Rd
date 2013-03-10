% File src/library/utils/man/toLatex.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{toLatex}
\title{Converting R Objects to BibTeX or LaTeX}
\alias{toLatex}
\alias{print.Latex}
\alias{toBibtex}
\alias{print.Bibtex}
\description{
  These methods convert \R objects to character vectors with
  BibTeX or LaTeX markup.
}
\usage{
toBibtex(object, \dots)
toLatex(object, \dots)
\S3method{print}{Bibtex}(x, prefix = "", \dots)
\S3method{print}{Latex}(x, prefix = "", \dots)
}
\arguments{
  \item{object}{object of a class for which a \code{toBibtex} or
    \code{toLatex} method exists.}
  \item{x}{object of class \code{"Bibtex"} or
    \code{"Latex"}.}
  \item{prefix}{a character string which is printed at the beginning of
    each line, mostly used to insert whitespace for indentation.}
  \item{\dots}{in the print methods, passed to \code{\link{writeLines}}.}
}
\details{
  Objects of class \code{"Bibtex"} or \code{"Latex"} are simply
  character vectors where each element holds one line of the
  corresponding BibTeX or LaTeX file.
}
\seealso{\code{\link{citEntry}} and \code{\link{sessionInfo}} for examples}
\keyword{misc}
