\name{latex}
\title{Converting R Objects to BibTeX or LaTeX}
\alias{latex}
\alias{print.latex}
\alias{bibtex}
\alias{print.bibtex}
\description{
  These methods convert R objects to character vectors with
  BibTeX or LaTeX markup.
}
\usage{
bibtex(object, \dots)
latex(object, \dots)
\S3method{print}{bibtex}(x, prefix="", \dots)
\S3method{print}{latex}(x, prefix="", \dots)
}
\arguments{
  \item{object}{object of a class for which a \code{bibtex} or
    \code{latex} method exists.}
  \item{x}{object of class \code{"bibtex"} or
    \code{"latex"}.}
  \item{prefix}{a character string which is printed at the beginning of
    each line, mostly used to insert whitespace for indentation.}
  \item{\dots}{currently not used in the print methods.}
}
\details{
  Objects of class \code{"bibtex"} or \code{"latex"} are simply
  character vectors where each element holds one line of the
  corresponding BibTeX or LaTeX file.
} 
\seealso{\code{\link{citEntry}} and \code{\link{sessionInfo}} for examples}
\keyword{misc}
