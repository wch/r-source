% File src/library/base/man/print.dataframe.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{print.data.frame}
\title{Printing Data Frames}
\alias{print.data.frame}
\description{
  Print a data frame.
}
\usage{
\method{print}{data.frame}(x, \dots, digits = NULL,
      quote = FALSE, right = TRUE, row.names = TRUE)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{\dots}{optional arguments to \code{print} or \code{plot} methods.}
  \item{digits}{the minimum number of significant digits to be used: see
    \code{\link{print.default}}.}
  \item{quote}{logical, indicating whether or not entries should be
    printed with surrounding quotes.}
  \item{right}{logical, indicating whether or not strings should be
    right-aligned.  The default is right-alignment.}
  \item{row.names}{logical (or character vector), indicating whether (or
    what) row names should be printed.}
}
\details{
  This calls \code{\link{format}} which formats the data frame
  column-by-column, then converts to a character matrix and dispatches
  to the \code{print} method for matrices.

  When \code{quote = TRUE} only the entries are quoted not the row names
  nor the column names.
}
\seealso{
  \code{\link{data.frame}}.
}
\examples{
(dd <- data.frame(x = 1:8, f = gl(2,4), ch = I(letters[1:8])))
     # print() with defaults
print(dd, quote = TRUE, row.names = FALSE)
     # suppresses row.names and quotes all entries
}
\keyword{print}
