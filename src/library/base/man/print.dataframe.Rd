% File src/library/base/man/print.dataframe.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{print.data.frame}
\title{Printing Data Frames}
\alias{print.data.frame}
\description{
  Print a data frame.
}
\usage{
\method{print}{data.frame}(x, \dots, digits = NULL, quote = FALSE, right = TRUE)
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
\keyword{print}
