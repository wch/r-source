\name{print.data.frame}
\title{Printing Data Frames}
\alias{print.data.frame}
\description{
  These functions create or manipulate data frames, tightly coupled
  collections of variables which share many of the properties of
  matrices and of lists, used as the fundamental data structure by most
  of \R's modeling software.
}
\usage{
\method{print}{data.frame}(x, \dots, digits = NULL, quote = FALSE, right = TRUE)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{\dots}{optional arguments to \code{print} or \code{plot} methods.}
  \item{digits}{the minimum number of significant digits to be used.}
  \item{quote}{logical, indicating whether or not strings
    (\code{\link{character}}s) should be printed with surrounding quotes.}
  \item{right}{logical, indicating whether or not strings should be
    right-aligned. The default is left-alignment.}
}
\value{
  For the \code{\link{print}} method (\code{print.data.frame}), see
  \code{\link{print.matrix}}.
}
}
\seealso{
  \code{\link{data.frame}}.
}
\keyword{print}
