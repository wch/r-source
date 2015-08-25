% File src/library/base/man/as.data.frame.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{as.data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{as.data.frame.AsIs}
\alias{as.data.frame.character}
\alias{as.data.frame.complex}
\alias{as.data.frame.data.frame}
\alias{as.data.frame.default}
\alias{as.data.frame.factor}
\alias{as.data.frame.integer}
\alias{as.data.frame.list}
\alias{as.data.frame.logical}
\alias{as.data.frame.raw}
\alias{as.data.frame.matrix}
\alias{as.data.frame.model.matrix}
\alias{as.data.frame.noquote}
\alias{as.data.frame.numeric}
\alias{as.data.frame.ordered}
\alias{as.data.frame.ts}
\alias{as.data.frame.vector}
\alias{as.data.frame.array}
\alias{as.data.frame.difftime}

\title{
  Coerce to a Data Frame
}
\description{
  Functions to check if an object is a data frame, or coerce it if possible.
}
\usage{
as.data.frame(x, row.names = NULL, optional = FALSE, \dots)

\method{as.data.frame}{character}(x, \dots,
              stringsAsFactors = default.stringsAsFactors())

\method{as.data.frame}{matrix}(x, row.names = NULL, optional = FALSE, \dots,
              stringsAsFactors = default.stringsAsFactors())

is.data.frame(x)
}
\arguments{
  \item{x}{any \R object.}
  \item{row.names}{\code{NULL} or a character vector giving the row
    names for the data frame.  Missing values are not allowed.}
  \item{optional}{logical. If \code{TRUE}, setting row names and
    converting column names (to syntactic names: see
    \code{\link{make.names}}) is optional.}
  \item{\dots}{additional arguments to be passed to or from methods.}
  \item{stringsAsFactors}{logical: should the character vector be converted
    to a factor?}
}
\value{
  \code{as.data.frame} returns a data frame, normally with all row names
  \code{""} if \code{optional = TRUE}.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame (that is, has \code{"data.frame"} amongst its classes)
  and \code{FALSE} otherwise.
}
\details{
  \code{as.data.frame} is a generic function with many methods, and
  users and packages can supply further methods.  For classes that act
  as vectors, often a copy of \code{as.data.frame.vector} will work 
  as the method.

  If a list is supplied, each element is converted to a column in the
  data frame.  Similarly, each column of a matrix is converted separately.
  This can be overridden if the object has a class which has
  a method for \code{as.data.frame}: two examples are
  matrices of class \code{"\link{model.matrix}"} (which are
  included as a single column) and list objects of class
  \code{"\link{POSIXlt}"} which are coerced to class
  \code{"\link{POSIXct}"}.

  Arrays can be converted to data frames.  One-dimensional arrays are
  treated like vectors and two-dimensional arrays like matrices.  Arrays
  with more than two dimensions are converted to matrices by
  \sQuote{flattening} all dimensions after the first and creating
  suitable column labels.

  Character variables are converted to factor columns unless protected
  by \code{\link{I}}.

  If a data frame is supplied, all classes preceding \code{"data.frame"}
  are stripped, and the row names are changed if that argument is supplied.

  If \code{row.names = NULL}, row names are constructed from the names
  or dimnames of \code{x}, otherwise are the integer sequence
  starting at one.  Few of the methods check for duplicated row names.
  Names are removed from vector columns unless \code{\link{I}}.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{as.data.frame.table}} for the
  \code{table} method (which has additional arguments if called directly).
}
\keyword{classes}
\keyword{methods}
