% File src/library/stats/man/sortedXyData.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{sortedXyData}
\encoding{UTF-8}
\title{Create a \code{sortedXyData} Object}
\alias{sortedXyData}
\alias{sortedXyData.default}
\usage{
sortedXyData(x, y, data)
}
\arguments{
 \item{x}{ a numeric vector or an expression that will evaluate in
   \code{data} to a numeric vector }
 \item{y}{ a numeric vector or an expression that will evaluate in
   \code{data} to a numeric vector }
 \item{data}{ an optional data frame in which to evaluate expressions
   for \code{x} and \code{y}, if they are given as expressions }
}
\description{
  This is a constructor function for the class of \code{sortedXyData}
  objects.  These objects are mostly used in the \code{initial}
  function for a self-starting nonlinear regression model, which will be
  of the \code{selfStart} class.
}
\value{
  A \code{sortedXyData} object. This is a data frame with exactly
  two numeric columns, named \code{x} and \code{y}.  The rows are
  sorted so the \code{x} column is in increasing order.  Duplicate
  \code{x} values are eliminated by averaging the corresponding \code{y}
  values.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{selfStart}}, \code{\link{NLSstClosestX}},
  \code{\link{NLSstLfAsymptote}}, \code{\link{NLSstRtAsymptote}}
}
\examples{
DNase.2 <- DNase[ DNase$Run == "2", ]
sortedXyData( expression(log(conc)), expression(density), DNase.2 )
}
\keyword{manip}
