% File src/library/grid/man/grid.collection.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.collection}
\alias{grid.collection}
\title{Create a Coherent Group of Grid Graphical Objects}
\description{
  This function is deprecated; please use \code{gTree}.

  This function creates a graphical object which contains
  several other graphical objects.  When it is drawn, it draws
  all of its children.

  It may be convenient to name the elements of the collection.
}
\usage{
grid.collection(..., gp=gpar(), draw=TRUE, vp=NULL)
}
\arguments{
  \item{\dots}{Zero or more objects of class \code{"grob"}.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value to indicate whether to produce
    graphical output.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  A collection grob.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}
\keyword{dplot}

