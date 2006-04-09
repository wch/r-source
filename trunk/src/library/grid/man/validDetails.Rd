\name{validDetails}
\alias{validDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Customising grid grob Validation }
\description{
  This generic hook function is called whenever a grid grob is created
  or
  edited
  via \code{grob}, \code{gTree}, \code{grid.edit} or \code{editGrob}.
  This provides an opportunity for customising the validation of a
  new class derived from grob (or gTree).
}
\usage{
validDetails(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A grid grob. }
}
\details{
  This function is called by \code{grob}, \code{gTree},
  \code{grid.edit} and \code{editGrob}.
  A method should be written for classes derived from grob or gTree
  to validate the values of slots specific to the new class.
  (e.g., see \code{grid:::validDetails.axis}).

  Note that the standard slots for grobs and gTrees are automatically
  validated (e.g., \code{vp}, \code{gp} slots for grobs and,
  in addition, \code{children}, and \code{childrenvp} slots for
  gTrees) so only slots specific to a new class need to be addressed.
}
\value{
  The function MUST return the validated grob.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.edit}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS
