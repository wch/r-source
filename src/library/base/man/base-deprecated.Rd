\name{base-deprecated}
\alias{base-deprecated}
\alias{symbol.C}
\alias{symbol.For}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
%% put usage here.
symbol.C(name)
symbol.For(name)
}
\arguments{
  \item{name}{a character string giving either the name of a C function
    or Fortran subroutine.}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  Functions \code{symbol.C} and \code{symbol.For} are for historical
  compatibility with S.  \code{symbol.C} just returns its argument
  unchanged, whereas \code{symbol.For} will map to lower case and
  append an underscore on those platforms (almost all) which do so for
  Fortran symbols.  These are no longer of much use in \R.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{base-defunct}}
}
\keyword{misc}
