% File src/library/methods/man/methods-deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{methods-deprecated}
\alias{methods-deprecated}
\alias{standardGeneric-class}
\alias{standardGenericWithTrace-class}
\alias{nonstandardGeneric-class}
\alias{nonstandardGenericWithTrace-class}
\alias{nonstandardGenericFunction-class}
\alias{nonstandardGroupGenericFunction-class}
%----- NOTE:  ../R/methods-deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{methods}}
%----- PLEASE: put \alias{.} here for EACH ! --- Do not remove this file, even when ``empty''
%
\description{
  These classes, methods, and functions are provided for compatibility
  with older versions of \R only, and may be defunct as soon as the next
  release.
}
\usage{
## Deprecated in R 4.7.0
"standardGeneric"
"standardGenericWithTrace"
"nonstandardGeneric"
"nonstandardGenericWithTrace"
"nonstandardGenericFunction"
"nonstandardGroupGenericFunction"
}
\details{
  \code{"standardGeneric"} and \code{"nonstandardGenericFunction"} are
  subclasses of \code{"\linkS4class{genericFunction}"} intended to
  distinguish non-group generic functions that only dispatch, whose body
  is a call to \code{\link{standardGeneric}}, from ones that perform
  additional operations before or after dispatch, whose body is a call
  to \code{\link{\{}} only eventually calling \code{standardGeneric}.
  From \R{} 4.7.0, they are deprecated: \code{\link{setGeneric}}
  continues to create objects from these classes, but user and package
  code must test only for inheritance from \code{"genericFunction"},
  conditioning on \preformatted{is.call(body(.)) &&
  identical(body(.)[[1L]], quote(standardGeneric))} in the rare case
  that distinction is needed.  It is intended that from \R{} 4.8.0,
  \code{setGeneric} will create objects of class
  \code{"genericFunction"} instead.

  Similarly, \code{"nonstandardGroupGenericFunction"} is a subclass of
  \code{"\linkS4class{groupGenericFunction}"} designating group generic
  functions that perform additional operations before or after dispatch.
  From \R{} 4.7.0, it is deprecated: \code{\link{setGroupGeneric}}
  continues to create objects from this class conditionally, but user
  and package code must test only for inheritance from
  \code{"groupGenericFunction"}.  It is intended that from \R{} 4.8.0,
  \code{setGroupGeneric} will create objects of class
  \code{"groupGenericFunction"} instead.

  \code{"standardGenericWithTrace"} is a subclass of
  \code{"standardGeneric"} also extending
  \code{"\linkS4class{traceable}"}.  It is used to represent standard
  non-group generic functions with tracing code.  From \R{} version
  4.7.0, it is deprecated: \code{\link{trace}} continues to create
  objects from this class conditionally, but user and package code must
  test only for inheritance from \code{"genericFunction"} and
  \code{"traceable"}.  It is intended that from \R{} 4.8.0, \code{trace}
  will create objects of class
  \code{"\linkS4class{genericFunctionWithTrace}"} instead, as a
  consequence of the change in behaviour of \code{setGeneric}.

  \code{"nonstandardGeneric"} is a virtual class containing the three
  other \code{"nonstandardGeneric*"}.
  \code{"nonstandardGenericWithTrace"} is nowhere used: because
  \code{"nonstandardGeneric"} is virtual, objects of that class are
  never created and hence never traced.  Both may be removed in \R{}
  4.8.0.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{methods-defunct}}
}
\keyword{internal}
\keyword{misc}
