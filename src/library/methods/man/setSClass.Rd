% File src/library/methods/man/setSClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{makeClassRepresentation}
\alias{makeClassRepresentation}
\title{Create a Class Definition}
\description{
  Constructs an object of class \code{\linkS4class{classRepresentation}}
  to describe a particular class.  Mostly a utility function, but you can
  call it to create a class definition without assigning it, as
  \code{\link{setClass}} would do.
}
\usage{
makeClassRepresentation(name, slots=list(), superClasses=character(),
                        prototype=NULL, package, validity, access,
                        version, sealed, virtual=NA, where)
}
\arguments{
  \item{name}{character string name for the class}
  \item{slots}{named list of slot classes as would be supplied to
    \code{setClass}, but \emph{without} the unnamed arguments for
    superClasses if any.}
  \item{superClasses}{what classes does this class extend}
  \item{prototype}{an object providing the default data for the class,
    e.g, the result of a call to \code{\link{prototype}}.}
  \item{package}{The character string name for the package in which
    the class will be stored; see \code{\link{getPackageName}}.}
  \item{validity}{Optional validity method.  See
    \code{\link{validObject}}, and the discussion of validity methods in
    the reference.}
  \item{access}{Access information.  Not currently used.}
  \item{version}{Optional version key for version control.  Currently
    generated, but not used.}
  \item{sealed}{Is the class sealed? See \code{\link{setClass}}.}
  \item{virtual}{Is this known to be a virtual class?}

  \item{where}{The environment from which to look for class
      definitions needed (e.g., for slots or superclasses). See the
      discussion of this argument under \link{GenericFunctions}.}
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{
  \code{\link{setClass}}
}
\keyword{programming}
\keyword{classes}
