\name{makeClassRepresentation}
\alias{makeClassRepresentation}
\title{Create a Class Definition}
\description{
  Constructs a \code{\link{classRepresentation-class}} object to
  describe a particular class.  Mostly a utility function, but you can
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
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\seealso{
  \code{\link{setClass}}
}
\keyword{programming}
\keyword{classes}
