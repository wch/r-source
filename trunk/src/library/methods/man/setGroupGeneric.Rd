% File src/library/methods/man/setGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setGroupGeneric}
\alias{setGroupGeneric}
\title{Create a Group Generic Version of a Function}
\description{

  The \code{setGroupGeneric} function behaves like \code{\link{setGeneric}}
  except that it constructs a group generic function, differing in two
  ways from an ordinary generic function.  First, this function cannot
  be called directly, and the body of the function created will contain
  a stop call with this information.  Second, the group generic function
  contains information about the known members of the group, used to
  keep the members up to date when the group definition changes, through
  changes in the search list or direct specification of methods, etc.

  All members of the group must have the identical argument list.
}
\usage{
setGroupGeneric(name, def= , group=list(), valueClass=character(),
                knownMembers=list(), package= , where= )
}
\arguments{
  \item{name}{the character string name of the generic function.
   }
  \item{def}{A function object.  There isn't likely to be an existing
    nongeneric of this name, so some function needs to be supplied.  Any
    known member or other function with the same argument list will do,
    because the group generic cannot be called directly.
  }
  \item{group, valueClass}{arguments to pass to
    \code{\link{setGeneric}}.
  }
  \item{knownMembers}{the names of functions that are
    known to be members of this group.  This information is used to
    reset cached definitions of the member generics when information
    about the group generic is changed.
  }
  \item{package, where}{passed to \code{\link{setGeneric}}, but
    obsolete and to be avoided.}
}
\value{
  The \code{setGroupGeneric} function exists for its side effect: saving the
  generic function to allow methods to be specified later.  It returns
  \code{name}.
}
\references{
 Chambers, John M. (2016)
 \emph{Extending R}
  Chapman & Hall
}
\examples{
\dontrun{
## the definition of the "Logic" group generic in the methods package
setGroupGeneric("Logic", function(e1, e2) NULL,
    knownMembers = c("&", "|"))
}
}

\seealso{
  \code{\link{Methods_Details}} and the links there for a general discussion,
  \code{\link{dotsMethods}} for methods that dispatch on
  \code{\dots}, and \code{\link{setMethod}} for method definitions.
}
\keyword{ programming }
\keyword{ methods }
