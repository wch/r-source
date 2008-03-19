% File src/library/methods/man/methods-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{methods-deprecated}
\alias{methods-deprecated}
\alias{getAllMethods}
\alias{mlistMetaName}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Methods package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
getAllMethods(f, fdef, where)
mlistMetaName (name, package)
}
\section{Summary of Functions}{

  \describe{
    \item{\code{getAllMethods}:}{
        This function replicates the behavior of \code{\link{getMethods}}, which should be called instead to obtain approximately the old behavior of \code{getAllMethods}.  However, a better substitution is to use \code{\link{findMethods}}, which returns a simpler structure.

        (The following is the original description, which has not been accurate since approximately version 2.5.0 of R.

      A generic function (with methods) representing the merge of all
      the methods defined for this generic starting from environment
      \code{where}, including all parent environments.  By default,
      uses the global environment (and therefore all packages on the
      search list).  This function exists largely
      to re-compute the full set of methods when a change to the
      available methods occurs.  Since all such recomputations are
      supposed to be automatic, direct calls to \code{getAllMethods}
      should not be needed.

      If the generic \code{f} has a group generic, methods for this
      group generic (and further generations of group generics, if any)
      are also merged.

      The merging rule is as follows: each generic is merged across
      packages, and the group generics are then merged, finally adding
      the directly defined methods of \code{f}.

      The effect of the merging rule is that any method directly defined
      for \code{f} on any included package overrides a method for the
      same signature defined for the group generic; similarly for the
      group generic and its group, if any, etc.

      For \code{f} or for a specific group generic, methods override in
      the order of the packages being searched.  A method for a
      particular signature on a particular package overrides any methods
      for the same signature on packages later on in the list of
      packages being searched.

      The slot \code{"allMethods"} of the merged methods list is set to
      a copy of the methods slot; this is the slot where inherited
      methods are stored.)
    }

    \item{\code{mlistMetaName}:}{Utility to  return the mangled
      name for the object that stored the \code{MethodsList} representation of the methods for a particular generic function, typically in the environment of a package.
These objects are no longer used, and will likely not be generated starting in version 2.8.0 of R.
}

}
}

\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the methods package are listed in
  \code{help("pkg-deprecated")}.

}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
