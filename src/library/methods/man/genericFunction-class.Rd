% File src/library/methods/man/genericFunction-class.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{genericFunction-class}
\docType{class}
\alias{genericFunction-class}
\alias{groupGenericFunction-class}
\title{Generic Function Objects }
\description{
  Generic functions (objects from or extending class
  \code{genericFunction}) are extended function objects,
  containing information used in creating and dispatching methods for
  this function.  They also identify the package associated with the
  function and its methods.
}
\section{Objects from the Class}{
  Generic functions are created and assigned by
  \code{\link{setGeneric}} or \code{\link{setGroupGeneric}} and, indirectly, by
  \code{\link{setMethod}}.

  As you might expect \code{\link{setGeneric}} and
  \code{\link{setGroupGeneric}} create objects of class
  \code{"genericFunction"} and \code{"groupGenericFunction"} respectively.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"}, the
        function definition of the generic, usually created
        automatically as a call to \code{\link{standardGeneric}}. }
    \item{\code{generic}:}{Object of class \code{"character"}, the
        name of the generic function. }
    \item{\code{package}:}{Object of class \code{"character"}, the
        name of the package to which the function definition belongs
        (and \emph{not} necessarily where the generic function is
        stored). If the package is not specified explicitly in the
        call to \code{setGeneric}, it is usually the package on which
        the corresponding non-generic function exists. }
    \item{\code{group}:}{Object of class \code{"list"}, the group or
        groups to which this generic function belongs.  Empty by default. }
    \item{\code{valueClass}:}{Object of class \code{"character"}; if
        not an empty character vector, identifies one or more classes.  It is
        asserted that all methods for this function return objects
        from these class (or from classes that extend them). }
    \item{\code{signature}:}{Object of class \code{"character"}, the
        vector of formal argument names that can appear in the
        signature of methods for this generic function.  By default,
        it is all the formal arguments, except for \dots.  Order
        matters for efficiency:  the most commonly used arguments in
        specifying methods should come first. }
    \item{\code{default}:}{Object of class \code{"optionalMethod"}
        (a union of classes \code{"function"} and \code{"NULL"}), containing
        the default method for this function if any.  Generated
        automatically and used to initialize method dispatch. }
    \item{\code{skeleton}:}{Object of class \code{"call"}, a slot used
        internally in method dispatch.  Don't expect to use it
        directly.}
  }
}
\section{Extends}{
  Class \code{"function"}, from data part.\cr
  Class \code{"OptionalMethods"}, by class \code{"function"}.\cr
  Class \code{"PossibleMethod"}, by class \code{"function"}.
}
\section{Methods}{
  Generic function objects are used in the creation and dispatch of
  formal methods; information from the object is used to create methods
  list objects and to merge or update the existing methods for this
  generic.
}
\keyword{classes}
