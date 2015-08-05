% File src/library/methods/man/findClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{findClass}
\title{Computations with Classes}
\alias{removeClass}
\alias{resetClass}
\alias{isClass}
\alias{getClasses}
\alias{findClass}
\alias{sealClass}
\description{
  Functions to find and manipulate class definitions.
}
\usage{
isClass(Class, formal=TRUE, where)

getClasses(where, inherits = missing(where))

findClass(Class, where, unique = "")

removeClass(Class, where,
            resolve.msg = getOption("removeClass.msg", default=TRUE))

resetClass(Class, classDef, where)

sealClass(Class, where)

}
\arguments{
  \item{Class}{character string name for the class.  The functions will
    usually take a class definition instead of the string.  To restrict
    the class to those defined in a particular package, set the
    \code{\link{packageSlot}} of the character string.}
  \item{where}{the \code{\link{environment}} in which to modify or
    remove the definition.  Defaults to the top-level environment of the
    calling function (the global environment for ordinary computations,
    but the environment or namespace of a package in the source for a
    package).

    When searching for class definitions, \code{where} defines where to
    do the search, and the default is to search from the top-level
    environment or namespace of the caller to this function.
  }
  \item{formal}{\code{\link{logical}} indicating if a formal definition
    is required.}
  \item{unique}{if \code{findClass} expects a unique location for the
    class, \code{unique} is a character string explaining the purpose
    of the search (and is used in warning and error messages).  By
    default, multiple locations are possible and the function always
    returns a list.
  }
  \item{inherits}{in a call to \code{getClasses}, should the value
    returned include all parent environments of \code{where}, or that
    environment only?  Defaults to \code{TRUE} if \code{where} is
    omitted, and to \code{FALSE} otherwise.
  }
  \item{resolve.msg}{\code{\link{logical}} indicating if \R should
    \code{\link{message}()} its decision if \code{Class} is found in
    multiple namespaces and one is chosen.}
  \item{classDef}{ For \code{resetClass}, the optional class
    definition (but usually it's better for \code{Class} to be the
    class definition, and to omit \code{classDef}).
  }
}

\details{
  These are the functions that test and manipulate formal class
  definitions.  Brief documentation is provided below.  See the
  references for an introduction and for more details.

  \describe{
    \item{\code{removeClass}:}{
      Remove the definition of this class, from the environment
      \code{where} if this argument is supplied; if not,
      \code{removeClass} will search for a definition, starting in the
      top-level environment of the call to \code{removeClass}, and
      remove the (first) definition found.
    }
    \item{\code{isClass}:}{
      Is this the name of a formally defined class? (Argument
      \code{formal} is for compatibility and is ignored.)
    }
    \item{\code{getClasses}:}{
      The names of all the classes formally defined on \code{where}.  If
      called with no argument, all the classes visible from the
      calling function (if called from the top-level, all the classes
      in any of the environments on the search list).  The
      \code{inherits} argument can be used to search a particular
      environment and all its parents, but usually the default setting
      is what you want.
    }
    \item{\code{findClass}:}{
      The list of environments or positions on the search list in
      which a class definition of \code{Class} is found.  If
      \code{where} is supplied, this is an environment (or namespace)
      from which the search takes place; otherwise the top-level
      environment of the caller is used.  If \code{unique} is supplied
      as a character string, \code{findClass} returns a single
      environment or position.  By default, it always returns a list.
      The calling function should select, say, the first element as a
      position or environment for functions such as \code{\link{get}}.

      If \code{unique} is supplied as a character string,
      \code{findClass} will warn if there is more than one definition
      visible (using the string to identify the purpose of the call),
      and will generate an error if no definition can be found.
    }
    \item{\code{resetClass}:}{
      Reset the internal definition of a class.  Causes the complete
      definition of the class to be re-computed, from the
      representation and superclasses specified in the original
      call to \code{setClass}.

      This function is called when aspects of the class definition are
      changed.  You would need to call it explicitly if you changed the
      definition of a class that this class extends (but doing that in
      the  middle of a session is living dangerously, since it may
      invalidate existing objects).
    }
    \item{\code{sealClass}:}{ Seal the current definition of the specified
      class, to prevent further changes.  It is possible to seal a class
      in the call to \code{setClass}, but sometimes further changes have
      to be made (e.g., by calls to \code{setIs}).  If so, call
      \code{sealClass} after all the relevant changes have been made.
    }
  }
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
  \code{\link{setClassUnion}},
  \code{\link{Methods}},
  \code{\link{makeClassRepresentation}}
}
%% should add Examples section
\keyword{programming}
\keyword{classes}
\keyword{methods}
