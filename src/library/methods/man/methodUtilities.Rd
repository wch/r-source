% File src/library/methods/man/methodUtilities.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{methodUtilities}
\alias{functionBody}
\alias{functionBody<-}
\alias{allNames}
\alias{getFunction}
\alias{existsFunction}
\alias{el}
\alias{elNamed}
\alias{el<-}
\alias{elNamed<-}
\alias{formalArgs}
\alias{Quote}
\alias{showDefault}
\alias{initMethodDispatch}
\alias{methodSignatureMatrix}
\title{Utility Functions for Methods and S-Plus Compatibility}
\description{
  These are \emph{internal} utilities, currently in the \pkg{methods}
  package, that either provide some functionality needed by the package
  (e.g., element matching by name), or add compatibility with S-Plus, or both.
}
\usage{
functionBody(fun=sys.function(sys.parent()))

allNames(x)

getFunction(name, generic=TRUE, mustFind=TRUE, where)

el(object, where)

elNamed(x, name, mustFind=FALSE)

formalArgs(def)

Quote(expr)

showDefault(object, oldMethods = TRUE)

initMethodDispatch(where = topenv(parent.frame()))

methodSignatureMatrix(object, sigSlots = c("target", "defined"))
}
\section{Summary of Functions}{
  \describe{
    \item{\code{allNames}:}{
      the character vector of names (unlike \code{names()}, never
      returns \code{NULL}).
    }
    \item{\code{getFunction}:}{
      find the object as a function.
    }
    \item{\code{el}:}{\code{el(object, i)} is equivalent to
      \code{object[i][[1]]} (and should typically be replacable by object[[i]]).
    }
    \item{\code{elNamed}:}{
      get the element of the vector corresponding to name.  Unlike the
      \code{\link{[}}, \code{\link{[[}}, and \code{\link{$}} operators, this function
      requires \code{name} to match the element name exactly (no partial
      matching).
    }
    \item{\code{formalArgs}:}{
      Returns the names of the formal arguments of this function.
    }
    \item{\code{existsFunction}:}{
      Is there a function of this name?  If \code{generic} is
      \code{FALSE}, generic functions are not counted.
    }
    \item{\code{findFunction}:}{
      return all the indices of the search list on which a function
      definition for \code{name} exists.

      If \code{generic} is \code{FALSE}, ignore generic functions.
    }
    \item{\code{showDefault}:}{
      Utility, used to enable \code{show} methods to be called by the
      automatic printing (via \code{print.default}).

      Argument \code{oldMethods} controls whether old-style print
      methods are used for this object.  It is \code{TRUE} by default
      if called directly, but \code{FALSE} when called from the
      \pkg{methods} package for automatic printing (to avoid potential
      recursion).
    }
    \item{\code{initMethodDispatch}:}{
      Turn on the internal method dispatch code.  Called on loading
      the namespace.  Also, if dispatch has been turned off (by calling
      \code{.isMethodsDispatchOn(FALSE)}---a very gutsy thing to do),
      calling this function should turn dispatch back on again.
    }
    \item{\code{methodSignatureMatrix}:}{
      Returns a matrix with the contents of the specified slots as
      rows.  The slots should be named character strings of the same
      length.  Basically used to turn the signatures of a
      \code{"MethodDefinition"} object into a matrix for printing.
    }
    \item{\code{Quote}:}{is a synonym of \code{\link{quote}()} and
      considered deprecated.
    }
  }
}
\keyword{internal}
