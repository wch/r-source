\name{methodUtilities}
\alias{functionBody}
\alias{functionBody<-}
\alias{allNames}
\alias{getFunction}
\alias{el}
\alias{elNamed}
\alias{el<-}
\alias{elNamed<-}
\alias{formalArgs}
\alias{Quote}
\alias{message}
\alias{showDefault}
\title{Utility Functions for Methods and S-Plus Compatibility}
\description{
  These are utilities, currently in the methods package, that either
  provide some functionality needed by the package (e.g., element
  matching by name), or add compatibility with S-Plus, or both.
}
\usage{
functionBody(fun=sys.function(sys.parent()))

allNames(x)

getFunction(name, generic=TRUE, mustFind=TRUE, where)

el(object, where)

elNamed(x, name, mustFind=FALSE)

formalArgs(def)

Quote()

message(...)

showDefault(object, printTo = stdout(), oldMethods = TRUE)
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
  
    \item{\code{elNamed}:}{ 
      get the element of the vector corresponding to name.  Unlike the
      \code{[}, \code{[[}, and \code{$} operators, this function
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
    \item{\code{message}:}{ 
      Output all the arguments, pasted together with no intervening
      spaces.
    }
    \item{\code{showDefault}:}{ 
      Utility, used to enable \code{show} methods to be called by the
      automatic printing (via \code{print.default}).
    }
  }
}
\keyword{internal}
