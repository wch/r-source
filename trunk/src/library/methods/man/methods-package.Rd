% File src/library/methods/man/methods-package.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{methods-package}
\alias{methods-package}
\docType{package}
\title{
Formal Methods and Classes
}
\description{
Formally defined methods and classes for R objects, plus
other programming tools, as described in the references.
}
\details{
  This package provides the \dQuote{S4} or \dQuote{S version 4}
  approach to methods and classes in a functional language.

  For basic use of the techniques, start with \link{Introduction} and
  follow the links there to the key functions for programming, notably
  \code{\link{setClass}} and \code{\link{setMethod}}.

  Some specific topics:

\describe{
    \item{Classes:}{ Creating one, see \code{\link{setClass}};
        examining definitions, see \code{\link{getClassDef}} and
        \linkS4class{classRepresentation}; inheritance and coercing,
      see \code{\link{is}} and \code{\link{as}}
      }

  \item{Generic functions:}{  Basic programming, see
      \code{\link{setGeneric}}; the class of objects, see
      \linkS4class{genericFunction}; other functions to examine or
      manipulate them, see \link{GenericFunctions}.
      }
   \item{S3:}{Using classes, see \code{\link{setOldClass}}; methods,
       see \link{Methods_for_S3}.
     }
     \item{Reference classes:}{See \link{ReferenceClasses}.
         }
    \item{Class unions; virtual classes}{See \code{\link{setClassUnion}}.
         }
 
}
These pages will have additional links to related topics.

  For a complete
  list of functions and classes, use \code{library(help="methods")}.
}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (Chapter 10 has some additional details.)

}
\author{
R Core Team

Maintainer: R Core Team \email{R-core@r-project.org}
}
\keyword{ package }
\keyword{ methods }
