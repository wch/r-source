% File src/library/methods/man/signature-class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{signature-class}
\docType{class}
\alias{signature-class}
\alias{show,signature-method}
\title{Class \code{"signature"} For Method Definitions}
\description{This class represents the mapping of some of the formal
  arguments of a function onto the corresponding classes.  It is used for
  two slots in the \code{\linkS4class{MethodDefinition}} class.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("signature",
    functionDef, ...)}.  The \code{functionDef} argument, if it is
  supplied as a function object, defines the formal names.  The other
  arguments define the classes.  More typically, the objects are
  created as side effects of defining methods.  Either way, note that
  the classes are expected to be well defined, usually because the
  corresponding class definitions exist.  See the comment on the
  \code{package} slot.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"} the class names. }
    \item{\code{names}:}{Object of class \code{"character"} the
      corresponding argument names. }
    \item{\code{package}:}{Object of class \code{"character"} the
        names of the packages corresponding to the class names. The
        combination of class name and package uniquely defines the
        class.  In principle, the same class name could appear in more
      than one package, in which case the \code{package} information
      is required for the signature to be well defined.}  }
}
\section{Extends}{
Class \code{"character"}, from data part.
Class \code{"vector"}, by class "character".
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(object = "signature")}: see the
      discussion of objects from the class, above. }
  }
}
\seealso{
  class \code{\linkS4class{MethodDefinition}} for the use of this class.
}

\keyword{classes}
