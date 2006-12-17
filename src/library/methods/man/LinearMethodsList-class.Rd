\name{LinearMethodsList-class}
\docType{class}
\alias{LinearMethodsList-class}
\title{Class "LinearMethodsList"}
\description{
  A version of methods lists that has been \dQuote{linearized}
  for producing summary information.  The actual objects from class
  \code{"MethodsList"} used for method dispatch are defined recursively
  over the arguments involved.
}
\section{Objects from the Class}{
  The function \code{\link{linearizeMlist}} converts an ordinary methods
  list object into the linearized form.
}
\section{Slots}{
  \describe{
    \item{\code{methods}:}{Object of class \code{"list"}, the method
      definitions.}
    \item{\code{arguments}:}{Object of class \code{"list"}, the
      corresponding formal arguments, namely as many of the arguments
      in the signature of the generic function as are active in the
      relevant method table. }
    \item{\code{classes}:}{Object of class \code{"list"}, the
      corresponding classes in the signatures. }
    \item{\code{generic}:}{Object of class \code{"genericFunction"};
        the generic function to which the methods correspond. }
  }
}

\section{Future Note}{
  The current version of \code{linearizeMlist} does not take advantage of
  the \code{MethodDefinition} class, and therefore does more work for less
  effect than it could.  In particular, we may move to redefine both the
  function and the class to take advantage of the stored signatures.
  Don't write code depending precisely on the present form, although all
  the current information will be obtainable in the future.
}
\seealso{ Function \code{\link{linearizeMlist}} for the computation,
  and \code{\link{MethodsList-class}} for the original, recursive form.
}

\keyword{classes}
