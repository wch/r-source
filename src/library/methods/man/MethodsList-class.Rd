\name{MethodsList-class}
\alias{MethodsList-class}
\alias{body<-,MethodDefinition-method}
\docType{class}
\title{Class MethodsList, Representation of Methods for a Generic Function }
\description{ Objects from this class are generated and revised by the
  definition of methods for a generic function.}
\section{Slots}{\describe{

  \item{\code{argument}:}{Object of class \code{"name"}.  The name of the
    argument being used for dispatch at this level. }
  \item{\code{methods}:}{A named list of the methods (and method lists)
    defined \emph{explicitly} for this argument. 
 The names are the names of classes, and the corresponding
  element defines the method or methods to be used if the corresponding
  argument has that class.  See the details below.}
  \item{\code{allMethods}:}{A named list,  contains
    all the directly defined methods from the \code{methods} slot, plus
    any inherited methods.  Ignored when methods tables are used for dispatch (see \link{Methods} }
}}

\details{
  Suppose a function \code{f} has
  formal arguments \code{x} and \code{y}.  The methods list object for
  that function has the object \code{as.name("x")} as its
  \code{argument} slot.  An element of the methods named \code{"track"}
  is selected if the actual argument corresponding to \code{x} is an
  object of class \code{"track"}.  If there is such an element, it can
  generally be either a function or another methods list object.

  In the first case, the function defines the method to use for any call
  in which \code{x} is of class \code{"track"}.  In the second case, the
  new methods list object defines the available methods depending on
  the remaining formal arguments, in this example, \code{y}. 

  Each method  corresponds conceptually to a \emph{signature};
  that is a named list of classes, with names corresponding to some or
  all of the formal arguments.  In the previous example, if selecting
  class \code{"track"} for \code{x}, finding that the selection was
  another methods list and then selecting class \code{"numeric"} for
  \code{y} would produce a method associated with the signature
  \code{x = "track", y = "numeric"}.

  You can see the methods
  arranged by signature by calling the function
  \code{\link{showMethods}}, 
  A methods list can be converted into an ordinary list with the methods arranged this
  way (in two different forms)  by calling the functions
  \code{\link{listFromMlist}} and \code{\link{linearizeMlist}}.

}

\section{Extends}{
Class \code{"OptionalMethods"}, directly.
}
\keyword{classes}
\keyword{methods}
