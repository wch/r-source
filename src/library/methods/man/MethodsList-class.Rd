\name{MethodsList-class}
\alias{MethodsList-class}
\doctype{class}
\title{Class MethodsList, Representation of Methods for a Generic Function }
\description{ Objects from this class are generated and revised by the
  definition of methods for a generic function.}
\section{Creating Objects}{\code{
  new('MethodsList',
     methods     = ...., # Object of class list
     argument    = ...., # Object of class name
     allMethods  = ...., # Object of class list
     fromClass   = ...., # Object of class character
  )}}
\section{Slots}{\describe{

  \item{\code{methods}:}{Object of class "list" ~~ }
  \item{\code{argument}:}{Object of class "name" ~~ }
  \item{\code{allMethods}:}{Object of class "list" ~~ }
  \item{\code{fromClass}:}{Object of class "character" ~~ }
}}


\section{Extends}{
Class "OptionalMethods" directly.
}
\keyword{methods}