\name{MethodsList-class}
\alias{MethodsList-class}
\docType{class}
\title{Class MethodsList, Representation of Methods for a Generic Function}
\description{
  Objects from this class are generated and revised by the definition of
  methods for a generic function.
}
\section{Creating Objects}{
\code{  new('MethodsList',}\cr
\code{    methods    = ...., # Object of class "list"}\cr
\code{    argument   = ...., # Object of class "name"}\cr
\code{    allMethods = ...., # Object of class "list"}\cr
\code{    fromClass  = ...., # Object of class "character"}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{methods}:}{Object of class \code{"list"}}
    \item{\code{argument}:}{Object of class \code{"name"}}
    \item{\code{allMethods}:}{Object of class \code{"list"}}
    \item{\code{fromClass}:}{Object of class \code{"character"}}
}}
\section{Extends}{
  Class \code{"OptionalMethods"} directly.
}
\keyword{methods}
