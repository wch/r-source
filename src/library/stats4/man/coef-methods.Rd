\name{coef-methods}
\docType{methods}
\alias{coef-methods}
\alias{coef,ANY-method}
\alias{coef,mle-method}
\alias{coef,summary.mle-method}
\title{Methods for Function `coef' in Package `stats4'}
\description{
  Extract the coefficient vector from \code{"mle"} objects.
}
\section{Methods}{\describe{

\item{object = "ANY"}{Generic function: see \code{\link[stats]{coef}}.}

\item{object = "mle"}{Extract the full coefficient vector (including any
  fixed coefficients) from the fit.}
\item{object = "summary.mle"}{Extract the coefficient vector and standard
  errors from the summary of the fit.} 
}}
\keyword{methods}
