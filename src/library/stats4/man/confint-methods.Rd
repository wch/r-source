\name{confint-methods}
\docType{methods}
\alias{confint-methods}
\alias{confint,ANY-method}
\alias{confint,profile.mle-method}
\alias{confint,mle-method}
\title{Methods for Function `confint' in Package `stats4'}
\description{Generate confidence intervals}
\section{Methods}{\describe{

\item{object = "ANY"}{Generic function: see \code{\link[stats]{confint}}.}

\item{object = "mle"}{First generate profile and then confidence
  intervals from the profile.}

\item{object = "profile.mle"}{Generate confidence intervals based on
  likelihood profile.}
}}
\keyword{methods}
