\name{mle-class}
\docType{class}
\alias{mle-class}
\title{Class "mle". Result of maximum likelihood estimation.}
\description{This class encapsulates results of a generic maximum
  likelihood procedure.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mle", ...)}, but
most often as the result of a call to \code{mle}.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"}. The call to \code{mle}}
    \item{\code{coef}:}{Object of class \code{"numeric"}. Estimated parameters}
    \item{\code{vcov}:}{Object of class \code{"matrix"}. Approximate
      variance-covariance matrix}
    \item{\code{min}:}{Object of class \code{"numeric"}. Minimum value
      of objective function.}
    \item{\code{details}:}{Object of class \code{"list"}. List returned
      from \code{optim} }
    \item{\code{minuslogl}:}{Object of class \code{"function"}. The
      negative loglikelihood function.}
    \item{\code{method}:}{Object of class \code{"character"}. The
      optimization method used.}
  }
}
\section{Methods}{
  \describe{
    \item{confint}{\code{signature(object = "mle")}: Confidence
      intervals from likelihood profiles.}
    \item{profile}{\code{signature(fitted = "mle")}: Likelihood profile
      generation.}
    \item{show}{\code{signature(object = "mle")}: Display object
      briefly.}
    \item{summary}{\code{signature(object = "mle")}: Generate object summary.}
  }
}
\keyword{classes}
