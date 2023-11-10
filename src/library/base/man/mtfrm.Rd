% File src/library/base/man/mtfrm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2021-2023 R Core Team
% Distributed under GPL 2 or later

\name{mtfrm}
\title{Auxiliary Function for Matching}
\alias{mtfrm}
\alias{mtfrm.default}
\alias{mtfrm.POSIXct}
\alias{mtfrm.POSIXlt}
\description{
  Transform objects for matching via \code{\link{match}()}, think
  \dQuote{match form} \code{-> "mtfrm"}.
  \pkg{base} provides the S3 generic and a \code{default} plus
  \code{"\link{POSIXct}"} and \code{"POSIXlt"} methods.
}
\usage{
mtfrm(x)
}
\arguments{
  \item{x}{an \R object}
}
\details{
  Matching via \code{\link{match}} will use \code{mtfrm} to transform
  internally classed objects (see \code{\link{is.object}}) to a vector
  representation appropriate for matching.  The default method performs
  \code{\link{as.character}} if this preserves the length.

  Ideally, methods for \code{mtfrm} should ensure that comparisons of
  same-classed objects via \code{\link{match}} are consistent with those
  employed by methods for \code{\link{duplicated}}/\code{\link{unique}}
  and \code{\link{==}}/\code{\link{!=}} (where applicable).
}
\value{
  A vector of the same length as \code{x}.
}
