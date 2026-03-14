% File src/library/utils/man/bitstring.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{bitstring}
\title{Bit-level representation of R atomic vectors}
\alias{bitstring}
\alias{bitstring.default}
\alias{bitstring.raw}
\alias{bitstring.integer}
\alias{bitstring.numeric}
\alias{format.bitstring}
\alias{print.bitstring}

\usage{

bitstring(x)

\method{bitstring}{raw}(x)
\method{bitstring}{integer}(x)
\method{bitstring}{numeric}(x)
\method{bitstring}{default}(x)

\method{format}{bitstring}(x, sep = " ", ...)
\method{print}{bitstring}(x, ...)

}

\arguments{

  \item{x}{ For \code{bitsring()}, an atomic R vector. Currently
    integer, numeric, and raw vectors are supported.

    For the \code{\link{format}} and \code{\link{print}} methods, an
    object of class \code{"bitstring"}.
  }
  \item{sep}{ character string to be used as visual breakpoints between
    sign bit, mantissa, and exponent, as appropriate.
  }
  \item{...}{ additional arguments for compatibility; ignored. }
}

\description{
  This is a simple user-friendly wrapper for \code{\link{intToBits}}
  etc., inspired by the Julia function \code{bitstring}.
}
\value{
  An object of class \code{"bitstring"} which stores the bit patterns of
  the underlying vector as character strings, along with type
  information.

  \code{format.bitstring} formats a bitstring object for user-friendly
  display.
}
\examples{

x <- bitstring(pi^((-3):3))
x
print(x, sep = "-")

bitstring(seq(-3, 3))
bitstring(seq(-3, 3) |> as.numeric())
bitstring(c(NA_real_, NaN, Inf, -Inf))
bitstring(NA_integer_)
bitstring(charToRaw("bits"))

}
\keyword{utilities}
