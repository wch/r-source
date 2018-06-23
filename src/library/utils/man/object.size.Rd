% File src/library/utils/man/object.size.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{object.size}
\alias{object.size}
\alias{format.object_size}
\alias{print.object_size}
\title{Report the Space Allocated for an Object}
\description{
  Provides an estimate of the memory that is being used to store an \R object.
}
\usage{
object.size(x)

\method{format}{object_size}(x, units = "b", standard = "auto", digits = 1L, \dots)
\method{print}{object_size}(x, quote = FALSE, units = "b", standard = "auto",
      digits = 1L, \dots)
}
\arguments{
  \item{x}{an \R object.}
  \item{quote}{logical, indicating whether or not the result should be
    printed with surrounding quotes.}
  \item{units}{the units to be used in formatting and printing the size.
    Allowed values for the different \code{standard}s are
    \describe{
      \item{\code{standard = "legacy"}:}{
	\code{"b"}, \code{"Kb"}, \code{"Mb"}, \code{"Gb"}, \code{"Tb"}, \code{"Pb"},
	\code{"B"}, \code{"KB"}, \code{"MB"}, \code{"GB"}, \code{"TB"} and \code{"PB"}.}
      \item{\code{standard = "IEC"}:}{
	\code{"B"}, \code{"KiB"}, \code{"MiB"}, \code{"GiB"},
	\code{"TiB"}, \code{"PiB"}, \code{"EiB"}, \code{"ZiB"} and \code{"YiB"}.}
      \item{\code{standard = "SI"}:}{
	\code{"B"}, \code{"kB"}, \code{"MB"}, \code{"GB"}, \code{"TB"},
	\code{"PB"}, \code{"EB"}, \code{"ZB"} and \code{"YB"}.}
    }
    For all standards, \code{unit = "auto"} is also allowed.
    If \code{standard = "auto"}, any of the "legacy" and \acronym{IEC}
    units are allowed.
    See \sQuote{Formatting and printing object sizes} for details.}
  \item{standard}{the byte-size unit standard to be used.  A character
    string, possibly abbreviated from \code{"legacy"}, \code{"IEC"},
    \code{"SI"} and \code{"auto"}.  See \sQuote{Formatting and printing
      object sizes} for details.}
  \item{digits}{the number of digits after the decimal point, passed to
    \code{\link{round}}.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  Exactly which parts of the memory allocation should be attributed to
  which object is not clear-cut.  This function merely provides a rough
  indication: it should be reasonably accurate for atomic vectors, but
  does not detect if elements of a list are shared, for example.
  (Sharing amongst elements of a character vector is taken into account,
  but not that between character vectors in a single object.)

  The calculation is of the size of the object, and excludes the space
  needed to store its name in the symbol table.

  Associated space (e.g., the environment of a function and what the
  pointer in a \code{EXTPTRSXP} points to) is not included in the
  calculation.

  Object sizes are larger on 64-bit builds than 32-bit ones, but will
  very likely be the same on different platforms with the same word
  length and pointer size.

  Sizes of objects using a compact internal representation may be
  over-estimated.
}

\section{Formatting and printing object sizes}{
  Object sizes can be formatted using byte-size units from \R's legacy
  standard, the \acronym{IEC} standard, or the \acronym{SI} standard.
  As illustrated by below tables, the legacy and \acronym{IEC} standards use
  \emph{binary} units (multiples of 1024), whereas the SI standard uses
  \emph{decimal} units (multiples of 1000).

  For methods \code{format} and \code{print}, argument \code{standard}
  specifies which standard to use and argument \code{units} specifies
  which byte-size unit to use.  \code{units = "auto"} chooses the largest
  units in which the result is one or more (before rounding).
  Byte sizes are rounded to \code{digits} decimal places.
  \code{standard = "auto"} chooses the standard based on \code{units},
  if possible, otherwise, the legacy standard is used.

  Summary of \R's legacy and \acronym{IEC} units:
  \tabular{lll}{
   \bold{object size} \tab\bold{legacy} \tab\bold{IEC}\cr
     1                \tab  1 bytes     \tab  1 B    \cr
     1024             \tab  1 Kb        \tab  1 KiB  \cr
     1024^2           \tab  1 Mb        \tab  1 MiB  \cr
     1024^3           \tab  1 Gb        \tab  1 GiB  \cr
     1024^4           \tab  1 Tb        \tab  1 TiB  \cr
     1024^5           \tab  1 Pb        \tab  1 PiB  \cr
     1024^6           \tab              \tab  1 EiB  \cr
     1024^7           \tab              \tab  1 ZiB  \cr
     1024^8           \tab              \tab  1 YiB  \cr
  }

  Summary of \acronym{SI} units:
  \tabular{ll}{
   \bold{object size} \tab \bold{SI} \cr
     1       \tab  1 B   \cr
     1000    \tab  1 kB  \cr
     1000^2  \tab  1 MB  \cr
     1000^3  \tab  1 GB  \cr
     1000^4  \tab  1 TB  \cr
     1000^5  \tab  1 PB  \cr
     1000^6  \tab  1 EB  \cr
     1000^7  \tab  1 ZB  \cr
     1000^8  \tab  1 YB  \cr
  }
}
\value{
  An object of class \code{"object_size"} with a length-one double value,
  an estimate of the memory allocation attributable to the object in bytes.
}
\author{R Core; Henrik Bengtsson for the non-legacy \code{standard}s.}
\seealso{
  \code{\link{Memory-limits}} for the design limitations on object size.
}
\references{
  The wikipedia page, \url{https://en.wikipedia.org/wiki/Binary_prefix},
  is extensive on the different standards, usages and their history.
}
\examples{
object.size(letters)
object.size(ls)
format(object.size(library), units = "auto")

sl <- object.size(rep(letters, 1000))

print(sl)                                    ## 209288 bytes
print(sl, units = "auto")                    ## 204.4 Kb
print(sl, units = "auto", standard = "IEC")  ## 204.4 KiB
print(sl, units = "auto", standard = "SI")   ## 209.3 kB

(fsl <- sapply(c("Kb", "KB", "KiB"),
               function(u) format(sl, units = u)))
stopifnot(identical( ## assert that all three are the same :
             unique(substr(as.vector(fsl), 1,5)),
             format(round(as.vector(sl)/1024, 1))))

## find the 10 largest objects in the base package
z <- sapply(ls("package:base"), function(x)
            object.size(get(x, envir = baseenv())))
if(interactive()) {
as.matrix(rev(sort(z))[1:10])
} else # (more constant over time):
    names(rev(sort(z))[1:10])
}
\keyword{utilities}
