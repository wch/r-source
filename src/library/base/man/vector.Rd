% File src/library/base/man/vector.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{vector}
\alias{vector}
\alias{as.vector}
\alias{as.vector.factor}
\alias{is.vector}
\alias{atomic} % for cat.Rd and read.table.Rd
\title{Vectors}
\usage{
vector(mode = "logical", length = 0)
as.vector(x, mode = "any")
is.vector(x, mode = "any")
}
\arguments{
  \item{mode}{character string naming an atomic mode or
    \code{"list"} or \code{"expression"} or (except for \code{vector})
    \code{"any"}.  Currently, \code{is.vector()} allows any type (see
    \code{\link{typeof}}) for \code{mode}, and when mode is not
    \code{"any"}, \code{is.vector(x, mode)} is almost the same as
    \code{typeof(x) == mode}.}% differences: for mode=NA, or mode="numeric"
  \item{length}{a non-negative integer specifying the desired length.
    For a long vector, i.e., \code{length > .Machine$integer.max}, it
    has to be of type \code{"double"}.  Supplying an argument of length
    other than one is an error.}
  \item{x}{an \R object.}
}
\description{
  \code{vector} produces a vector of the given length and mode.

  \code{as.vector}, a generic, attempts to coerce its argument into a
  vector of mode \code{mode} (the default is to coerce to whichever
  vector mode is most convenient): if the result is atomic all
  attributes are removed.

  \code{is.vector} returns \code{TRUE} if \code{x} is a vector of the
  specified mode having no attributes \emph{other than names}.  It returns
  \code{FALSE} otherwise.
}

\details{
  The atomic modes are \code{"logical"}, \code{"integer"},
  \code{"numeric"} (synonym \code{"double"}), \code{"complex"},
  \code{"character"} and \code{"raw"}.

  If \code{mode = "any"}, \code{is.vector} may return \code{TRUE} for
  the atomic modes, \code{\link{list}} and \code{\link{expression}}.
  For any \code{mode}, it will return \code{FALSE} if \code{x} has any
  attributes except names.  (This is incompatible with S.)  On the other
  hand, \code{as.vector} removes \emph{all} attributes including names
  for results of atomic mode (but not those of mode \code{"list"} nor
  \code{"expression"}).

  Note that factors are \emph{not} vectors; \code{is.vector} returns
  \code{FALSE} and \code{as.vector} converts a factor to a character
  vector for \code{mode = "any"}.
}

\value{
  For \code{vector}, a vector of the given length and mode.  Logical
  vector elements are initialized to \code{FALSE}, numeric vector
  elements to \code{0}, character vector elements to \code{""}, raw
  vector elements to \code{nul} bytes and list/expression elements to
  \code{NULL}.

  For \code{as.vector}, a vector (atomic or of type list or expression).
  All attributes are removed from the result if it is of an atomic mode,
  but not in general for a list result.  The default method handles 24
  input types and 12 values of \code{type}: the details of most
  coercions are undocumented and subject to change.

  For \code{is.vector}, \code{TRUE} or \code{FALSE}.
  \code{is.vector(x, mode = "numeric")} can be true for vectors of types
  \code{"integer"} or \code{"double"} whereas \code{is.vector(x, mode =
  "double")} can only be true for those of type \code{"double"}.
}

\section{Methods for \code{as.vector()}}{
  Writers of methods for \code{as.vector} need to take care to
  follow the conventions of the default method.  In particular
  \itemize{
    \item Argument \code{mode} can be \code{"any"}, any of the atomic
    modes, \code{"list"}, \code{"expression"}, \code{"symbol"},
    \code{"pairlist"} or one of the aliases \code{"double"} and \code{"name"}.

    \item The return value should be of the appropriate mode.  For
    \code{mode = "any"} this means an atomic vector or list.

    \item Attributes should be treated appropriately: in particular when
    the result is an atomic vector there should be no attributes, not
    even names.

    \item \code{is.vector(as.vector(x, m), m)} should be true for any
    mode \code{m}, including the default \code{"any"}.
  }
}

\note{
  \code{as.vector} and \code{is.vector} are quite distinct from the
  meaning of the formal class \code{"vector"} in the \pkg{methods}
  package, and hence \code{\link{as}(x, "vector")} and
  \code{\link{is}(x, "vector")}.

  Note that \code{as.vector(x)} is not necessarily a null operation if
  \code{is.vector(x)} is true: any names will be removed from an atomic
  vector.

  Non-vector \code{mode}s \code{"symbol"} (synonym \code{"name"}) and
  \code{"pairlist"} are accepted but have long been undocumented: they
  are used to implement \code{\link{as.name}} and
  \code{\link{as.pairlist}}, and those functions should preferably be
  used directly.  None of the description here applies to those
  \code{mode}s: see the help for the preferred forms.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{c}}, \code{\link{is.numeric}}, \code{\link{is.list}}, etc.
}
\examples{
df <- data.frame(x = 1:3, y = 5:7)
## Error:
try(as.vector(data.frame(x = 1:3, y = 5:7), mode = "numeric"))

x <- c(a = 1, b = 2)
is.vector(x)
as.vector(x)
all.equal(x, as.vector(x)) ## FALSE


###-- All the following are TRUE:
is.list(df)
! is.vector(df)
! is.vector(df, mode = "list")

is.vector(list(), mode = "list")
}
\keyword{classes}
