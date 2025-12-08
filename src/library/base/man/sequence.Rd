% File src/library/base/man/sequence.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{sequence}
\title{Create A Vector of Sequences}
\usage{
sequence(nvec, \dots)
\method{sequence}{default}(nvec, from = 1L, by = 1L,
    recycle = Sys.getenv("R_sequence_recycle", "false"), \dots)
}
\alias{sequence}
\alias{sequence.default}
\arguments{
  \item{nvec}{coerced to a non-negative integer vector each element of which
    specifies the length of a sequence.}
  \item{from}{coerced to an integer vector each element of which
    specifies the first element of a sequence.}
  \item{by}{coerced to an integer vector each element of which
    specifies the step size between elements of a sequence.}
  \item{recycle}{\code{\link{logical}} or coercible to it, indicating if
    \code{nvec} is recycled, as it has always been documented to.
    We recommend to set this to \code{TRUE} explicitly, as that will become
    default, see \sQuote{Details}.}% also \PR{18304} for background
  \item{\dots}{additional arguments passed to methods.}
}
\description{
  The default method for \code{sequence} generates the sequence
  \code{\link{seq}(from[i], by = by[i], length.out = nvec[i])} for each
  element \code{i} in the parallel (and recycled) vectors \code{from},
  \code{by} and \code{nvec}.
  Note that \code{nvec} is \emph{not} recycled unless \code{recycle}
  is true, see \sQuote{Details}.
  It then returns the result of concatenating those sequences.
}
\details{
  Negative values are supported for \code{from} and
  \code{by}. \code{sequence(nvec, from, by=0L)} is equivalent to
  \code{rep(from, each=nvec)}.

  This function was originally implemented in \R with fewer features, but
  it has since become more flexible, and the default method is
  implemented in C for speed.

  Argument \code{recycle} is new since \R 4.6.0;
  currently, the default is \code{FALSE} unless the environment variable
  \env{R_sequence_recycle} is set to a true value.  \cr
  This provides back compatibility with \R <= 4.5.z, where \code{from} and
  \code{by} are recycled or shortened to length \code{length(nvec)} in case
  that is shorter than the maximal length.  \cr
  The plan is to replace the environment variable with an option
  (\code{\link{getOption}}) defaulting to \code{TRUE} and later to
  \code{TRUE} without a global option, to use \R's usual recycling semantic
  for all three arguments \code{nvec, from, by}.
}
\seealso{
  \code{\link{gl}}, \code{\link{seq}}, \code{\link{rep}}.
}
\author{Of the current version, Michael Lawrence based on code from the
  \pkg{S4Vectors} Bioconductor package}
\examples{
sequence(c(3, 2)) # the concatenated sequences 1:3 and 1:2.
#> [1] 1 2 3 ' 1 2  (using ' to visualize the sub-sequences)
sequence(c(3, 2), from=2L)         #> [1] 2 3 4 ' 2 3
sequence(c(3, 2), from=2L, by=2L)  #> [1] 2 4 6 ' 2 4
sequence(c(3, 2), by=c(-1L, 1L))   #> [1] 1 0 -1 ' 1 2

## Cases where  'recycle'  makes a difference:
sequence(3, 1:3) #>  1 2 3  (_currently_ -- will change in future!)
sequence(3, 1:3, recycle = FALSE)# back compatible: 1 2 3
sequence(3, 1:3, recycle = TRUE) # future default -- use in new code!
                                    # --> 1 2 3 ' 2 3 4 ' 3 4 5
try(sequence(3, 1:3, 1[0], recycle = FALSE)) # an Error; default will be "empty":
    sequence(3, 1:3, 1[0], recycle = TRUE) # |--> integer(0)
}
\keyword{manip}
