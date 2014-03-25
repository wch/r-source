% File src/library/base/man/NA.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{NA}
\alias{NA}
\alias{NA_integer_}
\alias{NA_real_}
\alias{NA_complex_}
\alias{NA_character_}
\alias{is.na}
\alias{is.na.data.frame}
\alias{is.na<-}
\alias{is.na<-.default}
\alias{anyNA}
\alias{anyMissing}% an alternative name (in Biobase and S+ as of ~2006)
\title{\sQuote{Not Available} / Missing Values}
\description{
  \code{NA} is a logical constant of length 1 which contains a missing
  value indicator.  \code{NA} can be coerced to any other vector
  type except raw.  There are also constants \code{NA_integer_},
  \code{NA_real_}, \code{NA_complex_} and \code{NA_character_} of the
  other atomic vector types which support missing values: all of these
  are \link{reserved} words in the \R language.

  The generic function \code{is.na} indicates which elements are missing.

  The generic function \code{is.na<-} sets elements to \code{NA}.

  The generic function \code{anyNA} implements \code{any(is.na(x))} in a
  possibly faster way (especially for atomic vectors).
}
\usage{
NA
is.na(x)
anyNA(x)

\method{is.na}{data.frame}(x)

is.na(x) <- value
}
\arguments{
  \item{x}{an \R object to be tested: the default method for
    \code{is.na} handles atomic vectors, lists and pairlists.}
  \item{recursive}{logical: should \code{anyNA} be applied recursively
    to lists and pairlists.}
  \item{value}{a suitable index vector for use with \code{x}.}
}
%% Next par on character NAs taken from R-lang.texi:
\details{
  The \code{NA} of character type is distinct from the
  string \code{"NA"}.  Programmers who need to specify an explicit
  string \code{NA} should use \code{NA_character_} rather than
  \code{"NA"}, or set elements to \code{NA} using \code{is.na<-}.

  \code{is.na(x)} works elementwise when \code{x} is a
  \code{\link{list}}.  It and \code{anyNA} are generic: you can write
  methods to handle specific classes of objects, see
  \link{InternalMethods}.  A complex value is regarded as \code{NA} if
  either its real or imaginary part is \code{NA} or \code{NaN}.

  Function \code{is.na<-} may provide a safer way to set missingness.
  It behaves differently for factors, for example.

  Numerical computations using \code{NA} will normally result in \code{NA}: a
  possible exception is where \code{\link{NaN}} is also involved, in
  which case either might result.  Logical computations treat \code{NA}
  as a missing \code{TRUE/FALSE} value, and so may return \code{TRUE} or 
  \code{FALSE} if the expression does not depend on the \code{NA} operand.
  
  The default method for \code{anyNA} handles atomic vectors (without a
  class) and \code{NULL}.  It calls \code{any(is.na(x)} on objects with
  classes and on lists and pairlists.
}
\value{
  The default method for \code{is.na} applied to an atomic vector
  returns a logical vector of the same length as its argument \code{x},
  containing \code{TRUE} for those elements marked \code{NA} or, for
  numeric or complex vectors, \code{\link{NaN}} (!) and \code{FALSE}
  otherwise.  \code{dim}, \code{dimnames} and \code{names} attributes
  are preserved.

  The default methods also work for lists and pairlists:\cr
  For \code{is.na}, elementwise the result is false unless that element
  is a length-one atomic vector and the single element of that vector is
  regarded as \code{NA} or \code{NaN} (note that any \code{is.na}
  method for the class of the element is ignored).\cr
  \code{anyNA(recursive = FALSE)} works the same way as \code{is.na};
  \code{anyNA(recursive = TRUE)} apply \code{anyNA} (with method
  dispatch) to each element.

  The data frame method for \code{is.na} returns a logical matrix
  with the same dimensions as the data frame, and with dimnames taken
  from the row and column names of the data frame.
  
  \code{anyNA(NULL)} is false: \code{is.na(NULL)} is \code{logical(0)},
  with a warning.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{NaN}}, \code{\link{is.nan}}, etc.,
  and the utility function \code{\link{complete.cases}}.

  \code{\link{na.action}}, \code{\link{na.omit}}, \code{\link{na.fail}}
  on how methods can be tuned to deal with missing values.
}
\examples{
is.na(c(1, NA))        #> FALSE  TRUE
is.na(paste(c(1, NA))) #> FALSE FALSE

(xx <- c(0:4))
is.na(xx) <- c(2, 4)
xx                     #> 0 NA  2 NA  4
anyNA(xx) # TRUE

# Some logical operations do not return NA
c(TRUE, FALSE) & NA
c(TRUE, FALSE) | NA

\donttest{
## Measure speed difference in a favourable case:
## the difference depends on the platform, on Linux ca 3x.
x <- 1:10000; x[5000] <- NaN  # coerces x to be double
if(require("microbenchmark")) { # does not work reliably on all platforms
  print(microbenchmark(any(is.na(x)), anyNA(x)))
} else {
  nSim <- 2^13
  print(rbind(is.na = system.time(replicate(nSim, any(is.na(x)))),
              anyNA = system.time(replicate(nSim, anyNA(x)))))
}
}

## anyNA() can work recursively with list()s:
LL <- list(1:5, c(NA, 5:8), c("A","NA"), c("a", NA_character_))
L2 <- LL[c(1,3)]
sapply(LL, anyNA); c(anyNA(LL), anyNA(LL, TRUE))
sapply(L2, anyNA); c(anyNA(L2), anyNA(L2, TRUE))

## ... lists, and hence data frames, too:
dN <- dd <- USJudgeRatings; dN[3,6] <- NA
anyNA(dd) # FALSE
anyNA(dN) # TRUE
}
\keyword{NA}
\keyword{logic}
\keyword{manip}
