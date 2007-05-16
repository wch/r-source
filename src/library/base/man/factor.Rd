\name{factor}
\title{Factors}
\alias{factor}
\alias{ordered}
\alias{is.factor}
\alias{is.ordered}
\alias{as.factor}
\alias{as.ordered}
\alias{is.na<-.factor}
\alias{Math.factor}
\alias{Ops.factor}
\alias{Summary.factor}
\alias{Ops.ordered}
\concept{categorical variable}
\concept{enumerated type}
\concept{category}
\description{
  The function \code{factor} is used to encode a vector as a factor (the
  terms \sQuote{category} and \sQuote{enumerated type} are also used for
  factors).  If \code{ordered} is \code{TRUE}, the factor levels are
  assumed to be ordered.
  For compatibility with S there is also a function \code{ordered}.

  \code{is.factor}, \code{is.ordered}, \code{as.factor} and \code{as.ordered}
  are the membership and coercion functions for these classes.
}
\usage{
factor(x = character(), levels = sort(unique.default(x), na.last = TRUE),
       labels = levels, exclude = NA, ordered = is.ordered(x))
ordered(x, \dots)

is.factor(x)
is.ordered(x)

as.factor(x)
as.ordered(x)
}
\arguments{
  \item{x}{a vector of data, usually taking a small number of distinct
    values.}
  \item{levels}{an optional vector of the values that \code{x} might
    have taken. The default is the set of values taken by \code{x},
    sorted into increasing order.}
  \item{labels}{\emph{either} an optional vector of labels for the
    levels (in the same order as \code{levels} after removing those in
    \code{exclude}), \emph{or} a character string of length 1.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels. This should be of the same type as \code{x}, and
    will be coerced if necessary.}
  \item{ordered}{logical flag to determine if the levels should be regarded
    as ordered (in the order given).}
  \item{\dots}{(in \code{ordered(.)}): any of the above, apart from
    \code{ordered} itself.}
}
\value{
  \code{factor} returns an object of class \code{"factor"} which has a
  set of integer codes the length of \code{x} with a \code{"levels"}
  attribute of mode \code{\link{character}}.  If \code{ordered} is true
  (or \code{ordered} is used) the result has class
  \code{c("ordered", "factor")}.

  Applying \code{factor} to an ordered or unordered factor returns a
  factor (of the same type) with just the levels which occur: see also
  \code{\link{[.factor}} for a more transparent way to achieve this.

  \code{is.factor} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of type factor or not.  Correspondingly,
  \code{is.ordered} returns \code{TRUE} when its
  argument is ordered and \code{FALSE} otherwise.

  \code{as.factor} coerces its argument to a factor.
  It is an abbreviated form of \code{factor}.

  \code{as.ordered(x)} returns \code{x} if this is ordered, and
  \code{ordered(x)} otherwise.
}
\details{
  The type of the vector \code{x} is not restricted.

  Ordered factors differ from factors only in their class, but methods
  and the model-fitting functions treat the two classes quite differently.

  The encoding of the vector happens as follows. First all the values
  in \code{exclude} are removed from \code{levels}. If \code{x[i]} equals
  \code{levels[j]}, then the \code{i}-th element of the result is
  \code{j}.  If no match is found for \code{x[i]} in \code{levels},
  then the \code{i}-th element of the result is set to \code{\link{NA}}.

  Normally the \sQuote{levels} used as an attribute of the result are
  the reduced set of levels after removing those in \code{exclude}, but
  this can be altered by supplying \code{labels}. This should either
  be a set of new labels for the levels, or a character string, in
  which case the levels are that character string with a sequence
  number appended.

  \code{factor(x, exclude=NULL)} applied to a factor is a no-operation
  unless there are unused levels: in that case, a factor with the
  reduced level set is returned.  If \code{exclude} is used it should
  also be a factor with the same level set as \code{x} or a set of codes
  for the levels to be excluded.

  The codes of a factor may contain \code{\link{NA}}. For a numeric
  \code{x}, set \code{exclude=NULL} to make \code{\link{NA}} an extra
  level (\code{"NA"}), by default the last level.

  If \code{"NA"} is a level, the way to set a code to be missing is to
  use \code{\link{is.na}} on the left-hand-side of an assignment.
  Under those circumstances missing values are printed as \code{<NA>}.

  \code{is.factor} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\section{Warning}{
  The interpretation of a factor depends on both the codes and the
  \code{"levels"} attribute.  Be careful only to compare factors with
  the same set of levels (in the same order).  In particular,
  \code{as.numeric} applied to a factor is meaningless, and may
  happen by implicit coercion.	To \dQuote{revert} a factor \code{f} to
  its original numeric values, \code{as.numeric(levels(f))[f]} is
  recommended and slightly more efficient than
  \code{as.numeric(as.character(f))}.

  The levels of a factor are by default sorted, but the sort order
  may well depend on the locale at the time of creation, and should
  not be assumed to be ASCII.
}
\section{Comparison operators and group generic methods}{
  There are \code{"factor"} and \code{"ordered"} methods for the
  \link{group generic} \code{\link[base:groupGeneric]{Ops}}, which
  provide methods for the \link{Comparison} operators.  (The rest of the
  group and the \code{\link[base:groupGeneric]{Math}} and
  \code{\link[base:groupGeneric]{Summary}} groups generate an error as
  they are not meaningful for factors.)

  Only \code{==} and \code{!=} can be used for factors: a factor can
  only be compared to another factor with an identical set of levels
  (not necessarily in the same ordering) or to a character vector.
  Ordered factors are compared in the same way, but the general dispatch
  mechanism precludes comparing ordered and unordered factors.
  
  All the comparison operators are available for ordered factors.
  Sorting is done by the levels of the operands: if both operands are
  ordered factors they must have the same level set.
}
\note{
  Storing character data as a factor is more efficient storage if
  there is even a small proportion of repeats.  On a 32-bit machine
  storing a string of \eqn{n} bytes takes
  \eqn{28 + 8\lceil(n+1)/8\rceil}{28 + 8*ceiling((n+1)/8)}
  bytes whereas storing a factor code takes 4 bytes.  (On a 64-bit
  machine 28 is replaced by 56 or more.)  Only if they were computed
  from the same values (or in some cases read from a file: see
  \code{\link{scan}}) will identical strings share storage.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{[.factor}} for subsetting of factors.

  \code{\link{gl}} for construction of \dQuote{balanced} factors and
  \code{\link{C}} for factors with specified contrasts.
  \code{\link{levels}} and \code{\link{nlevels}} for accessing the
  levels, and \code{\link{unclass}} to get integer codes.
}
\examples{
(ff <- factor(substring("statistics", 1:10, 1:10), levels=letters))
as.integer(ff)  # the internal codes
factor(ff)	# drops the levels that do not occur
ff[, drop=TRUE] # the same, more transparently

factor(letters[1:20], labels="letter")

class(ordered(4:1)) # "ordered", inheriting from "factor"

## suppose you want "NA" as a level, and to allowing missing values.
(x <- factor(c(1, 2, "NA"), exclude = ""))
is.na(x)[2] <- TRUE
x  # [1] 1    <NA> NA, <NA> used because NA is a level.
is.na(x)
# [1] FALSE  TRUE FALSE
factor()
}
\keyword{category}
\keyword{NA}
