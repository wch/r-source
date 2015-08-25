% File src/library/base/man/factor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

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
\alias{Summary.ordered}
\alias{addNA}
\concept{categorical variable}
\concept{enumerated type}
\concept{category}
\description{
  The function \code{factor} is used to encode a vector as a factor (the
  terms \sQuote{category} and \sQuote{enumerated type} are also used for
  factors).  If argument \code{ordered} is \code{TRUE}, the factor
  levels are assumed to be ordered.  For compatibility with S there is
  also a function \code{ordered}.

  \code{is.factor}, \code{is.ordered}, \code{as.factor} and \code{as.ordered}
  are the membership and coercion functions for these classes.
}
\usage{
factor(x = character(), levels, labels = levels,
       exclude = NA, ordered = is.ordered(x), nmax = NA)

ordered(x, \dots)

is.factor(x)
is.ordered(x)

as.factor(x)
as.ordered(x)

addNA(x, ifany = FALSE)
}
\arguments{
  \item{x}{a vector of data, usually taking a small number of distinct
    values.}
  \item{levels}{an optional vector of the values (as character strings)
    that \code{x} might have taken.  The default is the unique set of
    values taken by \code{\link{as.character}(x)}, sorted into
    increasing order \emph{of \code{x}}.  Note that this set can be
    specified as smaller than \code{sort(unique(x))}.}
  \item{labels}{\emph{either} an optional character vector of labels for the
    levels (in the same order as \code{levels} after removing those in
    \code{exclude}), \emph{or} a character string of length 1.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels.  This should be of the same type as \code{x}, and
    will be coerced if necessary.}
  \item{ordered}{logical flag to determine if the levels should be regarded
    as ordered (in the order given).}
  \item{nmax}{an upper bound on the number of levels; see \sQuote{Details}.}
  \item{\dots}{(in \code{ordered(.)}): any of the above, apart from
    \code{ordered} itself.}
  \item{ifany}{(only add an \code{NA} level if it is used, i.e.
    if \code{any(is.na(x))}.}
}
\value{
  \code{factor} returns an object of class \code{"factor"} which has a
  set of integer codes the length of \code{x} with a \code{"levels"}
  attribute of mode \code{\link{character}} and unique
  (\code{!\link{anyDuplicated}(.)}) entries.  If argument \code{ordered}
  is true (or \code{ordered()} is used) the result has class
  \code{c("ordered", "factor")}.

  Applying \code{factor} to an ordered or unordered factor returns a
  factor (of the same type) with just the levels which occur: see also
  \code{\link{[.factor}} for a more transparent way to achieve this.

  \code{is.factor} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of type factor or not.  Correspondingly,
  \code{is.ordered} returns \code{TRUE} when its argument is an ordered
  factor and \code{FALSE} otherwise.

  \code{as.factor} coerces its argument to a factor.
  It is an abbreviated form of \code{factor}.

  \code{as.ordered(x)} returns \code{x} if this is ordered, and
  \code{ordered(x)} otherwise.

  \code{addNA} modifies a factor by turning \code{NA} into an extra
  level (so that \code{NA} values are counted in tables, for instance).
}
\details{
  The type of the vector \code{x} is not restricted; it only must have
  an \code{\link{as.character}} method and be sortable (by
  \code{\link{sort.list}}).

  Ordered factors differ from factors only in their class, but methods
  and the model-fitting functions treat the two classes quite differently.

  The encoding of the vector happens as follows.  First all the values
  in \code{exclude} are removed from \code{levels}. If \code{x[i]}
  equals \code{levels[j]}, then the \code{i}-th element of the result is
  \code{j}.  If no match is found for \code{x[i]} in \code{levels}
  (which will happen for excluded values) then the \code{i}-th element
  of the result is set to \code{\link{NA}}.

  Normally the \sQuote{levels} used as an attribute of the result are
  the reduced set of levels after removing those in \code{exclude}, but
  this can be altered by supplying \code{labels}.  This should either
  be a set of new labels for the levels, or a character string, in
  which case the levels are that character string with a sequence
  number appended.

  \code{factor(x, exclude = NULL)} applied to a factor is a no-operation
  unless there are unused levels: in that case, a factor with the
  reduced level set is returned.  If \code{exclude} is used it should
  also be a factor with the same level set as \code{x} or a set of codes
  for the levels to be excluded.

  The codes of a factor may contain \code{\link{NA}}.  For a numeric
  \code{x}, set \code{exclude = NULL} to make \code{\link{NA}} an extra
  level (prints as \code{<NA>}); by default, this is the last level.

  If \code{NA} is a level, the way to set a code to be missing (as
  opposed to the code of the missing level) is to
  use \code{\link{is.na}} on the left-hand-side of an assignment (as in
  \code{is.na(f)[i] <- TRUE}; indexing inside \code{is.na} does not work).
  Under those circumstances missing values are currently printed as
  \code{<NA>}, i.e., identical to entries of level \code{NA}.

  \code{is.factor} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.

  Where \code{levels} is not supplied, \code{\link{unique}} is called.
  Since factors typically have quite a small number of levels, for large
  vectors \code{x} it is helpful to supply \code{nmax} as an upper bound
  on the number of unique values.
}
\section{Warning}{
  The interpretation of a factor depends on both the codes and the
  \code{"levels"} attribute.  Be careful only to compare factors with
  the same set of levels (in the same order).  In particular,
  \code{as.numeric} applied to a factor is meaningless, and may
  happen by implicit coercion.  To transform a factor \code{f} to
  approximately its original numeric values,
  \code{as.numeric(levels(f))[f]} is recommended and slightly more
  efficient than \code{as.numeric(as.character(f))}.

  The levels of a factor are by default sorted, but the sort order
  may well depend on the locale at the time of creation, and should
  not be assumed to be ASCII.

  There are some anomalies associated with factors that have
  \code{NA} as a level.  It is suggested to use them sparingly, e.g.,
  only for tabulation purposes.
}
%% Is this still true, after Ops.factor (==, !=) is fixed ?

\section{Comparison operators and group generic methods}{
  There are \code{"factor"} and \code{"ordered"} methods for the
  \link{group generic} \code{\link[=S3groupGeneric]{Ops}} which
  provide methods for the \link{Comparison} operators,
  and for the \code{\link{min}}, \code{\link{max}}, and
  \code{\link{range}} generics in \code{\link[=S3groupGeneric]{Summary}}
  of \code{"ordered"}.  (The rest of the groups and the
  \code{\link[=S3groupGeneric]{Math}} group generate an error as they
  are not meaningful for factors.)

  Only \code{==} and \code{!=} can be used for factors: a factor can
  only be compared to another factor with an identical set of levels
  (not necessarily in the same ordering) or to a character vector.
  Ordered factors are compared in the same way, but the general dispatch
  mechanism precludes comparing ordered and unordered factors.

  All the comparison operators are available for ordered factors.
  Collation is done by the levels of the operands: if both operands are
  ordered factors they must have the same level set.
}
\note{
  In earlier versions of \R, storing character data as a factor was more
  space efficient if there is even a small proportion of
  repeats.  However, identical character strings now share storage, so
  the difference is small in most cases.  (Integer values are stored
  in 4 bytes whereas each reference to a character string needs a
  pointer of 4 or 8 bytes.)
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{[.factor}} for subsetting of factors.

  \code{\link{gl}} for construction of balanced factors and
  \code{\link{C}} for factors with specified contrasts.
  \code{\link{levels}} and \code{\link{nlevels}} for accessing the
  levels, and \code{\link{unclass}} to get integer codes.
}
\examples{
(ff <- factor(substring("statistics", 1:10, 1:10), levels = letters))
as.integer(ff)      # the internal codes
(f. <- factor(ff))  # drops the levels that do not occur
ff[, drop = TRUE]   # the same, more transparently

factor(letters[1:20], labels = "letter")

class(ordered(4:1)) # "ordered", inheriting from "factor"
z <- factor(LETTERS[3:1], ordered = TRUE)
## and "relational" methods work:
stopifnot(sort(z)[c(1,3)] == range(z), min(z) < max(z))
\dontshow{
of <- ordered(ff)
stopifnot(identical(range(of, rev(of)), of[3:2]),
	  identical(max(of), of[2]))
}

## suppose you want "NA" as a level, and to allow missing values.
(x <- factor(c(1, 2, NA), exclude = NULL))
is.na(x)[2] <- TRUE
x  # [1] 1    <NA> <NA>
is.na(x)
# [1] FALSE  TRUE FALSE

## Using addNA()
Month <- airquality$Month
table(addNA(Month))
table(addNA(Month, ifany = TRUE))
}
\keyword{category}
\keyword{NA}
