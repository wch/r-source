\name{Extract}
\title{Extract or Replace Parts of an Object}
\alias{Extract}
\alias{Subscript}
\alias{[}
\alias{[[}
\alias{$}
\alias{[<-}
\alias{[[<-}
\alias{$<-}
%- methods
\alias{[.data.frame}
\alias{[[.data.frame}
\alias{[<-.data.frame}
\alias{[[<-.data.frame}
\alias{[.formula}
\alias{[.ts}
\alias{[.noquote}
\alias{.subset}
\alias{.subset2}
\description{
  Operators act on vectors, arrays, data frames and lists to extract or
  replace subsets.
}
\usage{
x[i]
x[i, j, \dots]
x[i, j, \dots , drop=TRUE]
x[[i]]
x[[i, j, \dots]]
x$name
.subset(x, \dots)
.subset2(x, \dots)
}
\arguments{
  \item{x}{object from which to extract elements}
  \item{i,j,\dots,name}{elements to extract or replace. \code{i,j} are
    either \code{numeric} or \code{character} where \code{name} must be
    character or an (unquoted) name.  Numeric values are coerced to
    integer as by \code{\link{as.integer}(i)}.}
  \item{drop}{For data frames, matrices, and arrays.  If \code{TRUE} the
    result is coerced to the lowest possible dimension (see examples
    below).}
}
\details{
  If one of these expressions appears on the left side of an assignment
  then that part of \code{x} is set to the value of the right hand side
  of the assignment.

  These operators are generic. You can write methods to handle subsetting
  of specific classes of objects, see \link{InternalMethods}.

  The \code{[[} operator requires all relevant subscripts to be supplied.
  With the \code{[} operator a comma separated blank indicates that all
  entries in that dimension are selected.

  Indexing by factors is allowed and is equivalent to indexing by the
  numeric codes (see \code{\link{factor}}) and not by the character
  values which are printed (for which use \code{[as.character(i)]}).

  When \code{[.data.frame} is used for subsetting rows of a
  \code{\link{data.frame}}, it returns a data frame with unique
  (and non-missing)row names, if necessary transforming the names using
  \code{\link{make.names}( * , unique = TRUE)}. See the \code{swiss}
  example below.

  When operating on a list, the \code{[[} operator gives the specified
  element of the list while the \code{[} operator returns a list with
  the specified element(s) in it.

  As from \R 1.7.0 \code{[[} can be applied recursively to lists, so
  that if the single index \code{i} is a vector of length \code{p},
  \code{alist[[i]]} is equivalent to \code{alist[[i1]]\dots[[ip]]}
  providing all but the final indexing results in a list.

  The operators \code{$} and \code{$<-} do not evaluate their second
  argument.  It is translated to a string and that string is used to
  locate the correct component of the first argument.

  The functions \code{.subset} and \code{.subset2} are essentially
  equivalent to the \code{[} and \code{[[} operators, except that
  methods dispatch does not take place.  This is to avoid expensive
  unclassing in order to apply the default method to an object.  They
  should not normally be invoked by end users.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{list}}, \code{\link{array}}, \code{\link{matrix}}.
  \code{\link{factor}} for the behaviour of \code{[} applied to factors.

  \code{\link{Syntax}} for operator precedence, and the
  \emph{R Language} reference manual about indexing details.
%% Fixme: Link (to html in `help.start()', pdf from `ref manual', `info' from ESS
%%  , see \url{http://cran.R-project.org/manuals.html}.
}
\examples{
x <- 1:12; m <- matrix(1:6,nr=2); li <- list(pi=pi, e = exp(1))
x[10]                 # the tenth element of x
m[1,]                 # the first row of matrix m
m[1, , drop = FALSE]  # is a 1-row matrix
li[[1]]               # the first element of list li
y <- list(1,2,a=4,5)
y[c(3,4)]             # a list containing elements 3 and 4 of y
y$a                   # the element of y named a

data(swiss)
swiss[ c(1, 1:2), ]   # duplicate row, unique row names

## non-integer indices are truncated:
(i <- 3.999999999) # "4" is printed
(1:5)[i]  # 3

## recursive indexing into lists
z <- list( a=list( b=9, c='hello'), d=1:5)
unlist(z)
z[[c(1, 2)]]
z[[c(1, 2, 1)]]  # both "hello"
z[[c("a", "b")]] <- "new"
unlist(z)
}
\keyword{array}
\keyword{list}
