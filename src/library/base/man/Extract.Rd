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
\concept{delete}
\description{
  Operators acting on vectors, arrays and lists to extract or
  replace subsets.
}
\usage{
x[i]
x[i, j, \dots , drop = TRUE]
x[[i]]
x[[i, j, \dots]]
x$name
}
\arguments{
  \item{x}{
    object from which to extract elements or in which to replace elements.
  }
  \item{i, j, \dots, name}{
    elements to extract or replace. \code{i, j} are
    \code{numeric} or \code{character} or empty whereas \code{name} must be
    character or an (unquoted) name.  Numeric values are coerced to
    integer as by \code{\link{as.integer}}.  Character strings are
    partially matched to the (dim)names of the object if exact matching
    does not succeed.
    
    For \code{[}-indexing only: \code{i, j, \dots} can be
    logical vectors, indicating elements/slices to select.  Such vectors
    are recycled if necessary to match the corresponding extent.  When
    indexing arrays, \code{i} can be a (single) matrix with as many
    columns as there are dimensions of \code{x}; the result is then a
    vector with elements corresponding to the sets of indices in each
    row of \code{i}.
    
    \code{i, j, \dots} can also be negative integers, indicating 
    elements/slices to leave out of the selection.
    }
  \item{drop}{For matrices, and arrays.  If \code{TRUE} the
    result is coerced to the lowest possible dimension (see examples
    below). This only works for extracting elements, not for the
    replacement forms.}
}
\details{
  These operators are generic.  You can write methods to handle subsetting
  of specific classes of objects, see \link{InternalMethods} as well as
  \code{\link{[.data.frame}} and \code{\link{[.factor}}.  The
  descriptions here apply only to the default methods.

  The most important distinction between \code{[}, \code{[[} and
  \code{$} is that the \code{[} can select more than one element whereas
  the other two select a single element.  \code{$} does not allow
  computed indices, whereas \code{[[} does.  \code{x$name} is equivalent
  to \code{x[["name"]]} if \code{x} is recursive
  (see \code{\link{is.recursive}}) and \code{NULL} otherwise.
  
  The \code{[[} operator requires all relevant subscripts to be supplied.
  With the \code{[} operator an empty index (a comma separated blank)
  indicates that all entries in that dimension are selected.

  If one of these expressions appears on the left side of an assignment
  then that part of \code{x} is set to the value of the right hand side
  of the assignment.

  Indexing by factors is allowed and is equivalent to indexing by the
  numeric codes (see \code{\link{factor}}) and not by the character
  values which are printed (for which use \code{[as.character(i)]}).

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

  When \code{$<-} is applied to a \code{NULL} \code{x}, it coerces
  \code{x} to \code{list()}.  This is what happens with \code{[[<-} is
  \code{y} is of length greater than one: if \code{y} has length 1 or 0,
  \code{x} is coerced to a zero-length vector of the type of \code{value}, 

  As from \R 1.9.0 both \code{$} and \code{[[} can be applied to
  environments.  Only character arguments are allowed and no partial
  matching is done (this is in contrast to the behavior for lists). The
  semantics of these operations is basically that of \code{get(i, env=x,
    inherits=FALSE)}.  If no match is found then \code{NULL} is
  returned.  The assignment versions, \code{$<-} and
  \code{[[<-}, can also be used.  Again, only character arguments are
  allowed and no partial matching is done.  The semantics in this case
  are those of \code{assign(i, value, env=x, inherits=FALSE)}.  Such an
  assignment will either create a new binding or change the existing
  binding in \code{x}.
}
\section{NAs in indexing}{
  When subscripting, a numerical, logical or character \code{NA} picks
  an unknown element and so returns \code{NA} in the corresponding
  element of a logical, integer, numeric, complex or character result,
  and \code{NULL} for a list.

  When replacing (that is using subscripting on the lhs of an
  assignment) \code{NA} does not select any element to be replaced.  As
  there is ambiguity as to whether an element of the rhs should
  be used or not (and \R handled this inconsistently prior to \R 2.0.0),
  this is only allowed if the rhs value is of length one (so the two
  interpretations would have the same outcome).  
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{list}}, \code{\link{array}}, \code{\link{matrix}}.

  \code{\link{[.data.frame}} and \code{\link{[.factor}} for the
  behaviour when applied to data.frame and factors.

  \code{\link{Syntax}} for operator precedence, and the
  \emph{R Language} reference manual about indexing details.
  %% Fixme: Link (to html in 'help.start()', pdf from 'ref manual',
  %% 'info' from ESS, see \url{http://cran.R-project.org/manuals.html}.
}
\examples{
x <- 1:12; m <- matrix(1:6,nr=2); li <- list(pi=pi, e = exp(1))
x[10]                 # the tenth element of x
x <- x[-1]	      # delete the 1st element of x
m[1,]                 # the first row of matrix m
m[1, , drop = FALSE]  # is a 1-row matrix
m[,c(TRUE,FALSE,TRUE)]# logical indexing
m[cbind(c(1,2,1),3:1)]# matrix index
m <- m[,-1]	      # delete the first column of m
li[[1]]               # the first element of list li
y <- list(1,2,a=4,5)
y[c(3,4)]             # a list containing elements 3 and 4 of y
y$a                   # the element of y named a

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

## check $ and [[ for environments
e1 <- new.env()
e1$a <- 10
e1[["a"]]
e1[["b"]] <- 20
e1$b
ls(e1)
}
\keyword{array}
\keyword{list}
