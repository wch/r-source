% File src/library/base/man/Extract.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Copyright 2002-2012 The R Foundation
% Distributed under GPL 2 or later

\name{Extract}
\title{Extract or Replace Parts of an Object}
\alias{Extract}
\alias{Subscript}
\alias{[}
\alias{[.listof}
\alias{[.simple.list}
\alias{[[}
\alias{$}
\alias{getElement}
\alias{[<-}
\alias{[[<-}
\alias{$<-}
\concept{delete}

\description{
  Operators acting on vectors, matrices, arrays and lists to extract or
  replace parts.
}

\usage{
x[i]
x[i, j, \dots , drop = TRUE]
x[[i, exact = TRUE]]
x[[i, j, \dots, exact = TRUE]]
x$name
getElement(object, name)

x[i] <- value
x[i, j, \dots] <- value
x[[i]] <- value
x$i <- value
}

\arguments{
  \item{x, object}{
    object from which to extract element(s) or in which to replace element(s).
  }
  \item{i, j, \dots}{
    indices specifying elements to extract or replace.  Indices are
    \code{numeric} or \code{character} vectors or empty (missing) or
    \code{NULL}.  Numeric values are coerced to integer as by
    \code{\link{as.integer}} (and hence truncated towards zero).
    Character vectors will be matched to the \code{\link{names}} of the
    object (or for matrices/arrays, the \code{\link{dimnames}}):
    see \sQuote{Character indices} below for further details.

    For \code{[}-indexing only: \code{i}, \code{j}, \code{\dots} can be
    logical vectors, indicating elements/slices to select.  Such vectors
    are recycled if necessary to match the corresponding extent.
    \code{i}, \code{j}, \code{\dots} can also be negative integers,
    indicating elements/slices to leave out of the selection.

    When indexing arrays by \code{[} a single argument \code{i} can be a
    matrix with as many columns as there are dimensions of \code{x}; the
    result is then a vector with elements corresponding to the sets of
    indices in each row of \code{i}.

    An index value of \code{NULL} is treated as if it were \code{integer(0)}.
  }
  \item{name}{
    A literal character string or a \link{name} (possibly \link{backtick}
    quoted).  For extraction, this is normally (see under
    \sQuote{Environments}) partially matched to the \code{\link{names}}
    of the object.
  }
  \item{drop}{For matrices and arrays.  If \code{TRUE} the result is
    coerced to the lowest possible dimension (see the examples).  This
    only works for extracting elements, not for the replacement.  See
    \code{\link{drop}} for further details.
  }
  \item{exact}{Controls possible partial matching of \code{[[} when
    extracting by a character vector (for most objects, but see under
    \sQuote{Environments}).  The default is no partial matching.  Value
    \code{NA} allows partial matching but issues a warning when it
    occurs.  Value \code{FALSE} allows partial matching without any
    warning.}
  \item{value}{typically an array-like \R object of a similar class as
    \code{x}.}
}

\details{
  These operators are generic.  You can write methods to handle indexing
  of specific classes of objects, see \link{InternalMethods} as well as
  \code{\link{[.data.frame}} and \code{\link{[.factor}}.  The
  descriptions here apply only to the default methods.  Note that
  separate methods are required for the replacement functions
  \code{[<-}, \code{[[<-} and \code{$<-} for use when indexing occurs on
  the assignment side of an expression.

  The most important distinction between \code{[}, \code{[[} and
  \code{$} is that the \code{[} can select more than one element whereas
  the other two select a single element.

  The default methods work somewhat differently for atomic vectors,
  matrices/arrays and for recursive (list-like, see
  \code{\link{is.recursive}}) objects.  \code{$} is only valid for
  recursive objects, and is only discussed in the section below on
  recursive objects.

  Subsetting (except by an empty index) will drop all attributes except
  \code{names}, \code{dim} and \code{dimnames}.

  Indexing can occur on the right-hand-side of an expression for
  extraction, or on the left-hand-side for replacement.  When an index
  expression appears on the left side of an assignment (known as
  \emph{subassignment}) then that part of \code{x} is set to the value
  of the right hand side of the assignment.  In this case no partial
  matching of character indices is done, and the left-hand-side is
  coerced as needed to accept the values.  For vectors, the answer will
  be of the higher of the types of \code{x} and \code{value} in the
  hierarchy raw < logical < integer < double < complex < character <
  list < expression.  Attributes are preserved (although \code{names},
  \code{dim} and \code{dimnames} will be adjusted suitably).
  Subassignment is done sequentially, so if an index is specified more
  than once the latest assigned value for an index will result.

  It is an error to apply any of these operators to an object which is
  not subsettable (e.g. a function).
}

\section{Atomic vectors}{
  The usual form of indexing is \code{[}.  \code{[[} can be used to
  select a single element \emph{dropping} \code{\link{names}}, whereas
  \code{[} keeps them, e.g., in \code{c(abc = 123)[1]}.

  The index object \code{i} can be numeric, logical, character or empty.
  Indexing by factors is allowed and is equivalent to indexing by the
  numeric codes (see \code{\link{factor}}) and not by the character
  values which are printed (for which use \code{[as.character(i)]}).

  An empty index selects all values: this is most often used to replace
  all the entries but keep the \code{\link{attributes}}.
}

\section{Matrices and arrays}{
  Matrices and arrays are vectors with a dimension attribute and so all
  the vector forms of indexing can be used with a single index.  The
  result will be an unnamed vector unless \code{x} is one-dimensional
  when it will be a one-dimensional array.

  The most common form of indexing a \eqn{k}-dimensional array is to
  specify \eqn{k} indices to \code{[}.  As for vector indexing, the
  indices can be numeric, logical, character, empty or even factor.
  An empty index (a comma separated blank) indicates that all entries in
  that dimension are selected.
  The argument \code{drop} applies to this form of indexing.

  A third form of indexing is via a numeric matrix with the one column
  for each dimension: each row of the index matrix then selects a single
  element of the array, and the result is a vector.  Negative indices are
  not allowed in the index matrix.  \code{NA} and zero values are allowed:
  rows of an index matrix containing a zero are ignored, whereas rows
  containing an \code{NA} produce an \code{NA} in the result.

  Indexing via a character matrix with one column per dimensions is also
  supported if the array has dimension names.  As with numeric matrix
  indexing, each row of the index matrix selects a single element of the
  array.  Indices are matched against the appropriate dimension names.
  \code{NA} is allowed and will produce an \code{NA} in the result.
  Unmatched indices as well as the empty string (\code{""}) are not
  allowed and will result in an error.

  A vector obtained by matrix indexing will be unnamed unless \code{x}
  is one-dimensional when the row names (if any) will be indexed to
  provide names for the result.
}

\section{Recursive (list-like) objects}{
  Indexing by \code{[} is similar to atomic vectors and selects a list
  of the specified element(s).

  Both \code{[[} and \code{$} select a single element of the list.  The
  main difference is that \code{$} does not allow computed indices,
  whereas \code{[[} does.  \code{x$name} is equivalent to
  \code{x[["name", exact = FALSE]]}.  Also, the partial matching
  behavior of \code{[[} can be controlled using the \code{exact} argument.

  \code{getElement(x, name)} is a version of \code{x[[name, exact = TRUE]]}
  which for formally classed (S4) objects returns \code{\link{slot}(x, name)},
  hence providing access to even more general list-like objects.

  \code{[} and \code{[[} are sometimes applied to other recursive
  objects such as \link{call}s and \link{expression}s.  Pairlists are
  coerced to lists for extraction by \code{[}, but all three operators
  can be used for replacement.

  \code{[[} can be applied recursively to lists, so that if the single
  index \code{i} is a vector of length \code{p}, \code{alist[[i]]} is
  equivalent to \code{alist[[i1]]\dots[[ip]]} providing all but the
  final indexing results in a list.

  Note that in all three kinds of replacement, a value of \code{NULL}
  deletes the corresponding item of the list.  To set entries to
  \code{NULL}, you need \code{x[i] <- list(NULL)}.

  When \code{$<-} is applied to a \code{NULL} \code{x}, it first coerces
  \code{x} to \code{list()}.  This is what also happens with \code{[[<-}
  if the replacement value \code{value} is of length greater than one:
  if \code{value} has length 1 or 0, \code{x} is first coerced to a
  zero-length vector of the type of \code{value}.
}

\section{Environments}{
  Both \code{$} and \code{[[} can be applied to environments.  Only
  character indices are allowed and no partial matching is done.  The
  semantics of these operations are those of \code{get(i, env = x,
    inherits = FALSE)}.  If no match is found then \code{NULL} is
  returned.  The replacement versions, \code{$<-} and \code{[[<-}, can
  also be used.  Again, only character arguments are allowed.  The
  semantics in this case are those of \code{assign(i, value, env = x,
    inherits = FALSE)}.  Such an assignment will either create a new
  binding or change the existing binding in \code{x}.
}

\section{NAs in indexing}{
  When extracting, a numerical, logical or character \code{NA} index picks
  an unknown element and so returns \code{NA} in the corresponding
  element of a logical, integer, numeric, complex or character result,
  and \code{NULL} for a list.  (It returns \code{00} for a raw result.)

  When replacing (that is using indexing on the lhs of an
  assignment) \code{NA} does not select any element to be replaced.  As
  there is ambiguity as to whether an element of the rhs should
  be used or not, this is only allowed if the rhs value is of length one
  (so the two interpretations would have the same outcome).
  (The documented behaviour of S was that an \code{NA} replacement index
  \sQuote{goes nowhere} but uses up an element of \code{value}:
  Becker \emph{et al} p. 359.  However, that has not been true of
  other implementations.)
}

\section{Argument matching}{
  Note that these operations do not match their index arguments in the
  standard way: argument names are ignored and positional matching only is
  used.  So \code{m[j = 2, i = 1]} is equivalent to \code{m[2, 1]} and
  \strong{not} to \code{m[1, 2]}.

  This may not be true for methods defined for them; for example it is
  not true for the \code{data.frame} methods described in
  \code{\link{[.data.frame}} which warn if \code{i} or \code{j}
  is named and have undocumented behaviour in that case.

  To avoid confusion, do not name index arguments (but \code{drop} and
  \code{exact} must be named).
}

\section{S4 methods}{
  These operators are also implicit S4 generics, but as primitives, S4
  methods will be dispatched only on S4 objects \code{x}.

%   not yet implemented
%   These operators are also implicit S4 generics, but the implementation
%   of the primitive functions will only dispatch S4 methods if one of the
%   arguments is an S4 object (it is no longer true that only the first
%   argument is checked).  Method signatures can include any formal
%   argument, but the primitive evaluation will only look for a method if
%   \code{\link{isS4}} would return \code{TRUE} for some argument.

  The implicit generics for the \code{$} and \code{$<-} operators do not
  have \code{name} in their signature because the grammar only allows
  symbols or string constants for the \code{name} argument.
}

\section{Character indices}{
  Character indices can in some circumstances be partially matched (see
  \code{\link{pmatch}}) to the names or dimnames of the object being
  subsetted (but never for subassignment).  Unlike S (Becker \emph{et
  al} p. 358)), \R never uses partial matching when extracting by
  \code{[}, and partial matching is not by default used by \code{[[}
  (see argument \code{exact}).

  Thus the default behaviour is to use partial matching only when
  extracting from recursive objects (except environments) by \code{$}.
  Even in that case, warnings can be switched on by
  \code{\link{options}(warnPartialMatchDollar = TRUE)}.

  Neither empty (\code{""}) nor \code{NA} indices match any names, not
  even empty nor missing names.  If any object has no names or
  appropriate dimnames, they are taken as all \code{""} and so match
  nothing.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}

\seealso{
  \code{\link{names}} for details of matching to names, and
  \code{\link{pmatch}} for partial matching.

  \code{\link{list}}, \code{\link{array}}, \code{\link{matrix}}.

  \code{\link{[.data.frame}} and \code{\link{[.factor}} for the
  behaviour when applied to data.frame and factors.

  \code{\link{Syntax}} for operator precedence, and the
  \sQuote{R Language Definition} manual about indexing details.


  \code{\link{NULL}} for details of indexing null objects.
}
%% Fixme: Link (to html in 'help.start()', pdf from 'ref manual',
%% 'info' from ESS, see \url{http://cran.R-project.org/manuals.html}.

\examples{
x <- 1:12
m <- matrix(1:6, nrow = 2, dimnames = list(c("a", "b"), LETTERS[1:3]))
li <- list(pi = pi, e = exp(1))
x[10]                 # the tenth element of x
x <- x[-1]            # delete the 1st element of x
m[1,]                 # the first row of matrix m
m[1, , drop = FALSE]  # is a 1-row matrix
m[,c(TRUE,FALSE,TRUE)]# logical indexing
m[cbind(c(1,2,1),3:1)]# matrix numeric index
ci <- cbind(c("a", "b", "a"), c("A", "C", "B"))
m[ci]                 # matrix character index
m <- m[,-1]           # delete the first column of m
li[[1]]               # the first element of list li
y <- list(1, 2, a = 4, 5)
y[c(3, 4)]            # a list containing elements 3 and 4 of y
y$a                   # the element of y named a

## non-integer indices are truncated:
(i <- 3.999999999) # "4" is printed
(1:5)[i]  # 3

## named atomic vectors, compare "[" and "[[" :
nx <- c(Abc = 123, pi = pi)
nx[1] ; nx["pi"] # keeps names, whereas "[[" does not:
nx[[1]] ; nx[["pi"]]
\dontshow{
stopifnot(identical(names(nx[1]), "Abc"),
        identical(names(nx["pi"]), "pi"),
        is.null(names(nx[["Abc"]])), is.null(names(nx[[2]])))
}
## recursive indexing into lists
z <- list(a = list(b = 9, c = "hello"), d = 1:5)
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

## partial matching - possibly with warning :
stopifnot(identical(li$p, pi))
op <- options(warnPartialMatchDollar = TRUE)
stopifnot( identical(li$p, pi), #-- a warning
  inherits(tryCatch (li$p, warning = identity), "warning"))
## revert the warning option:
if(is.null(op[[1]])) op[[1]] <- FALSE; options(op)
}
\keyword{array}
\keyword{list}
