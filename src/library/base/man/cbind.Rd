% File src/library/base/man/cbind.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{cbind}
\alias{cbind}%%--do C-internal method dispatching !!
\alias{rbind}
\alias{cbind.data.frame}
\alias{rbind.data.frame}
\alias{.__H__.cbind}
\alias{.__H__.rbind}
\concept{insert}
\title{Combine R Objects by Rows or Columns}
\description{
  Take a sequence of vector, matrix or data-frame arguments and combine
  by \emph{c}olumns or \emph{r}ows, respectively.  These are generic
  functions with methods for other \R classes.
}
\usage{
cbind(\dots, deparse.level = 1)
rbind(\dots, deparse.level = 1)
\method{rbind}{data.frame}(\dots, deparse.level = 1, make.row.names = TRUE,
      stringsAsFactors = default.stringsAsFactors())
}
\arguments{
  \item{\dots}{(generalized) vectors or matrices.  These can be given as named
    arguments.  Other \R objects may be coerced as appropriate, or S4
    methods may be used: see sections \sQuote{Details} and
    \sQuote{Value}.  (For the \code{"data.frame"} method of \code{cbind}
    these can be further arguments to \code{\link{data.frame}} such as
    \code{stringsAsFactors}.)}
  \item{deparse.level}{integer controlling the construction of labels in
    the case of non-matrix-like arguments (for the default method):\cr
    \code{deparse.level = 0} constructs no labels; the default,\cr
    \code{deparse.level = 1 or 2} constructs labels from the argument
    names, see the \sQuote{Value} section below.}
  \item{make.row.names}{(only for data frame method:) logical
    indicating if unique and valid \code{\link{row.names}} should be
    constructed from the arguments.}
}
\details{
  The functions \code{cbind} and \code{rbind} are S3 generic, with
  methods for data frames.  The data frame method will be used if at
  least one argument is a data frame and the rest are vectors or
  matrices.  There can be other methods; in particular, there is one for
  time series objects.  See the section on \sQuote{Dispatch} for how
  the method to be used is selected.  If some of the arguments are of an
  S4 class, i.e., \code{\link{isS4}(.)} is true, S4 methods are sought
  also, and the hidden \code{cbind} / \code{rbind} functions
  from package \pkg{methods} maybe called, which in turn build on
  \code{\link{cbind2}} or \code{\link{rbind2}}, respectively.  In that
  case, \code{deparse.level} is obeyed, similarly to the default method.

  In the default method, all the vectors/matrices must be atomic (see
  \code{\link{vector}}) or lists.  Expressions are not allowed.
  Language objects (such as formulae and calls) and pairlists will be
  coerced to lists: other objects (such as names and external pointers)
  will be included as elements in a list result.  Any classes the inputs
  might have are discarded (in particular, factors are replaced by their
  internal codes).

  If there are several matrix arguments, they must all have the same
  number of columns (or rows) and this will be the number of columns (or
  rows) of the result.  If all the arguments are vectors, the number of
  columns (rows) in the result is equal to the length of the longest
  vector.  Values in shorter arguments are recycled to achieve this
  length (with a \code{\link{warning}} if they are recycled only
  \emph{fractionally}).

  When the arguments consist of a mix of matrices and vectors the number
  of columns (rows) of the result is determined by the number of columns
  (rows) of the matrix arguments.  Any vectors have their values
  recycled or subsetted to achieve this length.

  For \code{cbind} (\code{rbind}), vectors of zero length (including
  \code{NULL}) are ignored unless the result would have zero rows
  (columns), for S compatibility.
  (Zero-extent matrices do not occur in S3 and are not ignored in \R.)

  Matrices are restricted to less than \eqn{2^{31}}{2^31} rows and
  columns even on 64-bit systems.  So input vectors have the same length
  restriction: as from \R 3.2.0 input matrices with more elements (but
  meeting the row and column restrictions) are allowed.
}
\section{Data frame methods}{
  The \code{cbind} data frame method is just a wrapper for
  \code{\link{data.frame}(..., check.names = FALSE)}.  This means that
  it will split matrix columns in data frame arguments, and convert
  character columns to factors unless \code{stringsAsFactors = FALSE} is
  specified.

  The \code{rbind} data frame method first drops all zero-column and
  zero-row arguments.  (If that leaves none, it returns the first
  argument with columns otherwise a zero-column zero-row data frame.)
  It then takes the classes of the columns from the
  first data frame, and matches columns by name (rather than by
  position).  Factors have their levels expanded as necessary (in the
  order of the levels of the levelsets of the factors encountered) and
  the result is an ordered factor if and only if all the components were
  ordered factors.  (The last point differs from S-PLUS.)  Old-style
  categories (integer vectors with levels) are promoted to factors.
}
\value{
  For the default method, a matrix combining the \code{\dots} arguments
  column-wise or row-wise.  (Exception: if there are no inputs or all
  the inputs are \code{NULL}, the value is \code{NULL}.)

  The type of a matrix result determined from the highest type of any of
  the inputs in the hierarchy raw < logical < integer < double < complex <
  character < list .

  For \code{cbind} (\code{rbind}) the column (row) names are taken from
  the \code{colnames} (\code{rownames}) of the arguments if these are
  matrix-like.  Otherwise from the names of the arguments or where those
  are not supplied and \code{deparse.level > 0}, by deparsing the
  expressions given, for \code{deparse.level = 1} only if that gives a
  sensible name (a \sQuote{symbol}, see \code{\link{is.symbol}}).

  For \code{cbind} row names are taken from the first argument with
  appropriate names: rownames for a matrix, or names for a vector of
  length the number of rows of the result.

  For \code{rbind} column names are taken from the first argument with
  appropriate names: colnames for a matrix, or names for a vector of
  length the number of columns of the result.
}
\section{Dispatch}{
  The method dispatching is \emph{not} done via
  \code{\link{UseMethod}()}, but by C-internal dispatching.
  Therefore there is no need for, e.g., \code{rbind.default}.

  The dispatch algorithm is described in the source file
  (\file{.../src/main/bind.c}) as

  \enumerate{
    \item For each argument we get the list of possible class
    memberships from the class attribute.

    \item We inspect each class in turn to see if there is an
    applicable method.

    \item If we find an applicable method we make sure that it is
     identical to any method determined for prior arguments.
     If it is identical, we proceed, otherwise we immediately
     drop through to the default code.
   }
   If you want to combine other objects with data frames, it may be
   necessary to coerce them to data frames first.  (Note that this
   algorithm can result in calling the data frame method if all the
   arguments are either data frames or vectors, and this will result in
   the coercion of character vectors to factors.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{c}} to combine vectors (and lists) as vectors,
  \code{\link{data.frame}} to combine vectors and matrices as a data
  frame.
}
\examples{
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m
cbind(1:7, diag(3)) # vector is subset -> warning

cbind(0, rbind(1, 1:3))
cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names
xx <- data.frame(I = rep(0,2))
cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1

## deparse.level
dd <- 10
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2) # 4 rownames

## cheap row names:
b0 <- gl(3,4, labels=letters[1:3])
bf <- setNames(b0, paste0("o", seq_along(b0)))
df  <- data.frame(a = 1, B = b0, f = gl(4,3))
df. <- data.frame(a = 1, B = bf, f = gl(4,3))
new <- data.frame(a = 8, B ="B", f = "1")
(df1  <- rbind(df , new))
(df.1 <- rbind(df., new))
stopifnot(identical(df1, rbind(df,  new, make.row.names=FALSE)),
          identical(df1, rbind(df., new, make.row.names=FALSE)))
\dontshow{
## Testing a semi-official use:
d2 <- rbind.data.frame(as.list(df), as.list(new))
d3 <- rbind.data.frame(as.list(df), as.list(new), make.row.names=FALSE)
stopifnot(identical(.row_names_info(d3), -13L),
          attr(d2, "row.names")[c(1,13)] == c("13", "131"))
row.names(d2) <- attr(d3, "row.names")# = 1:13
stopifnot(identical(d2, d3))
}
}
\keyword{array}
\keyword{manip}
