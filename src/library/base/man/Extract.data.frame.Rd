% File src/library/base/man/Extract.data.frame.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Extract.data.frame}
\alias{[.data.frame}
\alias{[[.data.frame}
\alias{[<-.data.frame}
\alias{[[<-.data.frame}
\alias{$.data.frame}
\alias{$<-.data.frame}
\title{Extract or Replace Parts of a Data Frame}
\description{
  Extract or replace subsets of data frames.
}
\usage{
\method{[}{data.frame}(x, i, j, drop = )
\method{[}{data.frame}(x, i, j) <- value
\method{[[}{data.frame}(x, ..., exact = TRUE)
\method{[[}{data.frame}(x, i, j) <- value
\method{$}{data.frame}(x, name)
\method{$}{data.frame}(x, name) <- value
}
\arguments{
  \item{x}{data frame.}

  \item{i, j, ...}{elements to extract or replace.  For \code{[} and
    \code{[[}, these are \code{numeric} or \code{character} or, for
    \code{[} only, empty.  Numeric values are coerced to integer as if
    by \code{\link{as.integer}}.  For replacement by \code{[}, a logical
    matrix is allowed.}

  \item{name}{
    A literal character string or a \link{name} (possibly \link{backtick}
    quoted).}

  \item{drop}{logical.  If \code{TRUE} the result is coerced to the
    lowest possible dimension.  The default is to drop if only one
    column is left, but \bold{not} to drop if only one row is left.}

  \item{value}{A suitable replacement value: it will be repeated a whole
    number of times if necessary and it may be coerced: see the
    Coercion section.  If \code{NULL}, deletes the column if a single
    column is selected.}

   \item{exact}{logical: see \code{\link{[}}, and applies to column names.}
}
\details{
  Data frames can be indexed in several modes.  When \code{[} and
  \code{[[} are used with a single vector index (\code{x[i]} or
  \code{x[[i]]}), they index the data frame as if it were a list.  In
  this usage a \code{drop} argument is ignored, with a warning.

  The \code{data.frame} method for \code{$}, treats \code{x} as a
  list, except that (as of R-3.1.0) partial matching of \code{name} to
  the names of \code{x} will generate a warning; this may become an
  error in future versions.  The replacement method checks
  \code{value} for the correct number of rows, and replicates it if
  necessary.

  When \code{[} and \code{[[} are used with two indices (\code{x[i, j]}
  and \code{x[[i, j]]}) they act like indexing a matrix:  \code{[[} can
  only be used to select one element.  Note that for each selected
  column, \code{xj} say, typically (if it is not matrix-like), the
  resulting column will be \code{xj[i]}, and hence rely on the
  corresponding \code{[} method, see the examples section.

  If \code{[} returns a data frame it will have unique (and non-missing)
  row names, if necessary transforming the row names using
  \code{\link{make.unique}}.  Similarly, if columns are selected column
  names will be transformed to be unique if necessary (e.g., if columns
  are selected more than once, or if more than one column of a given
  name is selected if the data frame has duplicate column names).

  When \code{drop = TRUE}, this is applied to the subsetting of any
  matrices contained in the data frame as well as to the data frame itself.

  The replacement methods can be used to add whole column(s) by specifying
  non-existent column(s), in which case the column(s) are added at the
  right-hand edge of the data frame and numerical indices must be
  contiguous to existing indices.  On the other hand, rows can be added
  at any row after the current last row, and the columns will be
  in-filled with missing values.  Missing values in the indices are not
  allowed for replacement.

  For \code{[} the replacement value can be a list: each element of the
  list is used to replace (part of) one column, recycling the list as
  necessary.  If columns specified by number are created, the names
  (if any) of the corresponding list elements are used to name the
  columns.  If the replacement is not selecting rows, list values can
  contain \code{NULL} elements which will cause the corresponding
  columns to be deleted.  (See the Examples.)

  Matrix indexing (\code{x[i]} with a logical or a 2-column integer
  matrix \code{i}) using \code{[} is not recommended.  For extraction,
  \code{x} is first coerced to a matrix. For replacement, logical
  matrix indices must be of the same dimension as \code{x}.
  Replacements are done one column at a time, with multiple type
  coercions possibly taking place.

  Both \code{[} and \code{[[} extraction methods partially match row
  names.  By default neither partially match column names, but \code{[[}
  will if \code{exact = FALSE} (and with a warning if \code{exact =
  NA}).  If you want to exact matching on row names use
  \code{\link{match}}, as in the examples.
}
\section{Coercion}{
  The story over when replacement values are coerced is a complicated
  one, and one that has changed during \R's development.  This section
  is a guide only.

  When \code{[} and \code{[[} are used to add or replace a whole column,
  no coercion takes place but \code{value} will be
  replicated (by calling the generic function \code{\link{rep}}) to the
  right length if an exact number of repeats can be used.

  When \code{[} is used with a logical matrix, each value is coerced to
  the type of the column into which it is to be placed.

  When  \code{[} and \code{[[} are used with two indices, the
  column will be coerced as necessary to accommodate the value.

  Note that when the replacement value is an array (including a matrix)
  it is \emph{not} treated as a series of columns (as
  \code{\link{data.frame}} and \code{\link{as.data.frame}} do) but
  inserted as a single column.
}
\section{Warning}{
  The default behaviour when only one \emph{row} is left is equivalent to
  specifying \code{drop = FALSE}.  To drop from a data frame to a list,
  \code{drop = TRUE} has to be specified explicitly.

  Arguments other than \code{drop} and \code{exact} should not be named:
  there is a warning if they are and the behaviour differs from the
  description here.
}
\value{
  For \code{[} a data frame, list or a single column (the latter two
  only when dimensions have been dropped).  If matrix indexing is used for
  extraction a vector results.  If the result would be a data frame an
  error results if undefined columns are selected (as there is no general
  concept of a 'missing' column in a data frame).  Otherwise if a single
  column is selected and this is undefined the result is \code{NULL}.

  For \code{[[} a column of the data frame or \code{NULL}
  (extraction with one index)
  or a length-one vector (extraction with two indices).

  For \code{$}, a column of the data frame (or \code{NULL}).

  For \code{[<-}, \code{[[<-} and \code{$<-}, a data frame.
}
\seealso{
  \code{\link{subset}} which is often easier for extraction,
  \code{\link{data.frame}}, \code{\link{Extract}}.
}
\examples{
sw <- swiss[1:5, 1:4]  # select a manageable subset

sw[1:3]      # select columns
sw[, 1:3]    # same
sw[4:5, 1:3] # select rows and columns
sw[1]        # a one-column data frame
sw[, 1, drop = FALSE]  # the same
sw[, 1]      # a (unnamed) vector
sw[[1]]      # the same

sw[1,]       # a one-row data frame
sw[1,, drop = TRUE]  # a list

sw["C", ] # partially matches
sw[match("C", row.names(sw)), ] # no exact match
try(sw[, "Ferti"]) # column names must match exactly

\dontshow{
stopifnot(identical(sw[, 1], sw[[1]]),
          identical(sw[, 1][1], 80.2),
          identical(sw[, 1, drop = FALSE], sw[1]),
          is.data.frame(sw[1 ]), dim(sw[1 ]) == c(5,1),
          is.data.frame(sw[1,]), dim(sw[1,]) == c(1,4),
          is.list(s1 <- sw[1, , drop = TRUE]), identical(s1$Fertility, 80.2))
tools::assertError(sw[, "Ferti"])
}
swiss[ c(1, 1:2), ]   # duplicate row, unique row names are created

sw[sw <= 6] <- 6  # logical matrix indexing
sw

## adding a column
sw["new1"] <- LETTERS[1:5]   # adds a character column
sw[["new2"]] <- letters[1:5] # ditto
sw[, "new3"] <- LETTERS[1:5] # ditto
sw$new4 <- 1:5
sapply(sw, class)
sw$new4 <- NULL              # delete the column
sw
sw[6:8] <- list(letters[10:14], NULL, aa = 1:5)
# update col. 6, delete 7, append
sw

## matrices in a data frame
A <- data.frame(x = 1:3, y = I(matrix(4:9, 3, 2)),
                         z = I(matrix(letters[1:9], 3, 3)))
A[1:3, "y"] # a matrix
A[1:3, "z"] # a matrix
A[, "y"]    # a matrix
stopifnot(identical(colnames(A), c("x", "y", "z")), ncol(A) == 3L,
          identical(A[,"y"], A[1:3, "y"]),
          inherits (A[,"y"], "AsIs"))

## keeping special attributes: use a class with a
## "as.data.frame" and "[" method;
## "avector" := vector that keeps attributes.   Could provide a constructor
##  avector <- function(x) { class(x) <- c("avector", class(x)); x }
as.data.frame.avector <- as.data.frame.vector

`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}

d <- data.frame(i = 0:7, f = gl(2,4),
                u = structure(11:18, unit = "kg", class = "avector"))
str(d[2:4, -1]) # 'u' keeps its "unit"
\dontshow{
stopifnot(identical(d[2:4,-1][,"u"],
                    structure(12:14, unit = "kg", class = "avector")))
}
}
\keyword{array}
