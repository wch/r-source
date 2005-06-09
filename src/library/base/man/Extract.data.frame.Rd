\name{Extract.data.frame}
\alias{[.data.frame}
\alias{[[.data.frame}
\alias{[<-.data.frame}
\alias{[[<-.data.frame}
\alias{$<-.data.frame}
\title{Extract or Replace Parts of a Data Frame}
\description{
  Extract or replace subsets of data frames.
}
\usage{
x[i]
x[i] <- value
x[i, j, drop = TRUE]
x[i, j] <- value

x[[i]]
x[[i]] <- value
x[[i, j]]
x[[i, j]] <- value

x$name
x$name <- value
}
\arguments{
  \item{x}{data frame.}

  \item{i, j}{elements to extract or replace. \code{i, j} are
    \code{numeric} or \code{character} or, for \code{[} only, empty.
    Numeric values are coerced to integer as if by \code{\link{as.integer}}.
    For replacement by \code{[}, a logical matrix is allowed.
  }

  \item{drop}{logical.  If \code{TRUE} the result is coerced to the
    lowest possible dimension: however, see the Warning below.}

  \item{value}{A suitable replacement value: it will be repeated a whole
    number of times if necessary and it may be coerced: see the
    Coercion section.  If \code{NULL}, deletes the column if a single
    column is selected.}

  \item{name}{name or literal character string.}
}
\details{
  Data frames can be indexed in several modes.  When \code{[} and
  \code{[[} are used with a single index, they index the data frame
  as if it were a list.  In this usage a \code{drop} argument is
  ignored, with a warning.  Using \code{$} is equivalent to using
  \code{[[} with a single index.

  When \code{[} and \code{[[} are used with two indices they act
  like indexing a matrix:  \code{[[} can only be used to select one element.

  If \code{[} returns a data frame it will have unique (and non-missing)
  row names, if necessary transforming the row names using
  \code{\link{make.unique}}.  Similarly, column names
  will be transformed (if columns are selected more than once).

  When \code{drop =TRUE}, this is applied to the subsetting of any
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
  necessary.  If the columns specified by number are created, the names
  (if any) of the corresponding list elements are used to name the
  columns.  If the replacement is not selecting rows, list values can
  contain \code{NULL} elements which will cause the corresponding
  columns to be deleted.

  Matrixing indexing using \code{[} is not recommended, and barely
  supported.  For extraction, \code{x} is first coerced to a matrix.
  For replacement a logical matrix (only) can be used to select the
  elements to be replaced in the same ways as for a matrix (except that
  missing values in the index are only allowed in a few special cases).
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
  the type of the column in which it is to be placed.

  When  \code{[} and \code{[[} are used with two indices, the
  column will be coerced as necessary to accommodate the value.

  Note that when the replacement value is an array (including a matrix)
  it is \emph{not} treated as a series of columns (as
  \code{\link{data.frame}} and \code{\link{as.data.frame}} do) but
  inserted as a single column.
}
\section{Warning}{
  Although the default for \code{drop} is \code{TRUE}, the default
  behaviour when only one \emph{row} is left is equivalent to
  specifying \code{drop = FALSE}.  To drop from a data frame to a list,
  \code{drop = TRUE} has to be specified explicitly.
}
\value{
  For \code{[} a data frame, list or a single column (the latter two
  only when dimensions have been dropped).  If matrix indexing is used for
  extraction a matrix results.

  For \code{[[} a column of the data frame (extraction with one index)
  or a length-one vector (extraction with two indices).

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
sw[1,, drop=TRUE]  # a list

swiss[ c(1, 1:2), ]   # duplicate row, unique row names are created

sw[sw <= 6] <- 6  # logical matrix indexing
sw

## adding a column
sw["new1"] <- LETTERS[1:5]   # adds a character column
sw[["new2"]] <- letters[1:5] # ditto
sw[, "new3"] <- LETTERS[1:5] # ditto
                             # but this got converted to a factor in 1.7.x
sw$new4 <- 1:5
sapply(sw, class)
sw$new4 <- NULL              # delete the column
sw
sw[6:8] <- list(letters[10:14], NULL, aa=1:5) # delete col7, update 6, append
sw

## matrices in a data frame
A <- data.frame(x=1:3, y=I(matrix(4:6)), z=I(matrix(letters[1:9],3,3)))
A[1:3, "y"] # a matrix, was a vector prior to 1.8.0
A[1:3, "z"] # a matrix
A[, "y"]    # a matrix
}
\keyword{array}
