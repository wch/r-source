\name{Extract.data.frame}
\alias{[.data.frame}
\alias{[[.data.frame}
\alias{[<-.data.frame}
\alias{[[<-.data.frame}
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
    number of times if necessary.}
}
\details{
  Data frames can be indexed in several modes.  When \code{[} and
  \code{[[} are used with a single index, they index the data frame
  as if it were a list. In this usage the \code{drop} argument is
  ignored, with a warning.

  When \code{[} and \code{[[} are used with two indices they act
  like indexing a matrix:  \code{[[} can only be used to select one element.

  \code{[} returns a data frame with unique (and non-missing) row names,
  if necessary transforming the row names using
  \code{\link{make.names}( * , unique = TRUE)}.
  (However, duplicate column names can be achieved by indexing.)

  The replacement methods can be used to add whole column(s) by specifying
  non-existent column(s), in which case the column(s) are added at the
  right-hand edge of the data frame and numerical indices must be
  contiguous to existing indices.

  As a special case, when using \code{[} for replacement, a logical
  matrix can be used to select the elements to be replaced in the same
  ways as for a matrix.  Missing values in the matrix are
  treated as false, unlike S which does not replace them but uses up the
  corresponding values in \code{value}.
}
\section{Warning}{
  Although the default for \code{drop} is \code{TRUE}, the default
  behaviour is equivalent to specifying \code{drop=FALSE} when only one
  \emph{row} is left.  To drop from a data frame to a list, \code{drop =
    FALSE} has to specified explicitly.
}
\value{
  For \code{[} a data frame, list or vector (the latter two only when
  dimensions have been dropped).

  For \code{[[} a data frame or a length-one vector (for extractions
  with two indices).
}
\seealso{
  \code{\link{data.frame}}, \code{\link{Extract}}
}
\examples{
data(swiss)
sw <- swiss[1:5, 1:4]  # select a manageable subset

sw[1:3]      # select columns
sw[, 1:3]    # same
sw[4:5, 1:3] # select rows and columns
sw[1]        # a one-column data frame
sw[, 1, drop = FALSE]  # the same
sw[, 1]      # a (unamed) vector
sw[[1]]      # the same

sw[1,]       # a one-row data frame
sw[1,, drop=TRUE]  # a list

swiss[ c(1, 1:2), ]   # duplicate row, unique row names are created

sw[sw <= 6] <- 6  # logical matrix indexing
sw
}
\keyword{array}
