% File src/library/base/man/data.frame.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{data.frame}
\title{Data Frames}
\alias{data.frame}
\alias{default.stringsAsFactors}
\description{
  This function creates data frames, tightly coupled
  collections of variables which share many of the properties of
  matrices and of lists, used as the fundamental data structure by most
  of \R's modeling software.
}
\usage{
data.frame(\dots, row.names = NULL, check.rows = FALSE,
           check.names = TRUE,
           stringsAsFactors = default.stringsAsFactors())

default.stringsAsFactors()
}
\arguments{
  \item{\dots}{these arguments are of either the form \code{value} or
    \code{tag = value}.  Component names are created based on the tag (if
    present) or the deparsed argument itself.}
  \item{row.names}{\code{NULL} or a single integer or character string
    specifying a column to be used as row names, or a character or
    integer vector giving the row names for the data frame.}
  \item{check.rows}{if \code{TRUE} then the rows are checked for
    consistency of length and names.}
  \item{check.names}{logical.  If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    syntactically valid variable names and are not duplicated.
    If necessary they are adjusted (by \code{\link{make.names}})
    so that they are.}
  \item{stringsAsFactors}{logical: should character vectors be converted
    to factors?  The \sQuote{factory-fresh} default is \code{TRUE}, but
    this can be changed by setting \code{\link{options}(stringsAsFactors
      = FALSE)}.}
}
\value{
  A data frame, a matrix-like structure whose columns may be of
  differing types (numeric, logical, factor and character and so on).

  How the names of the data frame are created is complex, and the rest
  of this paragraph is only the basic story.  If the arguments are all
  named and simple objects (not lists, matrices of data frames) then the
  argument names give the column names.  For an unnamed simple argument,
  a deparsed version of the argument is used as the name (with an
  enclosing \code{I(...)} removed).  For a named matrix/list/data frame
  argument with more than one named column, the names of the columns are
  the name of the argument followed by a dot and the column name inside
  the argument: if the argument is unnamed, the argument's column names
  are used.  For a named or unnamed matrix/list/data frame argument that
  contains a single column, the column name in the result is the column
  name in the argument.  Finally, the names are adjusted to be unique
  and syntactically valid unless \code{check.names = FALSE}.
}

\details{
  A data frame is a list of variables of the same number of rows with
  unique row names, given class \code{"data.frame"}.  If no variables
  are included, the row names determine the number of rows.

  The column names should be non-empty, and attempts to use empty names
  will have unsupported results.  Duplicate column names are allowed,
  but you need to use \code{check.names = FALSE} for \code{data.frame}
  to generate such a data frame.  However, not all operations on data
  frames will preserve duplicated column names: for example matrix-like
  subsetting will force column names in the result to be unique.

  \code{data.frame} converts each of its arguments to a data frame by
  calling \code{\link{as.data.frame}(optional = TRUE)}.  As that is a
  generic function, methods can be written to change the behaviour of
  arguments according to their classes: \R comes with many such methods.
  Character variables passed to \code{data.frame} are converted to
  factor columns unless protected by \code{\link{I}} or argument
  \code{stringsAsFactors} is false.  If a list or data
  frame or matrix is passed to \code{data.frame} it is as if each
  component or column had been passed as a separate argument (except for
  matrices of class \code{"\link{model.matrix}"} and those protected by
  \code{\link{I}}).

  Objects passed to \code{data.frame} should have the same number of
  rows, but atomic vectors, factors and character vectors protected by
  \code{\link{I}} will be recycled a whole number of times if necessary
  (including as elements of list arguments).

  If row names are not supplied in the call to \code{data.frame}, the
  row names are taken from the first component that has suitable names,
  for example a named vector or a matrix with rownames or a data frame.
  (If that component is subsequently recycled, the names are discarded
  with a warning.)  If \code{row.names} was supplied as \code{NULL} or no
  suitable component was found the row names are the integer sequence
  starting at one (and such row names are considered to be
  \sQuote{automatic}, and not preserved by \code{\link{as.matrix}}).

  If row names are supplied of length one and the data frame has a
  single row, the \code{row.names} is taken to specify the row names and
  not a column (by name or number).

  Names are removed from vector inputs not protected by \code{\link{I}}.

  \code{default.stringsAsFactors} is a utility that takes
  \code{\link{getOption}("stringsAsFactors")} and ensures the result is
  \code{TRUE} or \code{FALSE} (or throws an error if the value is not
  \code{NULL}).
}
\note{
  In versions of \R prior to 2.4.0 \code{row.names} had to be
  character: to ensure compatibility with such versions of \R, supply
  a character vector as the \code{row.names} argument.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{I}},
  \code{\link{plot.data.frame}},
  \code{\link{print.data.frame}},
  \code{\link{row.names}}, \code{\link{names}} (for the column names),
  \code{\link{[.data.frame}} for subsetting methods,% ./Extract.data.frame.Rd
  \code{\link{Math.data.frame}} etc, about
  \emph{Group} methods for \code{data.frame}s;
  \code{\link{read.table}},
  \code{\link{make.names}}.
}
\examples{
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
(d <- data.frame(x = 1, y = 1:10, fac = fac))
## The "same" with automatic column names:
data.frame(1, 1:10, sample(L3, 10, replace = TRUE))

is.data.frame(d)

## do not convert to factor, using I() :
(dd <- cbind(d, char = I(letters[1:10])))
rbind(class = sapply(dd, class), mode = sapply(dd, mode))

stopifnot(1:10 == row.names(d))  # {coercion}

(d0  <- d[, FALSE])   # data frame with 0 columns and 10 rows
(d.0 <- d[FALSE, ])   # <0 rows> data frame  (3 named cols)
(d00 <- d0[FALSE, ])  # data frame with 0 columns and 0 rows
}
\keyword{classes}
\keyword{methods}
