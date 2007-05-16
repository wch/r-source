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
    to factors?}
}
\value{
  A data frame, a matrix-like structure whose columns may be of
  differing types (numeric, logical, factor and character and so on).
}
\details{
  A data frame is a list of variables of the same length with unique row
  names, given class \code{"data.frame"}.  If there are zero variables,
  the row names determine the number of rows.


  Duplicate column names are allowed, but you need to use
  \code{check.names=FALSE} for \code{data.frame} to generate such a data
  frame.  However, not all operations on data frames will preserve
  duplicated column names: for example matrix-like subsetting will force
  column names in the result to be unique.

  \code{data.frame} converts each of its arguments to a data frame by
  calling \code{\link{as.data.frame}(optional=TRUE)}.  As that is a
  generic function, methods can be written to change the behaviour of
  arguments according to their classes: \R comes with many such methods.
  Character variables passed to \code{data.frame} are converted to
  factor columns unless protected by \code{\link{I}}.  If a list or data
  frame or matrix is passed to \code{data.frame} it is as if each
  component or column had been passed as a separate argument.

  Objects passed to \code{data.frame} should have the same number of
  rows, but atomic vectors, factors and character vectors protected by
  \code{\link{I}} will be recycled a whole number of times if necessary.

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
  \code{TRUE} or \code{FALSE}.
}
\note{
  In versions of \R prior to 2.4.0 \code{row.names} had to be
  character: to ensure compatibility with earlier versions of \R, supply
  a character vector as the \code{row.names} argument.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{I}},
  \code{\link{plot.data.frame}},
  \code{\link{print.data.frame}},
  \code{\link{row.names}}, \code{\link{names}} (for the column names),
  \code{\link{[.data.frame}} for subsetting methods,
  \code{\link{Math.data.frame}} etc, about
  \emph{Group} methods for \code{data.frame}s;
  \code{\link{read.table}},
  \code{\link{make.names}}.
}
\examples{
L3 <- LETTERS[1:3]
(d <- data.frame(cbind(x=1, y=1:10), fac=sample(L3, 10, replace=TRUE)))

## The same with automatic column names:
data.frame(cbind(  1,   1:10),     sample(L3, 10, replace=TRUE))

is.data.frame(d)

## do not convert to factor, using I() :
(dd <- cbind(d, char = I(letters[1:10])))
rbind(class=sapply(dd, class), mode=sapply(dd, mode))

stopifnot(1:10 == row.names(d))# {coercion}

(d0  <- d[, FALSE]) # NULL data frame with 10 rows
(d.0 <- d[FALSE, ]) # <0 rows> data frame  (3 cols)
(d00 <- d0[FALSE,])  # NULL data frame with 0 rows
}
\keyword{classes}
\keyword{methods}
