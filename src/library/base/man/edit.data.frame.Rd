\name{edit.data.frame}
\alias{edit.data.frame}
\alias{edit.matrix}
\title{Edit Data Frames and Matrices}
\description{
  Use data editor on data frame or matrix contents.
}
\usage{
edit.data.frame(name, factor.mode=c("numeric", "character"),
                edit.row.names = any(row.names(name) != 1:nrow(name)),
                \dots)
edit.matrix(name, edit.row.names = any(rownames(name)), \dots)
}
\arguments{
  \item{name}{A data frame or matrix.}
  \item{factor.mode}{How to handle factors (as integers or using
    character levels).}
  \item{edit.row.names}{logical. Show the row names be displayed as
    a separate editable column?}
}
\details{
  At present, this only works on simple data frames containing numeric or
  character vectors and factors. Factors are represented in the
  spreadsheet as either numeric vectors (which is more suitable for data
  entry) or character vectors (better for browsing). After editing,
  vectors are padded with \code{NA} to have the same length and
  factor attributes are restored.  The set of factor levels can not be
  changed by editing in numeric mode; invalid levels are changed to
  \code{NA} and a warning is issued.  If new factor levels are
  introduced in character mode, they are added at the end of the list of
  levels in the order in which they encountered.

  It is possible to use the data-editor's facilities to select the mode
  of columns to swap between numerical and factor columns in a data
  frame.  Changing any column in a numerical matrix to character will
  cause the result to be coerced to a character matrix.
}
\value{
  The edited data frame.
}
\author{ Peter Dalgaard }
\note{
  \code{fix(dataframe)} works for in-place editing by calling this
  function.

  If the data editor is not available, a dump of the object is presented
  for editing using the default method of \code{edit}.
}
\seealso{\code{\link{data.entry}}, \code{\link{edit}}}
\examples{
\dontrun{
data(InsectSprays)
edit(InsectSprays)
edit(InsectSprays, factor.mode="numeric")
}
}

\keyword{utilities}
