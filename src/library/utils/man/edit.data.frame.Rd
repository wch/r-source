\name{edit.data.frame}
\alias{edit.data.frame}
\alias{edit.matrix}
\title{Edit Data Frames and Matrices}
\description{
  Use data editor on data frame or matrix contents.
}
\usage{
\method{edit}{data.frame}(name, factor.mode = c("character", "numeric"),
     edit.row.names = any(row.names(name) != 1:nrow(name)), \dots)

\method{edit}{matrix}(name, edit.row.names = any(rownames(name) != 1:nrow(name)), \dots)
}
\arguments{
  \item{name}{A data frame or matrix.}
  \item{factor.mode}{How to handle factors (as integers or using
    character levels) in a data frame.}
  \item{edit.row.names}{logical. Show the row names be displayed as
    a separate editable column?}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  At present, this only works on simple data frames containing numeric,
  logical or character vectors and factors.  Factors are represented in the
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
  cause the result to be coerced to a character matrix.  Changing 
  the mode of logical columns is not supported.

  The columns are coerced on input to numeric unless logical, character
  or factor (which may well not be what you want), and character columns
  not protected by \code{\link{I}()} will be coerced to factor on return.
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

  At present the data editor is limited to 65535 rows.
}
\seealso{\code{\link{data.entry}}, \code{\link{edit}}}
\examples{
\dontrun{
edit(InsectSprays)
edit(InsectSprays, factor.mode="numeric")
}
}

\keyword{utilities}
