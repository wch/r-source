% File src/library/utils/man/edit.data.frame.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

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

\method{edit}{matrix}(name, edit.row.names = !is.null(dn[[1]]), \dots)
}
\arguments{
  \item{name}{A data frame or (numeric, logical or character) matrix.}
  \item{factor.mode}{How to handle factors (as integers or using
    character levels) in a data frame.  Can be abbreviated.}
  \item{edit.row.names}{logical. Show the row names (if they exist) be
    displayed as a separate editable column?  It is an error to ask for
    this on a matrix with \code{NULL} row names.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  At present, this only works on simple data frames containing numeric,
  logical or character vectors and factors, and numeric, logical or
  character matrices.  Any other mode of matrix will give an error, and
  a warning is given when the matrix has a class (which will be discarded).

  Data frame columns are coerced on input to \emph{character} unless
  numeric (in the sense of \code{is.numeric}), logical or factor.  A
  warning is given when classes are discarded.  Special characters
  (tabs, non-printing ASCII, etc.) will be displayed as escape sequences.

  Factors columns are represented in the spreadsheet as either numeric
  vectors (which are more suitable for data entry) or character vectors
  (better for browsing). After editing, vectors are padded with
  \code{NA} to have the same length and factor attributes are restored.
  The set of factor levels can not be changed by editing in numeric
  mode; invalid levels are changed to \code{NA} and a warning is issued.
  If new factor levels are introduced in character mode, they are added
  at the end of the list of levels in the order in which they
  encountered.

  It is possible to use the data-editor's facilities to select the mode
  of columns to swap between numerical and factor columns in a data
  frame.  Changing any column in a numerical matrix to character will
  cause the result to be coerced to a character matrix.  Changing
  the mode of logical columns is not supported.

  For a data frame, the row names will be taken from the original object
  if \code{edit.row.names = FALSE} and the number of rows is unchanged,
  and from the edited output if \code{edit.row.names = TRUE} and there
  are no duplicates.  (If the \code{row.names} column is incomplete, it
  is extended by entries like \code{row223}.)  In all other cases the
  row names are replaced by \code{seq(length = nrows)}.

  For a matrix, colnames will be added (of the form \code{col7}) if
  needed.  The rownames will be taken from the original object if
  \code{edit.row.names = FALSE} and the number of rows is unchanged
  (otherwise \code{NULL}), and from the edited output if
  \code{edit.row.names = TRUE}.  (If the \code{row.names} column is
  incomplete, it is extended by entries like \code{row223}.)

  Editing a matrix or data frame will lose all attributes apart from the
  row and column names.
}
\value{
  The edited data frame or matrix.
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
edit(InsectSprays, factor.mode = "numeric")
}
}

\keyword{utilities}
