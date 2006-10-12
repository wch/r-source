\name{ftable}
\title{Flat Contingency Tables}
\alias{ftable}
\alias{ftable.default}
\alias{print.ftable}
\description{Create \dQuote{flat} contingency tables.}
\usage{
ftable(x, \dots)

\method{ftable}{default}(\dots, exclude = c(NA, NaN), row.vars = NULL, col.vars = NULL)
}
\arguments{
  \item{x, \dots}{\R objects which can be interpreted as factors (including
    character strings), or a list (or data frame) whose components can
    be so interpreted, or a contingency table object of class
    \code{"table"} or \code{"ftable"}.}
  \item{exclude}{values to use in the exclude argument of \code{factor}
    when interpreting non-factor objects.}
  \item{row.vars}{a vector of integers giving the numbers of the
    variables, or a character vector giving the names of the variables
    to be used for the rows of the flat contingency table.}
  \item{col.vars}{a vector of integers giving the numbers of the
    variables, or a character vector giving the names of the variables
    to be used for the columns of the flat contingency table.}
}
\value{
  \code{ftable} returns an object of class \code{"ftable"}, which is a
  matrix with counts of each combination of the levels of variables with
  information on the names and levels of the (row and columns) variables
  stored as attributes \code{"row.vars"} and \code{"col.vars"}.
}
\details{
  \code{ftable} creates \dQuote{flat} contingency tables.  Similar to the
  usual contingency tables, these contain the counts of each combination
  of the levels of the variables (factors) involved.  This information
  is then re-arranged as a matrix whose rows and columns correspond to
  unique combinations of the levels of the row and column variables (as
  specified by \code{row.vars} and \code{col.vars}, respectively).  The
  combinations are created by looping over the variables in reverse
  order (so that the levels of the \dQuote{left-most} variable vary the
  slowest).  Displaying a contingency table in this flat matrix form
  (via \code{print.ftable}, the print method for objects of class
  \code{"ftable"}) is often preferable to showing it as a
  higher-dimensional array.

  \code{ftable} is a generic function.  Its default method,
  \code{ftable.default}, first creates a contingency table in array
  form from all arguments except \code{row.vars} and \code{col.vars}.
  If the first argument is of class \code{"table"}, it represents a
  contingency table and is used as is; if it is a flat table of class
  \code{"ftable"}, the information it contains is converted to the usual
  array representation using \code{as.ftable}.  Otherwise, the arguments
  should be \R objects which can be interpreted as factors (including
  character strings), or a list (or data frame) whose components can be
  so interpreted, which are cross-tabulated using \code{\link{table}}.
  Then, the arguments \code{row.vars} and \code{col.vars} are used to
  collapse the contingency table into flat form.  If neither of these
  two is given, the last variable is used for the columns.  If both are
  given and their union is a proper subset of all variables involved,
  the other variables are summed out.

  When the arguments are \R expressions interpreted as factors,
  additional arguments will be passed to \code{table} to control how
  the variable names are displayed; see the last example below.

  Function \code{\link{ftable.formula}} provides a formula method for
  creating flat contingency tables.

  There are methods for \code{\link{as.table}} and
  \code{\link{as.data.frame}}.
}
\seealso{
  \code{\link{ftable.formula}} for the formula interface (which allows a
  \code{data = .} argument);
  \code{\link{read.ftable}} for information on reading, writing and
  coercing flat contingency tables;
  \code{\link{table}} for \dQuote{ordinary} cross-tabulation;
  \code{\link{xtabs}} for formula-based cross-tabulation.
}
\examples{
## Start with a contingency table.
ftable(Titanic, row.vars = 1:3)
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
ftable(Titanic, row.vars = 2:1, col.vars = "Survived")

## Start with a data frame.
x <- ftable(mtcars[c("cyl", "vs", "am", "gear")])
x
ftable(x, row.vars = c(2, 4))

## Start with expressions, use table()'s "dnn" to change labels
ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear, row.vars = c(2, 4),
       dnn = c("Cylinders", "V/S", "Transmission", "Gears"))
}
\keyword{category}
