\name{ftable}
\alias{ftable}
\alias{ftable.default}
\alias{as.table.ftable}
\alias{print.ftable}
\alias{read.ftable}
\alias{write.ftable}
\title{Flat Contingency Tables}
\description{Create and manipulate ``flat'' contingency tables.}
\synopsis{
ftable(x, ...)
ftable.default(..., exclude = c(NA, NaN), row.vars = NULL, col.vars = NULL)
as.table.ftable(x)
print.ftable(x, digits = getOption("digits"), ...)
read.ftable(file, sep = "", quote = "\"", row.var.names, col.vars, skip = 0)
write.ftable(x, file = "", quote = TRUE, digits = getOption("digits"))
}
\usage{
ftable(\dots, exclude = c(NA, NaN), row.vars = NULL, col.vars = NULL)
as.table.ftable(x)
read.ftable(file, sep = "", quote = "\"",
            row.var.names, col.vars, skip = 0)
write.ftable(x, file = "", quote = TRUE, digits = getOption("digits"))
}
\arguments{
  \item{\dots}{\R objects which can be interpreted as factors (including
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
  \item{x}{an arbitrary \R object.}
  \item{file}{a character string giving the name of the file which the
    data are to be read from or written to.} 
  \item{sep}{the field separator string.  Values on each line of the file
    are separated by this string.}
  \item{quote}{a character string giving the set of quoting characters
    for \code{read.ftable}; to disable quoting altogether, use
    \code{quote=""}.  For \code{write.table}, a logical indicating
    whether strings in the data will be surrounded by double quotes.}
  \item{row.var.names}{a character vector with the names of the row
    variables, in case these cannot be determined automatically.}
  \item{col.vars}{a list giving the names and levels of the column
    variables, in case these cannot be determined automatically.}
  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}
  \item{digits}{an integer giving the number of significant digits to
    use for (the cell entries of) \code{x}.}
}
\value{
  \code{ftable} returns an object of class \code{"ftable"}, which is a
  matrix with counts of each combination of the levels of variables with
  information on the names and levels of the (row and columns) variables
  stored as attributes \code{"row.vars"} and \code{"col.vars"}.
}
\details{
  \code{ftable} creates ``flat'' contingency tables.  Similar to the
  usual contingency tables, these contain the counts of each combination
  of the levels of the variables (factors) involved.  This information
  is then re-arranged as a matrix whose rows and columns correspond to
  unique combinations of the levels of the row and column variables (as
  specified by \code{row.vars} and \code{col.vars}, respectively).  The
  combinations are created by looping over the variables in reverse
  order (so that the levels of the ``left-most'' variable vary the
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

  Function \code{\link{ftable.formula}} provides a formula method for
  creating flat contingency tables.

  \code{as.table.ftable} converts a contingency table in flat matrix
  form to one in standard array form.  This is a method for the generic
  function \code{as.table}.

  \code{write.ftable} writes a flat table to a file, which is useful for
  generating ``pretty'' ASCII representations of contingency tables.

  \code{read.ftable} reads in a flat-like contingency table from a
  file.  If the file contains the written representation of a flat
  table (more precisely, a header with all information on names and
  levels of column variables, followed by a line with the names of the
  row variables), no further arguments are needed.  Similarly, flat
  tables with only one column variable the name of which is the only
  entry in the first line are handled automatically.  Other variants can
  be dealt with by skipping all header information using \code{skip},
  and providing the names of the row variables and the names and levels
  of the column variable using \code{row.var.names} and \code{col.vars},
  respectively.  See the examples below.

  Note that flat tables are characterized by their ``ragged'' display of
  row (and maybe also column) labels.  If the full grid of levels of the
  row variables is given, one should instead use \code{read.table} to
  read in the data, and create the contingency table from this using
  \code{\link{xtabs}}.
}
\seealso{
  \code{\link{ftable.formula}} for the formula interface (which allows a
  \code{data = .} argument);
  \code{\link{table}} for ``ordinary'' cross-tabulation.
}
\references{
  Agresti, A.  (1990)
  \emph{Categorical data analysis.}
  New York: Wiley.
}  
\examples{
## Start with a contingency table.
data(Titanic)
ftable(Titanic, row.vars = 1:3)
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
ftable(Titanic, row.vars = 2:1, col.vars = "Survived")

## Start with a data frame.
data(mtcars)
x <- ftable(mtcars[c("cyl", "vs", "am", "gear")])
x
ftable(x, row.vars = c(2, 4))

## Agresti (1990), page 157, Table 5.8.
## Not in ftable standard format, but o.k.
file <- tempfile()
cat("             Intercourse\n",
    "Race  Gender     Yes  No\n",
    "White Male        43 134\n",
    "      Female      26 149\n",
    "Black Male        29  23\n",
    "      Female      22  36\n",
    file = file)
file.show(file)
ft <- read.ftable(file)
ft
unlink(file)

## Agresti (1990), page 297, Table 8.16.
## Almost o.k., but misses the name of the row variable.
file <- tempfile()
cat("                      \"Tonsil Size\"\n",
    "            \"Not Enl.\" \"Enl.\" \"Greatly Enl.\"\n",
    "Noncarriers       497     560           269\n",
    "Carriers           19      29            24\n",
    file = file)
file.show(file)
ft <- read.ftable(file, skip = 2,
                  row.var.names = "Status",
                  col.vars = list("Tonsil Size" =
                      c("Not Enl.", "Enl.", "Greatly Enl.")))
ft
unlink(file)
}
\keyword{category}
