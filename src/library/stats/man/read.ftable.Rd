\name{read.ftable}
\alias{read.ftable}
\alias{write.ftable}
\alias{as.table.ftable}
\title{Manipulate Flat Contingency Tables}
\description{Read, write and coerce \dQuote{flat} contingency tables.}
\usage{
read.ftable(file, sep = "", quote = "\"",
            row.var.names, col.vars, skip = 0)

write.ftable(x, file = "", quote = TRUE,
             digits = getOption("digits"))

\method{as.table}{ftable}(x, \dots)
}
\arguments{
  \item{file}{either a character string naming a file or a connection
    which the data are to be read from or written to.  \code{""}
    indicates input from the console for reading and output to the
    console for writing.}
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
  \item{x}{an object of class \code{"ftable"}.}
  \item{digits}{an integer giving the number of significant digits to
    use for (the cell entries of) \code{x}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
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

  Note that flat tables are characterized by their \dQuote{ragged}
  display of row (and maybe also column) labels.  If the full grid of
  levels of the row variables is given, one should instead use
  \code{read.table} to read in the data, and create the contingency
  table from this using \code{\link{xtabs}}.

  \code{write.ftable} writes a flat table to a file, which is useful for
  generating \dQuote{pretty} ASCII representations of contingency tables.

  \code{as.table.ftable} converts a contingency table in flat matrix
  form to one in standard array form.  This is a method for the generic
  function \code{as.table}.
}
\seealso{
  \code{\link{ftable}} for more information on flat contingency tables.
}
\references{
  Agresti, A. (1990)
  \emph{Categorical data analysis}.
  New York: Wiley.
}  
\examples{
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
