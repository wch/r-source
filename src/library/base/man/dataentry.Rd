\name{dataentry}
\title{Spreadsheet Interface for Entering Data}
\usage{
data.entry(\dots, Modes=NULL, Names=NULL)
dataentry(data, modes)
de(\dots, Modes=NULL, Names=NULL)
}
\alias{data.entry}
\alias{dataentry}
\alias{de}
\alias{de.ncols}
\alias{de.restore}
\alias{de.setup}
\description{
This is a fairly big suite of programs designed to make the interface
to the spreadsheet painless for users.  \code{data.entry} has side
effects, any changes made in the spreadsheet are reflected in the
variables.  The functions \code{de}, \code{de.ncols}, \code{de.setup}
and \code{de.restore} are designed to help achieve these side effects.
If the user passes in a matrix, \code{X} say, then the matrix is
broken into columns before \code{dataentry} is called. Then on return
the columns are collected and glued back together and the result
assigned to the variable \code{X}.  If you don't want this behaviour
use dataentry directly.

The primitive function is \code{dataentry}. It takes a list of
vectors of possibly different lengths and modes (the second argument)
and opens a spreadsheet with these variables being the columns.
The columns of the dataentry window are returned as vectors in a
list when the spreadsheet is closed.

\code{de.ncols} counts the number of columns which are supplied as arguments
to \code{data.entry}. It attempts to count columns in lists, matrices
and vectors.  \code{de.setup} sets things up so that on return the
columns can be regrouped and reassigned to the correct name. This
is handled by \code{de.restore}.
}
\seealso{
\code{\link{vi}}, \code{\link{ed}}.
}
\examples{
# call data entry with variables x and y
data.entry(x,y)
}
\keyword{utilities}
\keyword{file}
