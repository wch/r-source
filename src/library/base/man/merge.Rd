\name{merge}
\alias{merge}
\alias{merge.default}
\alias{merge.data.frame}
\title{Merge Two Data Frames}
\description{
  Merge two data frames by common columns or row names.
}
\usage{
merge(x, y, by, by.x, by.y, sort = TRUE)
}
\synopsis{
merge(x, y, by, \dots)
merge.date.frame(x, y, by = intersect(names(x), names(y)), by.x = by,
                 by.y = by, sort = TRUE) 
}
\arguments{
 \item{x, y}{data frames, or objects to be coerced to one}
 \item{by, by.x, by.y}{specifcations of the common columns. See Details.}
 \item{sort}{logical. Should the results be sorted on the \code{by} columns?}
}
\details{
  By default the data frames are merged on the columns with names they
  both have, but separate specifcations of the columns can be given by
  \code{by.x} and \code{by.y}. Columns can be specified by name, number
  or by a logical vector: the name \code{"row.names"} or the number
  \code{0} specifies the row names.  The rows in the two data frames
  that match on the specified columns are extracted, and joined
  together. If there is more than one match, all possible matches
  contribute one row each.

  If the remaining columns in the data frames have any common names,
  these have \code{".x"} and \code{".y"} appended to make the names
  of the result unique.
}

\value{
  A data frame. The rows are by default lexicographically sorted on the
  common columns, but are otherwise in the order in which they occurred
  in \code{x}. The columns are the common columns followed by the
  remaining columns in \code{x} and then those in \code{y}.  If the
  matching involved row names, an extra column \code{Row.names} is added
  at the left, and in all cases the result has no special row names.
}

\seealso{\code{\link{data.frame}}, \code{\link{by}}, \code{\link{cbind}}}

\examples{
authors <- data.frame(
    surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    retired = c("yes", rep("no", 4)))
books <- data.frame(
    name = c("Tukey", "Venables", "Tierney", "Ripley", "Ripley", "McNeil"),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
               "Interactive Data Analysis"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA))

merge(authors, books, by.x="surname", by.y="name")
merge(books, authors, by.x="name", by.y="surname")
}
\keyword{iteration}
