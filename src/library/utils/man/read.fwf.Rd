\name{read.fwf}
\alias{read.fwf}
\title{Read Fixed Width Format Files}
\description{
  Read a \dQuote{table} of \bold{f}ixed \bold{w}idth \bold{f}ormatted
  data into a \code{\link{data.frame}}.
}
\usage{
read.fwf(file, widths, header = FALSE, sep = "\t", as.is = FALSE,
         skip = 0, row.names, col.names, n = -1, \dots)
}
\arguments{
  \item{file}{
    the name of the file which the data are to be read from.

    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and if so closed at the end of the
    function call.
  }
  \item{widths}{integer vector, giving the widths of the fixed-width
    fields (of one line).}
  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.}
  \item{sep}{character; the separator used internally; should be a
    character that does not occur in the file.}
  \item{as.is}{see \code{\link{read.table}}.}
  \item{skip}{number of initial lines to skip; see
    \code{\link{read.table}}.}
  \item{row.names}{see \code{\link{read.table}}.}
  \item{col.names}{see \code{\link{read.table}}.}
  \item{n}{the maximum number of records (lines) to be read, defaulting
    to no limit.}
  \item{\dots}{further arguments to be passed to
    \code{\link{read.table}}.}
}
\value{
  A \code{\link{data.frame}} as produced by \code{\link{read.table}}
  which is called internally.
}
\details{
  Fields that are of zero-width or are wholly beyond the end of the line
  in \code{file} are replaced by \code{NA}.
}
\author{
  Brian Ripley for \R version: original \code{Perl} by Kurt Hornik.
}
\seealso{\code{\link{scan}} and \code{\link{read.table}}.}
\examples{
ff <- tempfile()
cat(file=ff, "123456", "987654", sep="\n")
read.fwf(ff, width=c(1,2,3))    #> 1 23 456 \\ 9 87 654
unlink(ff)
cat(file=ff, "123", "987654", sep="\n")
read.fwf(ff, width=c(1,0, 2,3))    #> 1 NA 23 NA \\ 9 NA 87 654
unlink(ff)
}
\keyword{file}
\keyword{connection}
