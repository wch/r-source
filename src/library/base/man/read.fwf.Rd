\name{read.fwf}
\title{Read Fixed Width Format Files}
\usage{
read.fwf(file, widths, sep=" ", as.is = FALSE,
         skip = 0, row.names, col.names)
}
\alias{read.fwf}
\arguments{
\item{file}{the name of the file which the data are to be read from.
  Each row of the table appears as one line of the file.}
 \item{widths}{integer vector, giving the widths of the fixed-width
   fields (of one line).}
 \item{sep}{character; the separator used internally; should be irrelevant.}
%%--   === and therefore  'sep' shouldn't be an argument of read.fwf(.) ?!
%%--  Martin M.
 \item{as.is}{see \code{\link{read.table}}.}
 \item{skip}{number of initial lines to skip; see \code{\link{read.table}}.}
 \item{row.names}{see \code{\link{read.table}}.}
 \item{col.names}{see \code{\link{read.table}}.}
}
\description{
  Read a ``table'' of \bold{f}ixed \bold{w}idth \bold{f}ormatted data
  into a \code{\link{data.frame}}.
}
\value{
  A \code{\link{data.frame}} as by \code{\link{read.table}}
  which is called internally.
}
\note{This function currently relies on the \code{Perl} script
  \file{\$R\_HOME/bin/fwf2table}.
  Therefore, it will only work in an environment with
  \code{Perl 5} installed, see \file{INSTALL} in the top-level directory
  of the sources.
}
\author{Kurt Hornik \email{hornik@ci.tuwien.ac.at}}
\seealso{\code{\link{scan}} and \code{\link{read.table}}.}
\examples{
ff <- tempfile()
cat(file=ff, "123456", "987654", sep="\n")
read.fwf(ff, width=c(1,2,3))	#> 1 23 456 \\ 9 87 654
unlink(ff)
}
\keyword{file}
