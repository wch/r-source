\name{BATCH}
\alias{BATCH}
\title{Batch Execution of R}
\description{Run R non-interactively with input from a given file and
  place output (stdout/stderr) to another file.}
\usage{R CMD BATCH [options] infile [outfile]}
\arguments{
  \item{infile}{the name of a file with \R code to be executed.}
  \item{options}{a list of \R command line options, e.g., for setting the
    amount of memory available and controlling the load/save process.
    If \code{infile} starts with a \samp{-}, use \samp{--} as the final
    option.  The default options are \samp{--restore --save}.}
  \item{outfile}{the name of a file to which to write output.  If not
    given, the name used is that of \code{infile}, with a possible
    \file{.R} extension stripped, and \file{.Rout} appended.}
}
\details{
  Use \code{Rcmd BATCH --help} to be reminded of the usage.

  By default, the input commands are printed along with the output.  To
  suppress this behavior, add \code{options(echo = FALSE)} at the
  beginning of \code{infile}.

  The \code{infile} can have end of line marked by LF or CRLF (but not
  just CR), and files with a missing EOL mark are processed correctly.

  Using \code{R CMD BATCH} sets the GUI to \code{"none"}, so none of
  \code{\link{x11}}, \code{\link{jpeg}} and \code{\link{png}} are available.
}
\note{
  Unlike \code{Splus BATCH}, this does not run the \R process in the
  background.  In most shells,
  \code{R CMD BATCH [options] infile [outfile] &}
  will do so.
}
\keyword{utilities}
