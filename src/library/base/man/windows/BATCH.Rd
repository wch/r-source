\name{BATCH}
\title{Batch Execution of R}
\alias{BATCH}
\usage{Rcmd BATCH [options] infile [outfile]}
\description{Run \R non-interactively with input from \code{infile} and
  optionally send output (stdout/stderr) to another file.}
\arguments{
  \item{infile}{the name of a file with \R code to be executed.}
  \item{options}{a list of \R command line options, e.g., for setting the
    amount of memory available and controlling the load/save process.
    If \code{infile} starts with a `-', use \code{--} as the final 
    option.  The default options are \code{--restore --save}.}
  \item{outfile}{the name of a file to which to write output.  If not given,
    the name used is the one of \code{infile}, with a possible \file{.R}
    extension stripped, and \file{.Rout} appended.}
}
\details{
  By default, the input commands are printed along with the
  output.  To suppress this behavior, add \code{options(echo = FALSE)}
  at the beginning of \code{infile}.

  Files with an incomplete last line (no end of line mark) are now accepted.
}
\keyword{utilities}
    
