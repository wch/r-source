\name{BATCH}
\title{Batch Exectution of R}
\alias{BATCH}
\usage{R BATCH [options] infile [outfile]}
\description{Run R non-interactively with input from \code{infile} and
  place output (stdout/stderr) to another file.}
\arguments{
  \item{infile}{the name of a file with R code to be executed.}
  \item{options}{a list of R command line options, e.g., for setting the
    amount of memory available and controlling the load/save process.
    If \code{infile} starts with a `-', use \code{--} as the final 
    option.}
  \item{outfile}{the name of a file to write output to.  If not given,
    the name used is the one of \code{infile}, with a possible `.R'
    extension stripped, and `.Rout' appended.}
}
\details{By default, the input commands are printed along with the
  output.  To suppress this behavior, add \code{options(echo = FALSE)}
  at the beginning of \code{infile}.}
