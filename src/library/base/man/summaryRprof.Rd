\name{summaryRprof}
\alias{summaryRprof}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Summarise output of R profiler}
\description{
Summarise the output of the \code{\link{Rprof}} function to show the
amount of time used by different \R functions.
}
\usage{
summaryRprof(filename="Rprof.out", chunksize = 5000)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{filename}{Name of a file produced by \code{Rprof()}}
  \item{chunksize}{Number of lines to read at a time}
}
\details{
This function is an alternative to \code{R CMD Rprof}. It provides the
convenience of an all-\R implementation but will be slower for large
files.

As the profiling output file could be larger than available memory, it
is read in blocks of \code{chunksize} lines. Increasing \code{chunksize}
will make the function run faster if sufficient memory is available.
}
\value{
  A list with components
  \item{by.self}{Timings sorted by `self' time}
  \item{by.total}{Timings sorted by `total' time}
  \item{sampling.time}{Total length of profiling run}
}


\seealso{The chapter on ``Tidying and profiling R code'' in ``Writing \R
  Extensions'' (see the \file{doc/manual} subdirectory of the \R source
  tree).

\code{\link{Rprof}}}

\examples{
\dontrun{
## Rprof() is not available on all platforms
tmp<-tempfile()
Rprof(tmp<-tempfile())
example(glm)
Rprof()
summaryRprof(tmp)
unlink(tmp)
}
}
\keyword{utilities}% at least one, from doc/KEYWORDS
