\name{summaryRprof}
\alias{summaryRprof}
\title{Summarise Output of R Sampling Profiler}
\description{
Summarise the output of the \code{\link{Rprof}} function to show the
amount of time used by different \R functions.
}
\usage{
summaryRprof(filename = "Rprof.out", chunksize = 5000,
              memory=c("none","both","tseries","stats"),
              index=2, diff=TRUE, exclude=NULL)
}
\arguments{
  \item{filename}{Name of a file produced by \code{Rprof()}}
  \item{chunksize}{Number of lines to read at a time}
  \item{memory}{Summaries for memory information. See Details below}
  \item{index}{How to summarize the stack trace for memory
    information. See Details below.}
  \item{diff}{If \code{TRUE} memory summaries use change in memory
    rather than current memory}
  \item{exclude}{Functions to exclude when summarizing the stack trace
    for memory summaries}
  
}
\details{
This function is an alternative to \code{R CMD Rprof}. It provides the
convenience of an all-\R implementation but will be slower for large
files.

As the profiling output file could be larger than available memory, it
is read in blocks of \code{chunksize} lines. Increasing \code{chunksize}
will make the function run faster if sufficient memory is available.

When called with \code{memory.profiling=TRUE}, the profiler writes
information on three aspects of memory use: vector memory in small
blocks on the R heap, vector memory in large blocks (from
\code{malloc}), memory in nodes on the R heap.  It also records the number of
calls to the internal function \code{duplicate} in the time
interval. \code{duplicate} is called by C code when arguments need to be
copied. Note that the profiler does not track which function actually
allocated the memory.

With \code{memory="both"} the change in total memory (truncated at zero)
is reported in addition to timing data. 

With \code{memory="tseries"} or \code{memory="stats"} the \code{index}
argument specifies how to summarize the stack trace. A positive number
specifies that many calls from the bottom of the stack; a negative
number specifies the number of calls from the top of the stack.  With
\code{memory="tseries"} the index is used to construct labels and may be
a vector to give multiple sets of labels. With \code{memory="stats"} the
index must be a single number and specifies how to aggregate the data to
the maximum and average of the memory statistics.  With both
\code{memory="tseries"} and \code{memory="stats"} the argument
\code{diff=TRUE} asks for summaries of the increase in memory use over
the sampling interval and \code{diff=FALSE} asks for the memory use at
the end of the interval.

}
\value{
If \code{memory="none"},a  list with components
  \item{by.self}{Timings sorted by \sQuote{self} time}
  \item{by.total}{Timings sorted by \sQuote{total} time}
  \item{sampling.time}{Total length of profiling run}

If \code{memory="both"} the same list but with memory consumption in Mb
in addition to the timings.  

If \code{memory="tseries"} a data frame giving memory statistics over
time

If \code{memory="stats"} a \code{by} object giving memory statistics by function.
}


\seealso{
  The chapter on \dQuote{Tidying and profiling R code} in
  \dQuote{Writing \R Extensions} (see the \file{doc/manual} subdirectory
  of the \R source tree).

  \code{\link{Rprof}}
  
  \code{\link{tracemem}} traces copying of an object via the C function
  \code{duplicate}.

  \code{\link{Rprofmem}} is a non-sampling memory use profiler.

  \url{http://developer.r-project.org/memory-profiling.html}
}
\examples{
\dontrun{
## Rprof() is not available on all platforms
Rprof(tmp <- tempfile())
example(glm)
Rprof()
summaryRprof(tmp)
unlink(tmp)
}
}
\keyword{utilities}
