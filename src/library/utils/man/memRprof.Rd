\name{memRprof}
\alias{memRprof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Memory-use profiling}
\description{
Summarizes memory use information from the output of \code{\link{Rprof}}
called with \code{memory.profiling=TRUE}
}
\usage{
memRprof(filename = "Rprof.out", chunksize = 5000, label=c(1,-1),
        aggregate = 0, diff = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{File written by the R profiler}
  \item{chunksize}{Number of lines of the file to read at a time }
  \item{label, aggregate}{see Details below }
  \item{diff}{Report changes in memory allocation rather than current allocation }
}
\details{
When called with \code{memory.profiling=TRUE}, the profiler writes
information on three aspects of memory use: vector memory in small
blocks on the R heap, vector memory in large blocks (from
\code{malloc}), memory in nodes on the R heap.  It also records the number of
calls to the internal function \code{duplicate} in the time
interval. \code{duplicate} is called by C code when arguments need to be
copied. Note that the profiler does not track which function actually
allocated the memory.

\code{memRprof} with \code{aggregate=0} reports these four numbers as
time series, which can be graphed with \code{\link{matplot}}. Each
element of \code{label} specifies a vector of labels: a positive number
labels by that many functions at the top of the R stack, a negative
number by that many functions at the bottom of the R stack. The default
is two sets of labels using the top and bottom function on the stack.
When interest is in the time series it is probably
useful to specify a longer sampling interval than the default to
\code{\link{Rprof}}.

When \code{aggregate} is non-zero the memory information is aggregated
over subsets of the set of functions in the call stack. When
\code{aggregate} is positive it indicates how many functions at the top
of the call stack to include, when negative it indicates the number of
functions at the bottom of the call stack to include.  The mean and
maximum vector memory and node counts are reported, and the mean and
total number of duplications.


When \code{diff=TRUE} the change in vector memory and node counts are
used instead of the current value (truncated below at zero). This may
give a better impression of memory allocation during a particular
function.  Neither \code{diff=TRUE} nor \code{diff=FALSE} accurately
accounts for the impact of garbage collection.


}
\value{
  If \code{aggregate=0}, a data frame with four time series of memory
  data, otherwise a \code{by} object with mean and maximum memory use,
  mean and total duplications.
  
}

\seealso{\code{\link{Rprof}}, \code{\link{summaryRprof}} for the profiler.

  When running R under Linux, Valgrind can be used to do similar
  profiling of C code.

  \code{\link{tracemem}} traces copying of an object via the C function
  \code{duplicate}.

  \url{http://developer.r-project.org/memory-profiling.html}
}
\examples{

}
\keyword{utilities}% at least one, from doc/KEYWORDS

