% File src/library/utils/man/summaryRprof.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{summaryRprof}
\alias{summaryRprof}
\title{Summarise Output of R Sampling Profiler}
\description{
Summarise the output of the \code{\link{Rprof}} function to show the
amount of time used by different \R functions.
}
\usage{
summaryRprof(filename = "Rprof.out", chunksize = 5000,
              memory = c("none", "both", "tseries", "stats"),
              lines = c("hide", "show", "both"),
              index = 2, diff = TRUE, exclude = NULL,
              basenames = 1)
}
\arguments{
  \item{filename}{Name of a file produced by \code{Rprof()}.}
  \item{chunksize}{Number of lines to read at a time.}
  \item{memory}{Summaries for memory information.  See \sQuote{Memory profiling} below.   Can be abbreviated.}
  \item{lines}{Summaries for line information.  See \sQuote{Line profiling} below.   Can be abbreviated.}
  \item{index}{How to summarize the stack trace for memory
    information.  See \sQuote{Details} below.}
  \item{diff}{If \code{TRUE} memory summaries use change in memory
    rather than current memory.}
  \item{exclude}{Functions to exclude when summarizing the stack trace
    for memory summaries.}
  \item{basenames}{Number of components of the path to filenames to display.}
}

\details{
  This function provides the analysis code for \code{\link{Rprof}} files
  used by \command{R CMD Rprof}.

  As the profiling output file could be larger than available memory, it
  is read in blocks of \code{chunksize} lines.  Increasing \code{chunksize}
  will make the function run faster if sufficient memory is available.
}

\section{Memory profiling}{
  Options other than \code{memory = "none"} apply only to files produced
  by \code{\link{Rprof}(memory.profiling =  TRUE)}.
  
  When called with \code{memory.profiling = TRUE}, the profiler writes
  information on three aspects of memory use: vector memory in small
  blocks on the R heap, vector memory in large blocks (from
  \code{malloc}), memory in nodes on the R heap.  It also records the number of
  calls to the internal function \code{duplicate} in the time
  interval.  \code{duplicate} is called by C code when arguments need to be
  copied.  Note that the profiler does not track which function actually
  allocated the memory.

  With \code{memory = "both"} the change in total memory (truncated at zero)
  is reported in addition to timing data.

  With \code{memory = "tseries"} or \code{memory = "stats"} the \code{index}
  argument specifies how to summarize the stack trace.  A positive number
  specifies that many calls from the bottom of the stack; a negative
  number specifies the number of calls from the top of the stack.  With
  \code{memory = "tseries"} the index is used to construct labels and may be
  a vector to give multiple sets of labels.  With \code{memory = "stats"} the
  index must be a single number and specifies how to aggregate the data to
  the maximum and average of the memory statistics.  With both
  \code{memory = "tseries"} and \code{memory = "stats"} the argument
  \code{diff = TRUE} asks for summaries of the increase in memory use over
  the sampling interval and \code{diff = FALSE} asks for the memory use at
  the end of the interval.
}

\section{Line profiling}{
  If the code being run has source reference information retained (via
  \code{keep.source = TRUE} in \code{\link{source}} or
  \code{KeepSource = TRUE} in a package \file{DESCRIPTION} file or
  some other way), then information about the origin of lines is
  recorded during profiling.  By default this is not displayed, but
  the \code{lines} parameter can enable the display.
  
  If \code{lines = "show"}, line locations will be used in preference
  to the usual function name information, and the results
  will be displayed ordered by location in addition to the other orderings.
  
  If \code{lines = "both"}, line locations will be mixed with function
  names in a combined display.  
}
  

\value{
  If \code{memory = "none"} and \code{lines = "hide"}, a list with components
  \item{by.self}{A data frame of timings sorted by \sQuote{self} time.}
  \item{by.total}{A data frame of timings sorted by \sQuote{total} time.}
  \item{sample.interval}{The sampling interval.}
  \item{sampling.time}{Total time of profiling run.}

  The first two components have columns \samp{self.time},
  \samp{self.pct}, \samp{total.time} and \samp{total.pct}, the times in
  seconds and percentages of the total time spent executing code in that
  function and code in that function or called from that function,
  respectively.
  
  If \code{lines = "show"}, an additional component is added to the list:
  \item{by.line}{A data frame of timings sorted by source location.}

  If \code{memory = "both"} the same list but with memory consumption in Mb
  in addition to the timings.

  If \code{memory = "tseries"} a data frame giving memory statistics over
  time.

  If \code{memory = "stats"} a \code{\link{by}} object giving memory statistics
  by function.
  
  Prior to \R 2.15.3 an error was thrown if no events were recorded: now
  zero-row data frames are returned.
}

\seealso{
  The chapter on \dQuote{Tidying and profiling R code} in
  \dQuote{Writing \R Extensions} (see the \file{doc/manual} subdirectory
  of the \R source tree).

  \code{\link{Rprof}}

  \code{\link{tracemem}} traces copying of an object via the C function
  \code{duplicate}.

  \code{\link{Rprofmem}} is a non-sampling memory-use profiler.

  \url{https://developer.r-project.org/memory-profiling.html}
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
