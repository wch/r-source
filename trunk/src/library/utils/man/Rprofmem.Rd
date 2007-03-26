\name{Rprofmem}
\alias{Rprofmem}
\title{Enable Profiling of R's Memory Use}
\description{
 Enable or disable reporting of memory allocation in R.
}
\usage{
Rprofmem(filename = "Rprofmem.out", append = FALSE, threshold = 0)
}
\arguments{
  \item{filename}{The file to be used for recording the memory
    allocations. Set to \code{NULL} or \code{""} to disable reporting. }
  \item{append}{logical: should the file be over-written or appended to? }
  \item{threshold}{numeric: allocations on R's "large vector" heap
    larger than this number of bytes will be reported.
  }
}
\details{
  Enabling profiling automatically disables any existing profiling to
  another or the same file.

  Profiling writes the call stack to the specified file every time
  \code{malloc} is called to allocate a large vector object or to
  allocate a page of memory for small objects. The size of a page of
  memory and the size above which \code{malloc} is used for vectors are
  compile-time constants, by default 2000 and 128 bytes respectively.

  The profiler tracks allocations, some of which will be to previously
  used memory and will not increase the total memory use of R.
}
\note{
  The memory profiler slows down R even when not in use, and so is a
  compile-time option.  The memory profiler can be used at the same time
  as other R and C profilers.
  }
\value{
  None  
}

\seealso{
  The R sampling profiler, \code{\link{Rprof}} also collects
  memory information.

  \code{\link{tracemem}} traces duplications of specific objects.

  The "Writing R Extensions" manual section on "Tidying and profiling R code"
}
\examples{\dontrun{
## not supported unless R is compiled to support it.
Rprofmem("Rprofmem.out", threshold=1000)
example(glm)
Rprofmem(NULL)
noquote(readLines("Rprofmem.out", n=5))
}}
\keyword{utilities}
