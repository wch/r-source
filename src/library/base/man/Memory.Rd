% File src/library/base/man/Memory.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Development Team
% Distributed under GPL 2 or later

\name{Memory}
\alias{Memory}
\alias{mem.limits}
\title{Memory Available for Data Storage}
\description{
  How \R manages its workspace.
  
  \code{mem.limits} is deprecated as of \R 2.14.0
}
\usage{
mem.limits(nsize = NA, vsize = NA)
}
\arguments{
  \item{vsize}{Heap memory in bytes.}
  \item{nsize}{Number of cons cells.}
}
\details{
  \R has a variable-sized workspace which is sized automatically.  These
  limits are mainly historical, and now used only exceptionally.
#ifdef windows
  
  On Windows the \option{--max-mem-size} option sets the maximum memory
  allocation: it has a minimum allowed value of 32M.  This is intended
  to catch attempts to allocate excessive amounts of memory which may
  cause other processes to run out of resources.  See also
  \code{\link{memory.limit}}.
#endif

  \R maintains separate areas for fixed and variable sized objects.  The
  first of these is allocated as an array of \emph{cons cells} (Lisp
  programmers will know what they are, others may think of them as the
  building blocks of the language itself, parse trees, etc.), and the
  second are thrown on a \emph{heap} of \sQuote{Vcells} of 8 bytes each.
  Each cons cell occupies 28 bytes on a 32-bit build of \R, (usually) 56
  bytes on a 64-bit build.
  
  The default values are (currently) an initial setting of 350k cons
  cells, 6Mb of vector heap: note that the areas are not actually
  allocated initially: rather these values are the sizes for triggering
  garbage collection.  Thereafter \R will grow or shrink the areas
  depending on usage, never decreasing below the initial values.

  You can find out the current memory consumption (the heap and cons
  cells used as numbers and megabytes) by typing \code{\link{gc}()} at the
  \R prompt.  Note that following \code{\link{gcinfo}(TRUE)}, automatic
  garbage collection always prints memory use statistics.
  
  The function \code{mem.limits} is deprecated: use \code{\link{gc}}
  instead, which reports the limits in the exceptional case that they are
  set.
  
  The command-line option \option{--max-ppsize} controls the maximum
  size of the pointer protection stack.  This defaults to 50000, but can
  be increased to allow deep recursion or large and complicated
  calculations to be done.  \emph{Note} that parts of the garbage
  collection process goes through the full reserved pointer protection
  stack and hence becomes slower when the size is increased.  Currently
  the maximum value accepted is 500000.
}

\value{
  \code{mem.limits()} returns a numeric vector giving the current
  settings of the maxima, possibly \code{NA} (for unlimited).
}

\seealso{
  \emph{An Introduction to R} for more command-line options.

  \code{\link{Memory-limits}} for the design limitations.

  \code{\link{gc}} for information on the garbage collector and total
  memory usage,  \code{\link{object.size}(a)} for the (approximate)
  size of \R object \code{a}.  \code{\link{memory.profile}} for
  profiling the usage of cons cells.
#ifdef windows

  \code{\link{memory.size}} to monitor total memory usage,
  \code{\link{memory.limit}} for the current limit.
#endif
}

\keyword{environment}
