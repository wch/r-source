\name{Memory}
\alias{Memory}
\title{Memory Available for Data Storage}
\description{
  Use command line options to set the memory available for \R.
}
\usage{
#ifdef unix
R --vsize=v --nsize=n
#endif
#ifdef windows
Rgui --vsize=v --nsize=n
Rterm --vsize=v --nsize=n
#endif
}
\arguments{
  \item{v}{Use \code{v} bytes of heap memory}
  \item{n}{Use \code{n} cons cells.}
}
\details{
  \R (currently) uses a static memory model.  This means that when it
  starts up, it asks the operating system to reserve a fixed amount of
  memory for it.  The size of this chunk cannot be changed subsequently.
  Hence, it can happen that not enough memory was allocated, e.g., when
  trying to read large data sets into \R.

  In these cases, you should restart \R (after saving your current
  workspace) with more memory available, using the command line options
  \code{--nsize} and \code{--vsize}.  To understand these options, one
  needs to know that \R maintains separate areas for fixed and variable
  sized objects.  The first of these is allocated as an array of
  ``\emph{cons cells}'' (Lisp programmers will know what they are,
  others may think of them as the building blocks of the language
  itself, parse trees, etc.), and the second are thrown on a
  ``\emph{heap}'' of ``Vcells'' (see
 \code{\link{gc}()["Vcells","total"]}) of 8 bytes each.  Effectively,
  the input \code{v} is therefore truncated to the nearest multiple of
  8.

  The \code{--nsize} option can be used to specify the number of cons
  cells (each occupying 20 bytes on a 32-bit machine) which \R is to use
  (the default is 250000), and the \code{--vsize} option to specify the
  size of the vector heap in bytes (the default is 6 MB).  Both options
  must be integers or integers ending with \code{M}, \code{K}, or
  \code{k} meaning \emph{Mega} (\eqn{2^{20} = 1048576}), (computer)
  \emph{Kilo} (\eqn{2^{10} = 1024}), or regular \emph{kilo} (1000).
  (The minimum allowed values are 200000 and 2M.)

  E.g., to read in a table of 10000 observations on 40 numeric
  variables, \code{R --vsize=10M} should do; for sourcing a large file,
  you would use \code{R --nsize=500k}.

  Note that the information on where to find vectors and strings on the
  heap is stored using cons cells.  Thus, it may also be necessary to
  allocate more space for cons cells in order to perform computations
  with very ``large'' variable-size objects.

  You can find out the current memory consumption (the proportion of
  heap and cons cells used) by typing \code{\link{gc}()} at the \R
  prompt.  This may help you in finding out whether to increase
  \code{--vsize} or \code{--nsize}.  Note that following
  \code{\link{gcinfo}(TRUE)}, automatic garbage collection always prints
  memory use statistics.

  \R will tell you whether you ran out of cons or heap memory.

  The defaults for \code{--nsize} and \code{--vsize} can be changed by
  setting the environment variables \code{R_NSIZE} and \code{R_VSIZE}
  respectively, perhaps most conveniently in the file \file{.Renviron}
  or \file{~/.Renviron}.

  When using \code{\link{read.table}}, the memory requirements are in
  fact higher than anticipated, because the file is first read in as one
  long string which is then split again.  Use \code{\link{scan}} if
  possible in case you run out of memory when reading in a large table.
}
\seealso{
  \code{\link{gc}} for information on the garbage collector.
}
#ifdef windows
\note{
  When using the Rgui console it is simplest to make a shortcut and
  put these command-line flags at the end of the Target field.
}
#endif
\examples{
# Start R with 15MB of heap memory and 1 million cons cells
\dontrun{
## Unix
R --vsize=15M --nsize=1000k
}
}
\keyword{environment}
