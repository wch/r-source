% File src/library/utils/man/memory.size.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{memory.size}
\alias{memory.size}
\alias{memory.limit}
\title{Report on Memory Allocation (no longer supported)}
\description{
  \describe{

  These functions are currently stubs which report infinity
  (\code{\link{Inf}}) with a warning on all platforms.  OS's own facilities
  can be used on some systems to limit memory allocation.
 }
}
\usage{
memory.size(max = FALSE)

memory.limit(size = NA)
}
\keyword{utilities}
\keyword{internal}
