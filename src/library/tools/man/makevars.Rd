% File src/library/tools/man/makevars.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016 R Core Team
% Distributed under GPL 2 or later

\name{makevars}
\alias{makevars_user}
\alias{makevars_site}
\title{User and Site Compilation Variables}
\usage{
makevars_user()
makevars_site()
}
\description{
  Determine the location of the user and site specific \file{Makevars}
  files for customizing package compilation.
}
\value{
  A character string with the path to the user or site specific
  \file{Makevars} file, or an empty character vector if there is no such
  file.
}
\details{
  Package maintainers can use these functions to employ user and site
  specific compilation settings also for compilations not using \R's
  mechanisms (in particular, custom compilations in subdirectories of
  \file{src}), e.g., by adding configure code calling \R with
  \code{cat(tools::makevars_user())} or
  \code{cat(tools::makevars_site())}, and if non-empty passing this with
  \option{-f} to custom Make invocations.
}
\seealso{
  Section \dQuote{Customizing package compilation} in the
  \dQuote{R Installation and Administration} manual.
}
%% \donttest as output is user/site-dependent
\examples{\donttest{
makevars_user()
makevars_site()
}
\dontshow{
checkMV <- function(r)
  stopifnot(is.character(r),
            length(r) == 0 || (length(r) == 1 && file.exists(r)))
checkMV(makevars_user())
checkMV(makevars_site())
}
}
\keyword{utilities}
