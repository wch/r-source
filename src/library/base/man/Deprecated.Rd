% File src/library/base/man/Deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{Deprecated}
\title{Marking Objects as Deprecated}
\alias{Deprecated}
\alias{.Deprecated}
\alias{deprecated}

\description{
  When an object is about to be removed from \R it is first deprecated and
  should include a call to \code{.Deprecated}.
}
\usage{
.Deprecated(new, package = NULL, msg,
            old = as.character(sys.call(sys.parent()))[1L])
}
\arguments{
  \item{new}{
    character string: A suggestion for a replacement function.
  }
  \item{package}{
    character string: The package to be used when suggesting where the
    deprecated function might be listed.
  }
  \item{msg}{
    character string: A message to be printed, if missing a default
    message is used.}
  \item{old}{character string specifying the function (default) or usage
    which is being deprecated.}
}
\details{
  \code{.Deprecated("\var{new name}")} is called from deprecated
  functions.  The original help page for these functions is often
  available at \code{help("\var{old}-deprecated")} (note the quotes).
  % FIXME: the above is not true any more; the old \alias is usually kept as-is.
  Deprecated functions should be listed in \code{help("\var{pkg}-deprecated")}
  for an appropriate \var{pkg}, including \pkg{base}.

  \code{.Deprecated} signals a warning of class \code{"deprecatedWarning"}
  with fields \code{old}, \code{new}, and \code{package}.
}
\seealso{
  \code{\link{Defunct}}

  \code{help("\link{base-deprecated}")} and so on which list the
  deprecated functions in the packages.
}
\keyword{documentation}
\keyword{utilities}
