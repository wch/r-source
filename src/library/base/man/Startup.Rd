\name{Startup}
\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

.Rprofile <startup file>
}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
\alias{.First}
\description{
    In \R, the startup mechanism is as follows.  \R first searches for
    the site-wide startup profile unless the command line option
    \code{--no-site-file} was given.  The name of this file is taken
    from the value of the \file{R\_PROFILE} (\code{RPROFILE} as used by
    older versions is now deprecated) environment variable.  If that
    variable is unset, the default is \file{\$R\_HOME/etc/Rprofile}.

    Then, unless \code{--no-init-file} was given, \R searches for a file
    called \file{.Rprofile} in the current directory or in the user's
    home directory (in that order) and sources it.

    It also loads a saved image from \file{.RData} in case there is one
    (unless \code{--no-restore} was specified).

    Finally, if a function \code{.First} exists, it is executed as
    \code{.First()}.

    The functions \code{.First} and \code{\link{.Last}} can be defined
    in the appropriate startup profiles or reside in \file{.RData}.
}
\seealso{
    \code{\link{.Last}} for final actions before termination.
}
\keyword{environment}
