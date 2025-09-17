% File src/library/utils/man/bug.report.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{bug.report}
\alias{bug.report}
\title{Send a Bug Report}
\usage{
bug.report(subject = "", address, file = "R.bug.report",
           package = NULL, lib.loc = NULL, \dots)
}
\arguments{
  \item{subject}{the subject of the email (if one is composed).}
  \item{address}{the recipient's email address, which is by default
    inferred from the \code{\link{packageDescription}}
    (see \sQuote{Details}).}
  \item{file}{filename to use (if needed) for setting up the email.}
  \item{package}{an optional character string naming a single package which is
    the subject of the bug report.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees in which to search for the package, or \code{NULL}.
    The default value of \code{NULL} corresponds to all libraries
    currently known.}
  \item{\dots}{additional named arguments such as \code{method} and
    \code{ccaddress} to pass to \code{\link{create.post}}.}
}
\description{
  Invokes an editor or email program to write a bug report or opens a
  web page for bug submission.  Some standard information on the current
  version and configuration of \R are generated automatically.
}
\details{
  If \code{package} is \code{NULL} or a base package, this opens the \R
  bug tracker at \url{https://bugs.R-project.org/}.

  If \code{package} is specified, it is assumed that the bug report is
  about that package, and parts of its \file{DESCRIPTION} file are added
  to the standard information.  If the package has a non-empty
  \code{BugReports} field in the \file{DESCRIPTION} file specifying the
  URL of a webpage, that URL will be opened using
  \code{\link{browseURL}}, otherwise an email directed to the package
  maintainer will be generated using \code{\link{create.post}}.  If
  there is any other form of \code{BugReports} field or a \code{Contact}
  field, this is examined as it may provide a preferred email address.
}
\value{Nothing useful.}
\seealso{
  \code{\link{help.request}} which you possibly should try
  \emph{before} \code{bug.report}.

  \code{\link{create.post}}, which handles emailing reports.

  \code{\link{sessionInfo}()} from which you may add to the bug report.

  Chapter \manual{R-FAQ}{R Bugs} for more information on when and how to
  report a bug in \R.
}
\keyword{utilities}
\keyword{error}
