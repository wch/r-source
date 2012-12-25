% File src/library/utils/man/help.request.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-11 R Core Team
% Distributed under GPL 2 or later

\name{help.request}
\alias{help.request}
\title{Send a Post to R-help}
\description{
  Prompts the user to check they have done all that is expected of them
  before sending a post to the R-help mailing list, provides a template
  for the post with session information included and optionally sends
  the email (on Unix systems).
}
\usage{
help.request(subject = "",
             address = "r-help@R-project.org",
             file = "R.help.request", \dots)
}
\arguments{
  \item{subject}{subject of the email.  Please do not use single quotes
    (\kbd{'}) in the subject!  Post separate help requests for multiple
    queries.}
  \item{address}{recipient's email address.}
  \item{file}{filename to use (if needed) for setting up the email.}
  \item{\dots}{additional named arguments such as \code{method} and
    \code{ccaddress} to pass to \code{\link{create.post}}.}
}
\details{
  This function is not intended to replace the posting
  guide.  Please read the guide before posting to R-help or using this
  function (see \url{http://www.r-project.org/posting-guide.html}).

  The \code{help.request} function:
  \itemize{
    \item asks whether the user has consulted relevant resources,
      stopping and opening the relevant URL if a negative response if
      given.
    \item checks whether the current version of \R is being used and
      whether the add-on packages are up-to-date, giving the option of
      updating where necessary.
    \item asks whether the user has prepared appropriate (minimal,
      reproducible, self-contained, commented) example code ready to
      paste into the post.
  }
  Once this checklist has been completed a template post is prepared
  including current session information, and passed to
  \code{\link{create.post}}.
}
\value{
  Nothing useful.
}
\seealso{The posting guide (\url{http://www.r-project.org/posting-guide.html}),
  also \code{\link{sessionInfo}()} from which you may add to the help request.

  \code{\link{create.post}}.
}
\author{Heather Turner, based on the then current code and help page of
  \code{\link{bug.report}()}.}
\keyword{utilities}
\keyword{error}
