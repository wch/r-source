% File src/library/utils/man/help.request.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
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
             ccaddress = Sys.getenv("USER"),
             method = getOption("mailer"),
             address = "r-help@R-project.org",
             file = "R.help.request")
}
\arguments{
  \item{subject}{subject of the email.  Please do not use single quotes
    (\code{'}) in the subject!  Post separate help requests for multiple
    queries.}
  \item{ccaddress}{optional email address for copies (default is current
    user).  Use \code{ccaddress = FALSE} for no copies.}
  \item{method}{submission method: for Unix one of \code{"mailx"},
    \code{"gnudoit"}, \code{"none"} or \code{"ess"}; for Windows either
    \code{"none"} (default) or \code{"mailto"}.}
  \item{address}{recipient's email address.}
  \item{file}{file to use for setting up the email (or storing it when
    method is \code{"none"} or sending mail fails).}
}
\details{
  This function is not intended to replace the posting
  guide. Please read the guide before posting to R-help or using this
  function (see \url{http://www.r-project.org/posting-guide.html}).

  The \code{help.request} function:
  \itemize{
    \item asks whether the user has consulted relevant resources,
      stopping and opening the relevant url if a negative response if
      given.
    \item checks whether the current version of R is being used and
      whether the add-on packages are up-to-date, giving the option of
      updating where necessary.
    \item asks whether the user has prepared appropriate (minimal,
      reproducible, self-contained, commented) example code ready to
      paste into the post.
  }
  Once this checklist has been completed a template post is prepared
  including current session information.

  If method is \code{"none"} or \code{NULL}, then the default text
  editor is opened for the user to complete the post.  Which editor is
  used can be controlled using \code{\link{options}}, type
  \code{getOption("editor")} to see what editor is currently
  defined.  Please use the help pages of the respective editor for
  details of usage.  The report can then be copied to your favorite email
  program and sent to the r-help list.

  On Windows systems there is an experimental \code{"mailto"} option,
  which sends the template post to the system's default email program for
  the user to edit and send.

  On Unix systems there are three options for direct submission of the
  post. If the submission method is \code{"mailx"}, then the default
  editor is used to write the help request. After saving the help request
  (in the temporary file opened) and exiting the editor the report is
  mailed using a Unix command line mail utility such as \code{mailx}.  A
  copy of the mail is sent to the current user. If method is
  \code{"gnudoit"}, then an emacs mail buffer is opened and used for
  sending the email. If method is \code{"ess"} the body of the mail is
  simply sent to stdout.
}
\value{Nothing useful.}
\seealso{The posting guide
  (\url{http://www.r-project.org/posting-guide.html}),
  also \code{\link{sessionInfo}()} from which you may add
  to the help request.}
\author{Heather Turner, based on code and help page of
  \code{\link{bug.report}()}.}
\keyword{utilities}
\keyword{error}
