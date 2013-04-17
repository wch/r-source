% File src/library/utils/man/create.post.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{create.post}
\alias{create.post}
\title{
  Ancillary Function for Preparing Emails and Postings
}
\description{
  An ancillary function used by \code{\link{bug.report}} and
  \code{\link{help.request}} to prepare emails for submission to package
  maintainers or to \R mailing lists.
}
\usage{
create.post(instructions = character(), description = "post",
            subject = "",
            method = getOption("mailer"),
            address = "the relevant mailing list",
            ccaddress = getOption("ccaddress", ""),
            filename = "R.post", info = character())
}
\arguments{
  \item{instructions}{Character vector of instructions to put at the top
    of the template email.}
  \item{description}{Character string: a description to be incorporated
    into messages.}
  \item{subject}{Subject of the email. Optional except for the
    \code{"mailx"} method.}
  \item{method}{Submission method, one of \code{"none"}, \code{"mailto"},
    \code{"gnudoit"}, \code{"ess"} or (Unix only) \code{"mailx"}.
    See \sQuote{Details}.}
  \item{address}{Recipient's email address, where applicable: for
    package bug reports sent by email this defaults to the address of
    the package maintainer (the first if more than one is listed).}
  \item{ccaddress}{Optional email address for copies with the
    \code{"mailx"} and \code{"mailto"} methods.
    Use \code{ccaddress = ""} for no copy.}
  \item{filename}{Filename to use for setting up the email (or storing it when
    method is \code{"none"} or sending mail fails).}
  \item{info}{character vector of information to include in the template
    email below the \sQuote{please do not edit the information below} line.}
}
\details{
  What this does depends on the \code{method}.  The function first
  creates a template email body.

  \describe{
    \item{\code{none}}{A file editor (see \code{\link{file.edit}}) is
      opened with instructions and the template email.  When this returns,
      the completed email is in file \code{file} ready to be read/pasted
      into an email program.}

    \item{\code{mailto}}{
      This opens the default email program with a template email
      (including address, Cc: address and subject) for you to edit and
      send.

      This works where default mailers are set up (usual on OS X and
      Windows, and where \command{xdg-open} is available and configured on
      other Unix-alikes: if that fails it tries the browser set by
      \env{R_BROWSER}).

      This is the \sQuote{factory-fresh} default method as from \R 2.13.0.
    }

    \item{\code{mailx}}{(Unix-alikes only.)
      A file editor (see \code{\link{file.edit}}) is
      opened with instructions and the template email.  When this
      returns, it is mailed using a Unix command line mail utility such
      as \code{mailx}, to the address (and optionally, the Cc: address)
      given.}

    \item{\code{gnudoit}}{
      An (X)emacs mail buffer is opened for the email to be edited and
      sent: this requires the \command{gnudoit} program to be
      available.  Currently \code{subject} is ignored.}

    \item{\code{ess}}{
      The body of the template email is sent to \code{stdout}.}
  }
}
\value{
  Invisible \code{NULL}.
}

\seealso{
  \code{\link{bug.report}}, \code{\link{help.request}}.
}

\keyword{utilities}
