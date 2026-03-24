% File src/library/tools/man/checkMD5sums.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{checkSHA256sums}
\alias{checkSHA256sums}
\alias{verifySHA256signature}
\title{Check and Create SHA256 Checksum Files}
\description{
  \code{checkSHA256sums} checks the files against a file \file{SHA256}.

  \code{verifySHA256signature} implies \code{checkSHA256sums} but also
  verifies GnuPG detached signature in file \file{SHA256.sig}.
}
\usage{
checkSHA256sums(package, dir)
verifySHA256signature(package, dir)
}
\arguments{
  \item{package}{the name of an installed package}
  \item{dir}{the path to the top-level directory of an installed
    package.}
}
\details{
  The file \file{SHA256} which is a text file with the following
  entries separated by a single space each: hash (64-digit hexadecimal
  representation of the SHA256 hash), size of the file in bytes and
  the file path.

  If \code{dir} is missing, an installed package of name \code{package}
  is searched for.

  The private function \code{tools:::.installSHA256sums} is used to create
  \code{SHA256} files.

  \code{verifySHA256signature} relies on the GnuPG tool \code{gpg} to
  perform the verification. The environment variable \code{"GPG"} can
  override the default name of the command \code{"gpg"}.

  In order to ascertain the validity of a signature, it is necessary to
  obtain the public key of the signer and add it to a keyring for
  trust. \code{verifySHA256signature} will first look for any files in
  the \code{file.path(R.home("etc"), "keyrings")} directory with
  \code{".gpg"} extensions (if they exist) and any keyrings in
  the \file{~/.R/keyrings} user directory. If this yields no success
  then the user's default GnuPG keyring is consulted, including any
  local user settings.
}
\value{
  \code{checkSHA256sums} returns a logical, \code{NA} if there is no
  \file{SHA256} file to be checked.

  \code{verifySHA256signature} returns a logical, \code{NA} if there is
  no \file{SHA256.sig} file to be checked or if GnuPG is not available
  for checking. If \code{TRUE} or \code{FALSE} then additional
  information is supplied as a list in the \code{"result"}
  attribute. The attribute and well as its entries are considered
  optional, but most commonly populated values are:
  \item{fingerprint}{string, hexadecimal representation of the
    fingerprint of the public key that signed the signature}
  \item{keyid}{string, hexadecimal key id}
  \item{userid}{string, user id (typcially name and e-mail address of
    the signer)}
  \item{ts}{POSIXct, timestamp of the signature}
  \item{missing.pubkey}{logical, \code{TRUE} if the signature cannot be
    verified due to missing public key corresponding to the signature}

  OpenPGP does not mandate the presence of any other fields in
  signatures other than \code{ts} and \code{keyid}, but most modern
  signature include the \code{fingerprint} and \code{userid}.
}
\seealso{\code{\link{sha256sum}}, \code{\link{checkMD5sums}}}

\keyword{utilities}
