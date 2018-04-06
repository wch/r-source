% File src/library/base/man/validUTF8.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2015-2018 R Core Team
% Distributed under GPL 2 or later

\name{validUTF8}
\title{Check if a Character Vector is Validly Encoded}
\alias{validUTF8}
\alias{validEnc}
\description{
  Check if each element of a character vector is valid in its implied
  encoding.
}
\usage{
validUTF8(x)
validEnc(x)
}

\arguments{
  \item{x}{a character vector.}
}

\details{
  These use similar checks to those used by functions such as
  \code{\link{grep}}.

  \code{validUTF8} ignores any marked encoding (see
  \code{\link{Encoding}}) and so looks directly if the bytes in each
  string are valid UTF-8.

  \code{validEnc} regards character strings as validly encoded unless
  their encodings are marked as UTF-8 or they are unmarked and the \R
  session is in a UTF-8 or other multi-byte locale.  (The checks in
  other multi-byte locales depend on the OS and as with
  \code{\link{iconv}} not all invalid inputs may be detected.)
}

\note{
  It would be possible to check for the validity of character strings in
  a Latin-1 encoding, but extensions such as CP1252 are widely accepted
  as \sQuote{Latin-1} and 8-bit encodings rarely need to be checked for
  validity.
}

\value{
  A logical vector of the same length as \code{x}.   \code{NA} elements
  are regarded as validly encoded.
}

\examples{
x <-
  ## from example(text)
c("Jetz", "no", "chli", "z\xc3\xbcrit\xc3\xbc\xc3\xbctsch:",
  "(noch", "ein", "bi\xc3\x9fchen", "Z\xc3\xbc", "deutsch)",
   ## from a CRAN check log
   "\xfa\xb4\xbf\xbf\x9f")
validUTF8(x)
validEnc(x) # depends on the locale
Encoding(x) <-"UTF-8"
validEnc(x) # typically the last, x[10], is invalid

## Maybe advantageous to declare it "unknown":
G <- x ; Encoding(G[!validEnc(G)]) <- "unknown"
try( substr(x, 1,1) ) # gives 'invalid multibyte string' error
try( substr(G, 1,1) ) # works
nchar(G) # fine, too
## but it is not "more valid" typically:
all.equal(validEnc(x),
          validEnc(G)) # typically TRUE
}
