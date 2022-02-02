% File src/library/base/man/numeric_version.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{numeric_version}
% User-level functions.
\alias{numeric_version}
\alias{as.numeric_version}
\alias{is.numeric_version}
\alias{package_version}
\alias{is.package_version}
\alias{as.package_version}
\alias{R_system_version}
\alias{getRversion}
% Methods.
\alias{[.numeric_version}
\alias{[<-.numeric_version}
\alias{[[.numeric_version}
\alias{[[<-.numeric_version}
\alias{Ops.numeric_version}
\alias{Summary.numeric_version}
\alias{anyNA.numeric_version}
\alias{as.character.numeric_version}
\alias{as.data.frame.numeric_version}
\alias{as.list.numeric_version}
\alias{c.numeric_version}
\alias{duplicated.numeric_version}
\alias{format.numeric_version}
\alias{is.na.numeric_version}
\alias{is.na<-.numeric_version}
\alias{print.numeric_version}
\alias{rep.numeric_version}
%\alias{sort.numeric_version}
\alias{unique.numeric_version}
\alias{xtfrm.numeric_version}
\alias{$.package_version}
% Internal.
\alias{.encode_numeric_version}
\alias{.decode_numeric_version}
\alias{.make_numeric_version}
\title{Numeric Versions}
\description{A simple S3 class for representing numeric versions
  including package versions, and associated methods.}
\usage{
numeric_version(x, strict = TRUE)
package_version(x, strict = TRUE)
R_system_version(x, strict = TRUE)
getRversion()
}
\arguments{
  \item{x}{a character vector with suitable numeric version strings (see
    \sQuote{Details}); for \code{package_version}, alternatively an R
    version object as obtained by \code{\link{R.version}}.}
  \item{strict}{a logical indicating whether invalid numeric versions
    should results in an error (default) or not.}
}
\details{
  Numeric versions are sequences of one or more non-negative integers,
  usually (e.g., in package \file{DESCRIPTION} files) represented as
  character strings with the elements of the sequence concatenated and
  separated by single \samp{.} or \samp{-} characters.  \R package
  versions consist of at least two such integers, an \R system version
  of exactly three (major, minor and patchlevel).

  Functions \code{numeric_version}, \code{package_version} and
  \code{R_system_version} create a representation from such strings (if
  suitable) which allows for coercion and testing, combination,
  comparison, summaries (min/max), inclusion in data frames,
  subscripting, and printing.  The classes can hold a vector of such
  representations.

  \code{getRversion} returns the version of the running \R as an R
  system version object.

  The \code{[[} operator extracts or replaces a single version.  To
  access the integers of a version use two indices: see the examples.
}
\seealso{
  \code{\link{compareVersion}};
  \code{\link{packageVersion}} for the version of a specific \R package.
  \code{\link{R.version}} etc for the version of \R (and the information
  underlying \code{getRversion()}).
}
\examples{
x <- package_version(c("1.2-4", "1.2-3", "2.1"))
x < "1.4-2.3"
c(min(x), max(x))
x[2, 2]
x$major
x$minor

if(getRversion() <= "2.5.0") { ## work around missing feature
  cat("Your version of R, ", as.character(getRversion()),
      ", is outdated.\n",
      "Now trying to work around that ...\n", sep = "")
}

x[[c(1, 3)]]  # '4' as a numeric vector, same as x[1, 3]
x[1, 3]      # 4 as an integer
x[[2, 3]] <- 0   # zero the patchlevel
x[[c(2, 3)]] <- 0 # same
x
x[[3]] <- "2.2.3"; x
x <- c(x, package_version("0.0"))
is.na(x)[4] <- TRUE
stopifnot(identical(is.na(x), c(rep(FALSE,3), TRUE)),
	  anyNA(x))
}
\keyword{utilities}
