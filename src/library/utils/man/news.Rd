% File src/library/utils/man/news.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{news}
\alias{news}
\title{Build and Query R or Package News Information}
\description{
  Build and query the news for R or add-on packages.
}
\usage{
news(query, package = "R", lib.loc = NULL, format = NULL,
     reader = NULL, db = NULL)
}
\arguments{
  \item{query}{an expression for selecting news entries}
  \item{package}{a character string giving the name of an installed
    add-on package, or \code{"R"}.}
  \item{lib.loc}{a character vector of directory names of R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
  \item{format}{Not yet used.}
  \item{reader}{Not yet used.}
  \item{db}{a news db obtained from \code{news()}.}
}
\value{
  A data frame inheriting from class \code{"news_db"}.
}
\details{
  If \code{package} is \code{"R"} (default), a news db is built with the
  news since the 3.0.0 release of R (corresponding to R's top-level
  \file{NEWS} file).  Otherwise, if the given add-on package can be
  found in the given libraries, it is attempted to read its news in
  structured form from files \file{inst/NEWS.Rd}, \file{NEWS} or
  \file{inst/NEWS} (in that order).

  File \file{inst/NEWS.Rd} should be an Rd file given the entries as Rd
  \verb{\itemize} lists, grouped according to version using
  \verb{section} elements with names starting with a suitable prefix
  (e.g, \dQuote{Changes in version} followed by a space and the version
  number, and optionally followed by a space and a parenthesized ISO
  8601 (\%Y-\%m-\%d, see \code{\link{strptime}}) format date, and
  possibly further grouped according to categories using
  \verb{\subsection} elements named as the categories.

  The plain text \file{NEWS} files in add-on packages use a variety of
  different formats; the default news reader should be capable to
  extract individual news entries from a majority of packages from the
  standard repositories, which use (slight variations of) the following
  format:

  \itemize{
    \item Entries are grouped according to version, with version header
    \dQuote{Changes in version} at the beginning of a line, followed by
    a version number, optionally followed by an ISO 8601 format date,
    possibly parenthesized.
    \item Entries may be grouped according to category, with a category
    header (different from a version header) starting at the beginning
    of a line.
    \item Entries are written as itemize-type lists, using one of
    \samp{o}, \samp{*}, \samp{-} or \samp{+} as item tag.  Entries must
    be indented, and ideally use a common indentation for the item
    texts.
  }

  Additional formats and readers may be supported in the future.

  Package \pkg{tools} provides an (internal) utility function
  \code{news2Rd} to convert plain text \file{NEWS} files to Rd.  For
  \file{NEWS} files in a format which can successfully be handled by the
  default reader, package maintainers can use \code{tools:::news2Rd(dir,
    "NEWS.Rd")}, possibly with additional argument \code{codify = TRUE},
  with \code{dir} a character string specifying the path to a package's
  root directory.  Upon success, the \file{NEWS.Rd} file can further be
  improved and then be moved to the \file{inst} subdirectory of the
  package source directory.

  The news db built is a character data frame inheriting from
  \code{"news_db"} with variables \code{Version}, \code{Category},
  \code{Date} and \code{Text}, where the last contains the entry texts
  read, and the other variables may be \code{NA} if they were missing or
  could not be determined.

  Using \code{query}, one can select news entries from the db.  If
  missing or \code{NULL}, the complete db is returned.  Otherwise,
  \code{query} should be an expression involving (a subset of) the
  variables \code{Version}, \code{Category}, \code{Date} and
  \code{Text}, and when evaluated within the db returning a logical
  vector with length the number of entries in the db.  The entries for
  which evaluation gave \code{TRUE} are selected.  When evaluating,
  \code{Version} and \code{Date} are coerced to
  \code{\link{numeric_version}} and \code{\link{Date}} objects,
  respectively, so that the comparison operators for these classes can
  be employed.
}

\examples{
## Build a db of all R news entries.
db <- news()
\dontshow{
  vv <- capture.output(db)  # without an error
}
\donttest{% <- is not sensibly diff-ed against previous versions
## Bug fixes with PR number in 3.0.1.
news(Version == "3.0.1" & grepl("^BUG", Category) & grepl("PR#", Text),
     db = db)
## Which categories have been in use? % R-core maybe should standardize a bit more
sort(table(db[, "Category"]), decreasing = TRUE)
## Entries with version >= 3.0.0 (including "3.0.0 patched"):
table(news(Version >= "3.0.0", db = db)$Version)
}}
