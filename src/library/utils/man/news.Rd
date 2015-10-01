% File src/library/utils/man/news.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{news}
\title{Build and Query R or Package News Information}
\alias{news}
\alias{print.news_db}
\description{
  Build and query the news data base for \R or add-on packages.
}
\usage{
news(query, package = "R", lib.loc = NULL, format = NULL,
     reader = NULL, db = NULL)

\S3method{print}{news_db}(x, doBrowse = interactive(),
      browser = getOption("browser"), \dots)
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
  \item{db, x}{a news db obtained from \code{news()}.}

  \item{doBrowse}{logical specifying that the news should be opened in
    the browser (by \code{\link{browseURL}}, accessible as via
    \code{\link{help.start}}) instead of printed to the console.}
  \item{browser}{the browser to be used, see \code{\link{browseURL}}.}
  \item{\dots}{potentially further arguments passed to \code{print()}.}
}
\value{
  A data frame inheriting from class \code{"news_db"}, with
  \code{\link{attributes}} \code{"package"} (and \code{"subset"} if the
    \code{query} lead to proper subsetting).
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
  (e.g.\sspace{}\dQuote{Changes in version} followed by a space and the version
  number, and optionally followed by a space and a parenthesized ISO
  8601 (\%Y-\%m-\%d, see \code{\link{strptime}}) format date, and
  possibly further grouped according to categories using
  \verb{\subsection} elements named as the categories.
  At the very end of \verb{\section{..}}, the date may also be specified
  as \verb{(\%Y-\%m-\%d, <note>)}, i.e., including parentheses.

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
  vv <- capture.output(print(db, doBrowse=FALSE))  # without an error
  stopifnot(is.character(vv), length(vv) >= 3) # was wrong (for weeks during devel.)
}
## Bug fixes with PR number in 3.0.1.
db3 <- news(Version == "3.0.1" & grepl("^BUG", Category) & grepl("PR#", Text),
            db = db)
\dontshow{stopifnot( !any(attr(db3,"bad")) && nrow(db3) == 12 )}

## News from a date range ('Matrix' is there in a regular R installation):
if(length(iM <- find.package("Matrix", quiet=TRUE)) && nzchar(iM)) {
   dM <- news(package="Matrix")
   stopifnot(identical(dM, news(db=dM)))
   dM2014 <- news("2014-01-01" <= Date & Date <= "2014-12-31", db = dM)
   stopifnot(paste0("1.1-", 2:4) \%in\% dM2014[,"Version"])
}
\donttest{% <- is not sensibly diff-ed against previous versions
## Which categories have been in use? % R-core maybe should standardize a bit more
sort(table(db[, "Category"]), decreasing = TRUE)
## Entries with version >= 3.0.0 (including "3.0.0 patched"):
table(news(Version >= "3.0.0", db = db)$Version)
}}
