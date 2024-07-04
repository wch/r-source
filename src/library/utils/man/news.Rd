% File src/library/utils/man/news.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
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
  \item{query}{an optional expression for selecting news entries.}
  \item{package}{a character string giving the name of an installed
    add-on package, or \code{"R"} or \code{"R-3"} or \code{"R-2"}.}
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
  character variables \code{Version}, \code{Category}, \code{Date},
  \code{Text} and \code{HTML}, where the last two each contain the
  entry texts read (in plain-text and HTML format, respectively), and
  the other variables may be \code{NA} if they were missing or could not
  be determined. The data frame has
  \code{\link{attributes}} \code{"package"} (and \code{"subset"} if the
  \code{query} lead to proper subsetting).
}
\details{
  If \code{package} is \code{"R"} (default), a news db is built with the
  news since the 4.0.0 release of \R, corresponding to the
  \file{NEWS} file in the \code{R.home("doc")} directory.
  \code{"R-3"} or \code{"R-2"} give the news for \R 3.x.y
  or \R 2.x.y respectively.  Otherwise, if the given add-on package can
  be found in the given libraries, it is attempted to read its news in
  structured form from files \file{inst/NEWS.Rd}, \file{NEWS.md} (since
  \R version 3.6.0, needs packages \CRANpkg{commonmark} and
  \CRANpkg{xml2} to be available), \file{NEWS} or \file{inst/NEWS} (in
  that order). See section \sQuote{NEWS Formats} for the file
  specifications.

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

\section{NEWS Formats}{
\subsection{\file{inst/NEWS.Rd}}{
  File \file{inst/NEWS.Rd} should be an Rd file given the entries as Rd
  \verb{\itemize} lists, grouped according to version using
  \verb{\section} elements. Section titles start with a suitable prefix
  followed by a space and the version
  number, and optionally end with a (parenthesized) ISO
  8601 (\%Y-\%m-\%d, see \code{\link{strptime}}) format date
  (optionally including a note), for example:\preformatted{
    \section{Changes in version 2.0 (2020-02-02, <note>)}{
      \itemize{
        \item ....
      }
    }
  }
  The entries can be further grouped according to categories using
  \verb{\subsection} elements named as the categories.
  The \file{NEWS.Rd} file is assumed to be UTF-8-encoded (but an
  included \verb{\encoding} specification takes precedence).
}

\subsection{\file{NEWS.md}}{
  File \file{NEWS.md} should contain the news in Markdown (following the
  \I{CommonMark} (\url{https://commonmark.org/}) specification), with the
  primary heading level giving the version number after a prefix
  followed by a space, and optionally followed by a space and a
  parenthesized ISO 8601 format date.  Where available, secondary
  headings are taken to indicate categories.  To accommodate for common
  practice, news entries are only split down to the category level.
}

\subsection{\file{NEWS}}{
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

  Package \pkg{tools} provides an (internal) utility function
  \code{news2Rd} to convert plain text \file{NEWS} files to Rd.  For
  \file{NEWS} files in a format which can successfully be handled by the
  default reader, package maintainers can use
  \code{tools:::news2Rd(dir, "NEWS.Rd")},
  possibly with additional argument \code{codify = TRUE},
  with \code{dir} a character string specifying the path to a package's
  root directory.  Upon success, the \file{NEWS.Rd} file can further be
  improved and then be moved to the \file{inst} subdirectory of the
  package source directory.
}

  Additional formats and readers may be supported in the future.
}

\examples{
## Build a db of all R news entries.
db <- news()
\dontshow{
  vv <- capture.output(print(db, doBrowse=FALSE))  # without an error
  stopifnot(is.character(vv), length(vv) >= 3) # was wrong (for weeks during devel.)
  html <- tools::toHTML(subset(db, is.na(Version))) # emulate doBrowse with query
}
## Bug fixes with PR number in 4.0.0.
db4 <- news(Version == "4.0.0" & grepl("^BUG", Category) & grepl("PR#", Text),
            db = db)
nrow(db4)
\dontshow{stopifnot(is.null(attr(db4, "bad")) && nrow(db4) == 25 )}
## print db4 to show in an HTML browser.

## News from a date range ('Matrix' is there in a regular R installation):
if(length(iM <- find.package("Matrix", quiet = TRUE)) && nzchar(iM)) {
   dM <- news(package="Matrix")
   stopifnot(identical(dM, news(db=dM)))
   dM2014 <- news("2014-01-01" <= Date & Date <= "2014-12-31", db = dM)
   stopifnot(paste0("1.1-", 2:4) \%in\% dM2014[,"Version"])
}

## Which categories have been in use? % R-core maybe should standardize a bit more
sort(table(db[, "Category"]), decreasing = TRUE)
## Entries with version >= 4.0.0
table(news(Version >= "4.0.0", db = db)$Version)

\donttest{
## do the same for R 3.x.y, more slowly
db3 <- news(package = "R-3")
sort(table(db3[, "Category"]), decreasing = TRUE)
## Entries with version >= 3.6.0
table(news(Version >= "3.6.0", db = db3)$Version)
}
}
