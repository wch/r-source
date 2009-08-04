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
  An data frame inheriting from class \code{"news_db"}.
}
\details{
  If \code{package} is \code{"R"} (default), \code{\link{readNEWS}} in
  package \pkg{tools} is used to build a news db from the R \file{NEWS}
  file.  Otherwise, if the given add-on package can be found in the
  given libraries and has a \file{NEWS} file, it is attempted to read
  the package news in structured form.  The \file{NEWS} files in add-on
  packages use a variety of different formats; the default news reader
  should be capable to extract individual news entries from a majority
  of packages from the standard repositories, which use (slight
  variations of) the following format:

  \itemize{
    \item Entries are grouped according to version, with version header
    \samp{Changes in version} at the beginning of a line, followed by a
    version number, optionally followed by an ISO 8601 (\%Y-\%m-\%d, see
    \code{\link{strptime}}) format date, possibly parenthesized.
    \item Entries may be grouped according to category, with a category
    header (different from a version header) starting at the beginning
    of a line.
    \item Entries are written as itemize-type lists, using one of
    \samp{o}, \samp{*}, \samp{-} or \samp{+} as item tag.  Entries must
    be indented, and ideally use a common indentation for the item
    texts.
  }

  Additional formats and readers may be supported in the future.

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
## Bug fixes with PR number in 2.9.0.
news(Version == "2.9.0" & grepl("^BUG", Category) & grepl("PR#", Text),
     db = db)
## Entries with version >= 2.8.1 (including "2.8.1 patched"):
table(news(Version >= "2.8.1", db = db)$Version)
}
