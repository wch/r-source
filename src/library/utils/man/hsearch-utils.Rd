% File src/library/utils/man/hsearch-utils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
% Distributed under GPL 2 or later

\name{hsearch-utils}
\alias{hsearch_db}
\alias{hsearch_db_concepts}
\alias{hsearch_db_keywords}
\title{Help Search Utilities}
\description{
  Utilities for searching the help system.
}
\usage{
hsearch_db(package = NULL, lib.loc = NULL,
           types = getOption("help.search.types"), 
           verbose = getOption("verbose"),
           rebuild = FALSE, use_UTF8 = FALSE)
hsearch_db_concepts(db = hsearch_db())
hsearch_db_keywords(db = hsearch_db())
}
\arguments{
   \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which case \emph{all} available
    packages in the library trees specified by \code{lib.loc} are
    searched.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{types}{a character vector listing the types of documentation
    to search.
    See \code{\link{help.search}} for details.}
  \item{verbose}{a logical controlling the verbosity of building the
    help search database.
    See \code{\link{help.search}} for details.}
  \item{rebuild}{a logical indicating whether the help search database
    should be rebuilt.
    See \code{\link{help.search}} for details.}
  \item{use_UTF8}{logical: should results be given in UTF-8 encoding?}
  \item{db}{a help search database as obtained by calls to
    \code{hsearch_db()}.}
}
\details{
  \code{hsearch_db()} builds and caches the help search database for
  subsequent use by \code{\link{help.search}}.  (In fact, re-builds only
  when forced (\code{rebuild = TRUE}) or \dQuote{necessary}.)

  The format of the help search database is still experimental, and may
  change in future versions.  Currently, it consists of four tables: one
  with base information about all documentation objects found, including
  their names and titles and unique ids; three more tables contain the
  individual aliases, concepts and keywords together with the ids of the
  documentation objects they belong to.  Separating out the latter three
  tables accounts for the fact that a single documentation object may
  provide several of these entries, and allows for efficient searching.

  See the details in \code{\link{help.search}} for how searchable
  entries are interpreted according to help type.

  \code{hsearch_db_concepts()} and \code{hsearch_db_keywords()} extract
  all concepts or keywords, respectively, from a help search database,
  and return these in a data frame together with their total frequencies
  and the numbers of packages they are used in, with entries sorted in
  decreasing total frequency.
}
\examples{
db <- hsearch_db()
## Total numbers of documentation objects, aliases, keywords and
## concepts (using the current format):
sapply(db, NROW)
## Can also be obtained from print method:
db
## 10 most frequent concepts:
head(hsearch_db_concepts(), 10)
## 10 most frequent keywords:
head(hsearch_db_keywords(), 10)
}
\keyword{documentation}
