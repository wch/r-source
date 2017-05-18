% File src/library/tools/man/CRANtools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016-2017 R Core Team
% Distributed under GPL 2 or later

\name{CRANtools}
\title{CRAN Package Repository Tools}
\alias{CRAN_package_db}
\alias{CRAN_check_results}
\alias{CRAN_check_details}
\alias{CRAN_check_issues}
\alias{CRAN_memtest_notes}
\alias{summarize_CRAN_check_status}
\description{
  Tools for obtaining information about current packages in the
  \acronym{CRAN} package repository, and their check status.
}
\usage{
CRAN_package_db()

CRAN_check_results(flavors = NULL)
CRAN_check_details(flavors = NULL)
CRAN_check_issues()
summarize_CRAN_check_status(packages,
                            results = NULL,
                            details = NULL,
                            issues = NULL)
}
\arguments{
  \item{packages}{a character vector of package names.}
  \item{flavors}{a character vector of \acronym{CRAN} check flavor names, or
    \code{NULL} (default), corresponding to all available flavors.}
  \item{results}{the return value of \code{CRAN_check_results()}
    (default), or a subset of this.}
  \item{details}{the return value of \code{CRAN_check_details()}
    (default), or a subset of this.}
  \item{issues}{the return value of \code{CRAN_check_issues()}
    (default), or a subset of this.}
}
\details{
  \code{CRAN_package_db()} returns a character data frame with most
  \file{DESCRIPTION} metadata for the current packages in the CRAN
  package repository, including in particular the Description and
  Maintainer information not provided by
  \code{utils::\link{available.packages}()}.

  \code{CRAN_check_results()} returns a data frame with the basic
  \acronym{CRAN} package check results including timings, with columns
  \code{Package}, \code{Flavor} and \code{Status} giving the package
  name, check flavor, and overall check status, respectively.

  \code{CRAN_check_details()} returns a data frame inheriting from class
  \code{"check_details"} (which has useful \code{print} and
  \code{format} methods) with details on the check results, providing
  check name, status and output for every non-OK check (\emph{via}
  columns \code{Check}, \code{Status} and \code{Output}, respectively).
  Packages with all-OK checks are indicated via a \code{*} \code{Check}
  wildcard name and OK \code{Status}.

  \code{CRAN_check_issues()} returns a character frame with additional
  check issues (including the memory-access check results made available
  from \url{https://www.stats.ox.ac.uk/pub/bdr/memtests/} and the
  without-long-double check results from
  \url{https://www.stats.ox.ac.uk/pub/bdr/noLD}), as a character frame
  with variables \code{Package}, \code{Version}, \code{kind} (an
  identifier for the issue) and \code{href} (a URL with information on
  the issue).

  \code{CRAN_memtest_notes()} is now deprecated, with its functionality
  integrated into that of \code{CRAN_check_issues()}.
}

\value{
  See \sQuote{Details}.  Note that the results are collated on
  \acronym{CRAN}: currently this is done in a locale which sorts
  \code{aAbB} \dots.
}

\section{Which CRAN?}{
  The main functions access a \acronym{CRAN} mirror specified by the
  environment variable \env{R_CRAN_WEB}, defaulting to one specified in
  the \file{repositories} file (see \code{\link{setRepositories}}): if
  that specifies \code{@CRAN@} (the default) then
  \url{https://CRAN.R-project.org} is used.  (Note that
  \code{\link{options}("repos")} is not consulted.)

  Note that these access parts of \acronym{CRAN} under
  \file{web/contrib} and \file{web/packages} so if you have specified a
  mirror of just \file{src/contrib} for installing packages you will
  need to set \env{R_CRAN_WEB} to point to a full mirror.
}

%% Tested in tests/CRANtools.R
\examples{\donttest{
## This can be rather slow, especially with a non-local CRAN mirror
## and would fail (slowly) without Internet access in that case.

set.seed(11)  # but the packages chosen will change as soon as CRAN does.
pdb <- CRAN_package_db()
dim(pdb)
## DESCRIPTION fields included:
colnames(pdb)
## Summarize publication dates:
summary(as.Date(pdb$Published))
## Summarize numbers of packages according to maintainer:
summary(lengths(split(pdb$Package, pdb$Maintainer)))
## Packages with 'LASSO' in their Description:
pdb$Package[grepl("LASSO", pdb$Description)]

results <- CRAN_check_results()
## Available variables:
names(results)
## Tabulate overall check status according to flavor:
with(results, table(Flavor, Status))

details <- CRAN_check_details()
## Available variables:
names(details)
## Tabulate checks according to their status:
tab <- with(details, table(Check, Status))
## Inspect some installation problems:
bad <- subset(details,
              ((Check == "whether package can be installed") &
               (Status != "OK")))
## Show a random sample of up to 6
head(bad[sample(seq_len(NROW(bad)), NROW(bad)), ])

issues <- CRAN_check_issues()
head(issues)
## Show counts of issues according to kind:
table(issues[, "kind"])

## Summarize CRAN check status for 10 randomly-selected packages
## (reusing the information already read in):
pos <- sample(seq_len(NROW(pdb)), 10L)
summarize_CRAN_check_status(pdb[pos, "Package"],
                            results, details, issues)
}}
