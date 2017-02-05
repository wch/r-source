% File src/library/tools/man/CRANtools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016 R Core Team
% Distributed under GPL 2 or later

\name{CRANtools}
\title{CRAN Package Repository Tools}
\alias{CRAN_package_db}
\alias{CRAN_check_results}
\alias{CRAN_check_details}
\alias{CRAN_memtest_notes}
\alias{summarize_CRAN_check_status}
\description{
  Tools for obtaining information about current packages in the CRAN
  package repository, and their check status.
}
\usage{
CRAN_package_db()

CRAN_check_results(flavors = NULL)
CRAN_check_details(flavors = NULL)
CRAN_memtest_notes()
summarize_CRAN_check_status(package,
                            results = NULL,
                            details = NULL,
                            mtnotes = NULL)
}
\arguments{
  \item{package}{a character vector of package names.}
  \item{flavors}{a character vector of CRAN check flavor names, or
    \code{NULL} (default), corresponding to all available flavors.}
  \item{results}{the return value of \code{CRAN_check_results()}
    (default), or a subset of this.}
  \item{details}{the return value of \code{CRAN_check_details()}
    (default), or a subset of this.}
  \item{mtnotes}{the return value of \code{CRAN_check_mtnotes()}
    (default), or a subset of this.}
}
\details{
  \code{CRAN_package_db()} returns a character frame most
  \file{DESCRIPTION} metadata for the current packages in the CRAN
  package repository, including in particular the Description and
  Maintainer information not provided by
  \code{utils::\link{available.packages}()}.

  \code{CRAN_check_results()} returns a data frame with the basic CRAN
  package check results including timings, with variables
  \code{Package}, \code{Flavor} and \code{Status} giving the package
  name, check flavor, and overall check status, respectively.

  \code{CRAN_check_details()} returns a data frame inheriting from
  \code{check_details} (which has useful \code{print} and \code{format}
  methods) with details on the check results, providing check name,
  status and output for every non-ok check (via variables \code{Check},
  \code{Status} and \code{Output}, respectively).  Packages with all-ok
  checks are indicated via a \code{*} \code{Check} wildcard name and OK
  \code{Status}.

  \code{CRAN_memtest_notes()} returns pointers to issues identified in
  the additional memory access check results made available from
  \url{https://www.stats.ox.ac.uk/pub/bdr/memtests/}, as a list which
  for every package with such issues provides a character matrix with
  columns \code{Test} and \code{Path}.
}
\examples{
## IGNORE_RDIFF_BEGIN
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
head(bad[sample(seq_len(NROW(bad)), NROW(bad)), ])

mtnotes <- CRAN_memtest_notes()
head(mtnotes)
## Show counts of issues according to test:
table(do.call(rbind, mtnotes)[, "Test"])

## Summarize CRAN check status for 10 random packages
## (reusing the information already read in):
pos <- sample(seq_len(NROW(pdb)), 10L)
summarize_CRAN_check_status(pdb[pos, "Package"],
                            results, details, mtnotes)
## IGNORE_RDIFF_END
}
