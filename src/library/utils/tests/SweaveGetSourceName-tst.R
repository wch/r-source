#  File src/library/utils/tests/SweaveGetSourceName-tst.R
#  Part of the R package, https://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## Testing SweaveGetSourceName
RNWFILE <- "SweaveGetSourceName.Rnw"

## Test using "R CMD Sweave"
out <- tools::Rcmd(c("Sweave", RNWFILE),
                   stdout = TRUE, stderr = TRUE)
file <- strsplit(grepv("^~~~source: ", out), ": ", fixed = TRUE)[[1]][2]
stopifnot(file == RNWFILE)

## Test using "R CMD Sweave" with options
out <- tools::Rcmd(c("Sweave", "--encoding=utf-8", RNWFILE),
                   stdout = TRUE, stderr = TRUE)
file <- strsplit(grepv("^~~~source: ", out), ": ", fixed = TRUE)[[1]][2]
stopifnot(file == RNWFILE)

## Test using "R -e 'Sweave(...)'"
out <- system2(file.path(R.home("bin"), "R"),
               c("-e", shQuote(paste0("Sweave(\"", RNWFILE, "\")"))),
               stdout = TRUE, stderr = TRUE)
file <- strsplit(grepv("^~~~source: ", out), ": ", fixed = TRUE)[[1]][2]
stopifnot(file == RNWFILE)

## Test using "R -e 'Sweave(...)'" with options
out <- system2(file.path(R.home("bin"), "R"),
               c("--no-echo",
                 "-e", shQuote(paste0("Sweave(\"", RNWFILE, "\")")),
                 "--no-restore"),
               stdout = TRUE, stderr = TRUE)
file <- strsplit(grepv("^~~~source: ", out), ": ", fixed = TRUE)[[1]][2]
stopifnot(file == RNWFILE)
