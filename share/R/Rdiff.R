#  File share/R/Rdiff.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

## usage Rscript --vanilla --default-packages=NULL \
## ${R_SHARE_DIR}/R/Rdiff.R "$1" "$2" $3

args <- commandArgs(TRUE)

exitstatus <- as.integer(args[3])
if(is.na(existatatus)) exitstatus <- 0L

left <- args[1]
if(left == "-") left <- "stdin"
status <- tools::Rdiff(left, args[2], TRUE)
if(status) status <- exitstatus
q("no", status = status, runLast = FALSE)
