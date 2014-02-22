#  File src/library/parallel/R/detectCores.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

## In part based on code in package multicore 0.1-6 by Simon Urbanek

detectCores <-
    if(.Platform$OS.type == "windows") {
        function(all.tests = FALSE, logical = TRUE) {
            ## result is # cores, logical processors.
            res <- .Call(C_ncpus, FALSE)
            ifelse(logical, res[2L], res[1L]);
        }
    } else {
        function(all.tests = FALSE, logical = FALSE) {
            systems <-
                list(darwin = "/usr/sbin/sysctl -n hw.ncpu 2>/dev/null",
                     freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
                     linux = "grep processor /proc/cpuinfo 2>/dev/null | wc -l",
                     irix  = c("hinv | grep Processors | sed 's: .*::'",
                     "hinv | grep '^Processor '| wc -l"),
                     solaris = if(logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l")
            for (i in seq(systems))
                if(all.tests ||
		   length(grep(paste0("^", names(systems)[i]), R.version$os)))
                    for (cmd in systems[i]) {
                        a <- gsub("^ +","", system(cmd, TRUE)[1])
                        if (length(grep("^[1-9]", a))) return(as.integer(a))
                    }
            NA_integer_
        }
    }

## added in R 3.0.3
.check_ncores <- function(nc)
{
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && nc > 2L) {
        msg <- sprintf("%d simultaneous processes spawned", nc)
        if(chk == "warn") warning(msg, call. = FALSE, immediate. = TRUE)
        else stop(msg, call. = TRUE)
    }
}
