#  File src/library/parallel/R/detectCores.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

## In part based on code in package multicore 0.1-6 by Simon Urbanek

detectCores <-
    if(.Platform$OS.type == "windows") {
        function(all.tests = FALSE, logical = TRUE) {
            ## result is # cores, logical processors.
            res <- .Call(C_ncpus, FALSE)
            ifelse(logical, res[2L], res[1L]);
        }
    } else {
        function(all.tests = FALSE, logical = TRUE) {
            ## Commoner OSes first
            systems <-
                list(linux =
                     if(logical) "grep processor /proc/cpuinfo 2>/dev/null | wc -l" else "cat /proc/cpuinfo | grep 'cpu cores'| uniq | cut -f2 -d:",
                     ## hw.physicalcpu is not documented for 10.9, but works
                     darwin = if(logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null",
                     solaris = if(logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l",
                     freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
                     openbsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
                     irix  = c("hinv | grep Processors | sed 's: .*::'", "hinv | grep '^Processor '| wc -l"))
            for (i in seq(systems))
                if(all.tests ||
		   length(grep(paste0("^", names(systems)[i]), R.version$os)))
                    for (cmd in systems[i]) {
                        a <- try(suppressWarnings(system(cmd, TRUE)),
                                 silent = TRUE)
                        if(inherits(a, "try-error")) next
                        a <- gsub("^ +","", a[1])
                        if (length(grep("^[1-9]", a))) return(as.integer(a))
                    }
            NA_integer_
        }
    }

## added in R 3.0.3
.check_ncores <- function(nc)
{
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && (chk != "false") && nc > 2L) {
        msg <- sprintf("%d simultaneous processes spawned", nc)
        if(chk == "warn") warning(msg, call. = FALSE, immediate. = TRUE)
        else stop(msg, call. = TRUE)
    }
}
