#  File src/library/utils/R/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
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

.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    ## In R < 4.3.0, options(repos = c(CRAN = "@CRAN@")) was hard-wired.
    ## We now respect custom repositories files, which by default gives
    ## the old behavior, or initialize no repositories as a special case.
    if(identical(Sys.getenv("R_REPOSITORIES"), "NULL"))
        repos <- character()
    else {
        reposdf <- tryCatch(.get_repositories(), error = identity)
        if(inherits(reposdf, "error"))
            repos <- character()
        else {
            reposdf <- reposdf[reposdf$default,]
            repos <- reposdf$URL
            names(repos) <- row.names(reposdf)
        }
        ## Be extra careful (but see the comments in tools/R/utils.R).
        if(is.na(match("CRAN", names(repos))))
            repos <- c(CRAN = "@CRAN@", repos)
    }

    ## Set default options() related to functionality in 'utils' pkg
    op.utils <-
	list(help.try.all.packages = FALSE,
	     help.search.types = c("vignette", "demo", "help"),
             citation.bibtex.max = 1L, internet.info = 2L,
	     pkgType = if(.Platform$pkgType != "source") "both" else "source",
	     str = strOptions(), # -> ./str.R
	     demo.ask = "default", example.ask = "default",
	     HTTPUserAgent = defaultUserAgent(),
	     menu.graphics = TRUE, mailer = "mailto",
            repos = repos)
    if (.Platform$pkgType != "source")
        op.utils[["install.packages.compile.from.source"]] <-
            Sys.getenv("R_COMPILE_AND_INSTALL_PACKAGES", "interactive")

    extra <-
        if(.Platform$OS.type == "windows") {
            list(unzip = "internal",
                 editor = if(length(grep("Rgui", commandArgs(), TRUE))) "internal" else "notepad",
                 askYesNo = if (.Platform$GUI == "Rgui") askYesNoWinDialog
                 )
        } else
            list(unzip = Sys.getenv("R_UNZIPCMD"),
                 editor = Sys.getenv("EDITOR"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(.Options))
    if(any(toset)) options(op.utils[toset])

    ns <- environment(sys.function()) # the namespace
    assign("osVersion", .osVersion(), envir = ns)
}
