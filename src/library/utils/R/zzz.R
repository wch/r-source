#  File src/library/utils/R/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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
    ## Set default options() related to functionality in 'utils' pkg
    op <- options()
    op.utils <-
	list(help.try.all.packages = FALSE,
	     help.search.types = c("vignette", "demo", "help"),
             citation.bibtex.max = 1L, internet.info = 2L,
	     pkgType = if(.Platform$pkgType != "source") "both" else "source",
	     str = list(strict.width = "no", digits.d = 3L, vec.len = 4L),
	     demo.ask = "default", example.ask = "default",
	     HTTPUserAgent = defaultUserAgent(),
	     menu.graphics = TRUE, mailer = "mailto")
    if (.Platform$pkgType != "source")
        op.utils[["install.packages.compile.from.source"]] =
            Sys.getenv("R_COMPILE_AND_INSTALL_PACKAGES", "interactive")

    extra <-
        if(.Platform$OS.type == "windows") {
            list(unzip = "internal",
                 editor = if(length(grep("Rgui", commandArgs(), TRUE))) "internal" else "notepad",
                 repos = c(CRAN = "@CRAN@",
                           CRANextra = "http://www.stats.ox.ac.uk/pub/RWin")
                 )
        } else
            list(unzip = Sys.getenv("R_UNZIPCMD"),
                 editor = Sys.getenv("EDITOR"),
                 repos = c(CRAN = "@CRAN@"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}
