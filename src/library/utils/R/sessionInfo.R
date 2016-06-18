#  File src/library/utils/R/sessionInfo.R
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

sessionInfo <- function(package = NULL)
{
    z <- list()
    z$R.version <- R.Version()
    z$platform <- z$R.version$platform
    if(nzchar(.Platform$r_arch))
        z$platform <- paste(z$platform, .Platform$r_arch, sep = "/")
    z$platform <- paste0(z$platform, " (", 8*.Machine$sizeof.pointer, "-bit)")
    z$locale <- Sys.getlocale()
    ## Now try to figure out the OS we are running under
    if (.Platform$OS.type == "windows") {
        z$running <- win.version()
    } else if (nzchar(Sys.which('uname'))) { ## we could try /usr/bin/uname
        uname <- system("uname -a", intern = TRUE)
        os <- sub(" .*", "", uname)
        z$running <-
            switch(os,
                   "Linux" = if(file.exists("/etc/os-release")) {
    ## http://www.freedesktop.org/software/systemd/man/os-release.html
                       tmp <- readLines("/etc/os-release")
                       t2 <- if (any(startsWith(tmp, "PRETTY_NAME=")))
                           sub("^PRETTY_NAME=", "",
                               grep("^PRETTY_NAME=", tmp, value = TRUE)[1L])
                       else if (any(startsWith(tmp, "NAME")))
                           ## could check for VERSION or VERSION_ID
                           sub("^NAME=", "",
                               grep("^NAME=", tmp, value = TRUE)[1L])
                       else "Linux (unknown distro)"
                       sub('"(.*)"', "\\1", t2)
                   } else if(file.exists("/etc/system-release")) {
                       ## RHEL-like
                       readLines("/etc/system-release")
                   },
                   "Darwin" = {
                       ver <- readLines("/System/Library/CoreServices/SystemVersion.plist")
                       ind <- grep("ProductUserVisibleVersion", ver)
                       ver <- ver[ind + 1L]
                       ver <- sub(".*<string>", "", ver)
                       ver <- sub("</string>$", "", ver)
                       ver1 <- strsplit(ver, ".", fixed = TRUE)[[1L]][2L]
                       sprintf("%s %s %s",
                               ifelse(as.numeric(ver1) < 12, "OS X", "macOS"),
                               switch(ver1,
                                      ## 10.6 is earliest that can be installed
                                      "6" = "Snow Leopard",
                                      "7" = "Lion",
                                      "8" = "Mountain Lion",
                                      "9" = "Mavericks",
                                      "10" = "Yosemite",
                                      "11" = "El Capitan",
                                      "12" = "Sierra",
                                      ""), ver)
                   },
                   "SunOS" = {
                       ver <- system('uname -r', intern = TRUE)
                       paste("Solaris",
                             strsplit(ver, ".", fixed = TRUE)[[1L]][2L])
                   },
                   uname)
    }

    if(is.null(package)){
        package <- grep("^package:", search(), value=TRUE)
        # weed out environments which are not really packages
        keep <- vapply(package, function(x) x == "package:base"
                       || !is.null(attr(as.environment(x), "path")), NA)
        package <- .rmpkg(package[keep])
    }

    ## no need to re-encode given what we extract.
    pkgDesc <- lapply(package, packageDescription, encoding = NA)
    if(length(package) == 0) stop("no valid packages were specified")
    basePkgs <- sapply(pkgDesc,
                       function(x) !is.null(x$Priority) && x$Priority=="base")
    ## Hmm, see tools:::.get_standard_package_names()$base
    z$basePkgs <- package[basePkgs]
    if(any(!basePkgs)){
        z$otherPkgs <- pkgDesc[!basePkgs]
        names(z$otherPkgs) <- package[!basePkgs]
    }
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% package)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        z$loadedOnly <- pkgDesc[loadedOnly]
    }
    class(z) <- "sessionInfo"
    z
}

print.sessionInfo <- function(x, locale = TRUE, ...)
{
    mkLabel <- function(L, n) {
        vers <- sapply(L[[n]], function(x) x[["Version"]])
        pkg <-  sapply(L[[n]], function(x) x[["Package"]])
        paste(pkg, vers, sep = "_")
    }

    cat(x$R.version$version.string, "\n", sep = "")
    cat("Platform: ", x$platform, "\n", sep = "")
    if (!is.null(x$running)) cat("Running under: ",  x$running, "\n", sep = "")
    cat("\n")
    if(locale) {
        cat("locale:\n")
	print(strsplit(x$locale, ";", fixed=TRUE)[[1]], quote=FALSE, ...)
        cat("\n")
    }
    cat("attached base packages:\n")
    print(x$basePkgs, quote=FALSE, ...)
    if(!is.null(x$otherPkgs)){
        cat("\nother attached packages:\n")
	print(mkLabel(x, "otherPkgs"), quote = FALSE, ...)
    }
    if(!is.null(x$loadedOnly)){
        cat("\nloaded via a namespace (and not attached):\n")
	print(mkLabel(x, "loadedOnly"), quote = FALSE, ...)
    }
    invisible(x)
}

toLatex.sessionInfo <- function(object, locale = TRUE, ...)
{
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("\\begin{itemize}\\raggedright",
           paste0("  \\item ", object$R.version$version.string,
                  ", \\verb|", object$R.version$platform, "|"))

    if(locale) {
        z <- c(z,
               paste0("  \\item Locale: \\verb|",
                      gsub(";","|, \\\\verb|", object$locale) , "|"))
    }

    z <- c(z,
           paste0("  \\item Running under: \\verb|",
                  gsub(";","|, \\\\verb|", object$running) , "|"))

    z <- c(z, strwrap(paste("\\item Base packages: ",
                         paste(sort(object$basePkgs), collapse = ", ")),
                      indent = 2, exdent = 4))

    if(length(opkgver)){
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z,
               strwrap(paste("  \\item Other packages: ",
                             paste(names(opkgver), opkgver, sep = "~",
                                   collapse = ", ")),
                       indent = 2, exdent = 4))
    }
    if(length(nspkgver)){
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z,
               strwrap(paste("  \\item Loaded via a namespace (and not attached): ",
                             paste(names(nspkgver), nspkgver, sep = "~",
                                   collapse = ", ")),
                       indent = 2, exdent = 4))
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "Latex"
    z
}
