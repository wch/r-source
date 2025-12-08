#  File src/library/tools/R/pkg2HTML.R
#
#  Copyright (C) 2023-2024 The R Core Team
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


## Experimental interface to convert package source directory into
## single-page manual

## FIXME: Rd2HTML(standalone=FALSE) needs to include mathjaxr
## detection results in the info attribute.

## This cannot be done per Rd file, but we can switch to mathjaxr if
## any Rd file in a package uses mathjaxr

.convert_package_rdfiles <- function(package, dir = NULL, lib.loc = NULL, ...,
                                     stages = c("build", "later", "install", "render"),
                                     xLinks = character(0))
{
    ## if 'package' is an installed package (simplest) just use
    ## Rd_db(package) to get parsed Rd files. Otherwise, if 'package'
    ## is a .tar.gz file, assume that it's a source package (so unpack
    ## and call Rd_db on those). If 'package' is missing but 'dir' is
    ## not, interpret as source package (no need to unpack)

    isPkgTarball <- function(x) {
        length(x) == 1L && 
            endsWith(x, "tar.gz") &&
            length(strsplit(basename(x), "_", fixed = TRUE)[[1]]) == 2L
    }
    isURL <- function(x) {
        length(x) == 1L && 
            (startsWith(x, "http://") || startsWith(x, "https://"))
    }
    db <- 
        if (!missing(package) && isTRUE(isPkgTarball(package)))
        {
            src.type <- "tarball"
            ## If URL, download first
            if (isURL(package)) {
                destdir <- tempfile("dir")
                if (!dir.create(destdir))
                    stop(gettextf("unable to create temporary directory %s",
                                  sQuote(destdir)))
                utils::download.file(package, destfile = file.path(destdir, basename(package)))
                package <- file.path(destdir, basename(package))
            }
            ## Unpack first.
            ## Copied from src/library/utils/R/unix/mac.install.R::unpackPkg
            tmpDir <- tempfile("pkg")
            if (!dir.create(tmpDir))
                stop(gettextf("unable to create temporary directory %s",
                              sQuote(tmpDir)))
            utils::untar(package, exdir = tmpDir)
            pkgdir <- list.dirs(tmpDir, recursive = FALSE)
            if (length(pkgdir) != 1)
                stop(gettextf("expected one package directory, found %d.",
                              length(pkgdir)))
            Rd_db(dir = pkgdir, stages = stages)
        }
        else {
            src.type <- if (is.null(dir)) "installed" else "source"
            pkgdir <- if (is.null(dir)) find.package(package, lib.loc) else dir
            if (is.null(dir)) Rd_db(package, , lib.loc, stages = stages)
            else Rd_db(, dir, lib.loc, stages = stages)
        }

    ## create links database for help links. Level 0 links are
    ## obtained directly from the db, which is useful for non-installed packages.
    Links0 <- .build_links_index(Rd_contents(db), basename(pkgdir))
    Links <- c(Links0, findHTMLlinks(pkgdir, level = 1))
    Links2 <- xLinks
    
    rd2lines <- function(Rd, ...) {
        ## Rd2HTML() returns output location, which is not useful
        ## here, but also attributes that are.
        outlines <-
            utils::capture.output(
                       h <- Rd2HTML(Rd, out = "",
                                    package = pkgdir, # to extract pkgname/version info
                                    Links = Links, Links2 = Links2,
                                    ...)
                   )
        list(outlines = outlines, info = attr(h, "info"),
             concordance = attr(h, "concordance"))
    }
    structure(lapply(db, rd2lines, standalone = FALSE, ...),
              pkgdir = pkgdir, src.type = src.type,
              descfile = file.path(pkgdir, "DESCRIPTION"))

}



pkg2HTML <- function(package, dir = NULL, lib.loc = NULL,
                     outputEncoding = "UTF-8",
                     stylesheet = file.path(R.home("doc"), "html", "R-nav.css"),
                     hooks = list(pkg_href = function(pkg) sprintf("%s.html", pkg)),
                     texmath = getOption("help.htmlmath"),
                     prism = TRUE,
                     out = NULL,
                     toc_entry = c("name", "title"),
                     ...,
                     Rhtml = FALSE,
                     mathjax_config = file.path(R.home("doc"), "html", "mathjax-config.js"),
                     include_description = TRUE,
		     concordance = FALSE)
{
    toc_entry <- match.arg(toc_entry)
    hcontent <- .convert_package_rdfiles(package = package, dir = dir, lib.loc = lib.loc,
                                         outputEncoding = outputEncoding,
                                         Rhtml = Rhtml, hooks = hooks,
                                         texmath = "katex", prism = prism, concordance = concordance, ...)
    descfile <- attr(hcontent, "descfile")
    src.type <- attr(hcontent, "src.type")
    pkgdir <- attr(hcontent, "pkgdir")
    descmeta <- .read_description(descfile)
    pkgname <- descmeta["Package"]
    if (is.null(out)) {
        out <- if (is.null(hooks$pkg_href)) ""
               else hooks$pkg_href(pkgname)
    }
    
    ## Sort by name, as in PDF manual (check exact code). The
    ## '<pkg>-package.Rd' summary page, if present, should come first.
    hcontent <- hcontent[order(vapply(hcontent,
                                      function(h) h$info$name,
                                      ""))]
    if (length(hcontent) > 1 &&
        length(wsumm <- which(vapply(hcontent,
                                     function(h) isTRUE(h$info$pkgsummary),
                                     FALSE))) > 0L) {
        hcontent <- c(hcontent[wsumm], hcontent[-wsumm])
    }
    rdnames <- vapply(hcontent, function(h) h$info$name, "")
    rdtitles <- vapply(hcontent, function(h) h$info$title[[1L]], "")
    ## rdtitles <- vapply(hcontent, function(h) h$info$htmltitle[[1L]], "") # FIXME: has extra <p>
    use_mathjax <- any(vapply(hcontent, function(h) h$info$mathjaxr, FALSE))
    if (missing(texmath) || is.null(texmath))
        texmath <- if (use_mathjax) "mathjax" else "katex"

    toclines <- sprintf("<li><a href='#%s'>%s</a></li>",
                        name2id(rdnames),
                        switch(toc_entry, title = rdtitles, name = rdnames))

    language <- descmeta["Language"]
    if(is.na(language))
        language <- "en"
    else if(grepl(",", language))
        language <- NA_character_
    ## If DESCRIPTION specifices several languages, we currently cannot
    ## tell which one will be used for the package Rd files.  We could
    ## guess to use the first language given, for now simply take the
    ## language as unknown.
    
    ## Now to make a file with header + DESCRIPTION + TOC + content + footer

    hfcomps <- # should we be able to specify static URLs here?
        HTMLcomponents(title = sprintf('Package {%s}', pkgname),
                       headerTitle = paste0("Help for package ", pkgname),
                       logo = FALSE,
                       up = NULL, top = NULL,
                       css = stylesheet,
                       outputEncoding = outputEncoding,
                       dynamic = FALSE, prism = prism,
                       doTexMath = TRUE,
                       texmath = if (use_mathjax) "mathjax" else texmath,
                       MATHJAX_CONFIG_STATIC = mathjax_config,
                       language = language)

    linecount <- 0L
    writeHTML <- function(..., sep = "\n", append = TRUE) {
        cat(..., file = out, sep = sep, append = append)
	if (concordance) {
	    if (!append)
		linecount <<- 0L
	    if (sep == "\n")
		linecount <<- linecount + sum(lengths(list(...)))
	    # Also add any embedded newlines...
	    linecount <<- linecount + sum(sapply(list(...),
			function(s) sum(unlist(gregexpr("\n", s, fixed = TRUE)) > 0)))
	}
    }

    ## cat(hfcomps$header, fill = TRUE) # debug
    writeHTML(hfcomps$header, sep = "", append = FALSE)
    ## writeHTML(sprintf("<header class='top'><h1>Package {%s}</h1><hr></header>",
    ##                   pkgname))
    writeHTML('<nav class="package" aria-label="Topic Navigation">',
              '<div class="dropdown-menu">',
              sprintf('<img class="toplogo" src="%s" alt="[logo]">',
                      if (src.type == "installed") staticLogoPath(pkgname, relative = FALSE)
                      else staticLogoPath(pkgdir, relative = FALSE, dir = TRUE)),
              '<h2>Contents</h2>',
              '<ul class="menu">',
              toclines,
              '</ul>',
              '</div>',
              '<hr>',
              '</nav>',
              '<main>')

    if (include_description) writeHTML(.DESCRIPTION_to_HTML(descfile))
    lapply(hcontent, function(h) {
    	if (concordance) {
    	    conc <- h$concordance
    	    if (inherits(conc, "Rconcordance")) {
    	        conc$offset <- conc$offset + linecount + 1L
    	        h$outlines[length(h$outlines)] <-
    	            paste("<!--", as.character(conc), "-->")
    	    }
    	}
    	writeHTML("<hr>", h$outlines)
    })
    writeHTML('</main>')
    writeHTML(hfcomps$footer, sep = "")
    invisible(out)
}


