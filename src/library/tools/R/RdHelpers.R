#  File src/library/tools/R/RdHelpers.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2014 The R Core Team
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

Rd_expr_PR <-
function(x)
{
    baseurl <- "https://bugs.R-project.org/bugzilla3/show_bug.cgi?id"
    sprintf("\\href{%s=%s}{PR#%s}", baseurl, x, x)
}

## These following functions are to take information from the package
## DESCRIPTION file at build time.  During a build, the current
## directory holds the DESCRIPTION file; set dir to something else if
## used in a different context.

Rd_macros_package_dir <-
function()
    Sys.getenv("_R_RD_MACROS_PACKAGE_DIR_", ".")

Rd_package_title <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc["Title"]
}

Rd_package_description <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc["Description"]
}

Rd_package_author <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc["Author"]
}

Rd_package_maintainer <-
function(pkg, dir = Rd_macros_package_dir())
{
    desc <- .read_description(file.path(dir, "DESCRIPTION"))
    if (pkg != desc["Package"])
    	stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
    desc["Maintainer"]
}

Rd_package_DESCRIPTION <-
function(pkg, lib.loc = Sys.getenv("R_BUILD_TEMPLIB"))
{
    if (!length(find.package(pkg, lib.loc = lib.loc, quiet=TRUE)))
        "This package was not yet installed at build time.\\cr"
    else {    
	tabular <- function(col1, col2)
	    c("\\tabular{ll}{", paste0(col1, " \\tab ", col2, "\\cr"), "}")

	desc <- utils::packageDescription(pkg, lib.loc = lib.loc)
	if (pkg != desc[["Package"]])
	    stop(gettextf("DESCRIPTION file is for package '%s', not '%s'", desc["Package"], pkg))
	desc <- desc[names(desc) != "Built"] # Probably a stale value
	tabular(paste0(names(desc), ":"), unlist(desc))
    }
}

Rd_package_indices <-
function(pkg, lib.loc = Sys.getenv("R_BUILD_TEMPLIB"))
{
    if (!length(find.package(pkg, lib.loc = lib.loc, quiet=TRUE)))
        result <- c("", "Index:  This package was not yet installed at build time.\\cr")
    else {    
    	tabular <- function(col1, col2)
    	    c("\\tabular{ll}{", paste0(col1, " \\tab ", col2, "\\cr"), "}")
    
        info <- library(help = pkg, lib.loc = lib.loc,
	  	    character.only = TRUE)

	result <- NULL
	# FIXME:  these indices should contain links...
	if (!is.null(info$info[[2L]]))
	    result <- c("", "Index of help topics:", "\\preformatted{",
				  info$info[[2L]], "}")
	if (!is.null(info$info[[3L]]))
	    result <- c(result, "",
			"Further information is available in the following vignettes:\\cr\\cr",
			tabular(paste0("\\code{", info$info[[3L]][,1], "}"),
			      info$info[[3L]][,2]))
    }
    result
}

Rd_expr_doi <-
function(x)
{
    ## Be nice ...
    x <- .canonicalize_doi(x)

    ## Poor person's way to allow LaTeX to break lines at slashes and
    ## dashes:
    y <- gsub("/", "\\\\out{\\\\slash{}}", gsub("-", "\\\\out{\\\\-}", x))

    sprintf("\\ifelse{text}{%s}{\\ifelse{latex}{%s}{%s}}",
            sprintf("doi: %s (URL: http://doi.org/%s)",
                    x, x),
            sprintf("doi:\\out{\\nobreakspace{}}\\href{http://doi.org/%s}{%s}",
                    x, y),
            sprintf("doi: \\href{http://doi.org/%s}{%s}",
                    x, x)
            )
}
