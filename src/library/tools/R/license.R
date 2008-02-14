#  File src/library/tools/R/license.R
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

re_anchor <- function(s)
    if(length(s)) paste("^", s, "$", sep = "") else character()
re_group <- function(s)
    if(length(s)) paste("(", s, ")", sep = "") else character()
re_or <- function(s, group = TRUE) {
    if(!length(s))
        character()
    else if(group)
        re_group(paste(s, collapse = "|"))
    else
        paste(s, collapse = "|")
}

.make_license_regexps <-
function()
{
    ## Build license regexps according to the specs.

    ## Read in the license data base, and precompute some variables.
    license_db <-
        read.dcf(file.path(R.home(), "share", "licenses", "license.db"))
    license_db[is.na(license_db)] <- ""
    ## (Could also keeps NAs and filter on is.finite().)
    license_db <- data.frame(license_db, stringsAsFactors = FALSE)

    has_abbrev <- nzchar(license_db$Abbrev)
    has_version <- nzchar(license_db$Version)

    license_short_specs <-
        unique(c(Filter(nzchar, license_db$SSS),
                 do.call(paste,
                         c(license_db[has_abbrev & has_version,
                                      c("Abbrev", "Version")],
                           list(sep = "-")))))
    license_names_or_abbrevs_without_version <-
        Filter(nzchar,
               unlist(subset(license_db, Version == "",
                             c("Name", "Abbrev")),
                      use.names = FALSE))
    license_names_or_abbrevs_with_version <-
        Filter(nzchar,
               unlist(subset(license_db, Version != "",
                             c("Name", "Abbrev")),
                      use.names = FALSE))
    
    operators <- c("<", "<=", ">", ">=", "==", "!=")
    re_for_numeric_version <- .standard_regexps()$valid_numeric_version
    re_for_single_version_spec <-
        paste("[[:space:]]*",
              re_or(operators),
              "[[:space:]]*",
              re_for_numeric_version,
              "[[:space:]]*",
              sep = "")
    re_for_version_spec <-
        paste("\\(",
              paste("(", re_for_single_version_spec, ",)*", sep = ""),
              re_for_single_version_spec,
              "\\)",
              sep = "")
    re_for_free_or_open_software_spec <-
        re_or(c(re_or(license_names_or_abbrevs_without_version),
                ## We currently considers names or abbrevs of versioned
                ## licenses, *possibly* followed by a version spec, as
                ## canonical.  This is not quite perfect, as ideally a
                ## version spec should be provided in case it matters.
                ## Let us use the interpretation that no version spec
                ## means "any version" (which is correct for GPL).
                paste(re_or(license_names_or_abbrevs_with_version),
                      "[[:space:]]*",
                      paste("(", re_for_version_spec, ")*", sep = ""),
                      sep = ""),
                ## Also allow for things like
                ##   GNU General Public License version 2
                ##   Apache License Version 2.0
                ## as one can argue that these are really the full names
                ## of these licenses.
                re_or(paste(license_db$Name[has_version],
                            "[[:space:]]+([Vv]ersion[[:space:]]+)?",
                            license_db$Version[has_version],
                            sep = ""))))

    re_for_license_short_spec <- re_or(license_short_specs)
    re_for_license_file <- "file LICEN[CS]E"
    re_for_component <-
        re_anchor(re_or(c(re_for_license_short_spec,
                          re_for_free_or_open_software_spec,
                          re_for_license_file,
                          "Unlimited")))
    list(re_for_component = re_for_component,
         re_for_license_file = re_for_license_file)
}

license_regexps <- .make_license_regexps()

## License specifications found on CRAN/BioC/Omegahat and manually
## classified as "safe" open/free software licenses (even though not
## canonical).  With ongoing standardization this should gradually be
## eliminated.
## Last updated: 2007-11-04.

## Note
##   http://www.fsf.org/licensing/licenses
##   http://en.wikipedia.org/wiki/List_of_FSF_approved_software_licences

.safe_license_specs_in_standard_repositories <-
    c(## <NOTE>
      ## These really need fixing for a variety of reasons:
      "'GPL'",
      "Artistic",
      "CeCILL 2 (GNU GPL 2 compatible)",
      "LGPL version 2.1 or newer (the releases)",
      "Standard GNU public license",
      "The Gnu general public liscense, current version, 2/12/2004.", 
      "Unlimited distribution.",
      ## It is really GNU Library General Public License 2
      ## and GNU Lesser General Public License 2.1.
      "Lesser GPL Version 2 or later.",
      ## These are variants of GPL 2.0 which does not exist:
      "GNU GPL v2.0 or greater",
      "GNU General Public License 2.0.",
      "GNU Public Licence 2.0 or above at your convenience", 
      "GNU-license 2.0 or newer",
      "GPL (Version 2.0 or later)",
      "GPL (version 2.0 or later)",
      "GPL 2.0", 
      "GPL 2.0 or above",
      "GPL 2.0 or higher",
      "GPL 2.0 or later", 
      "GPL 2.0 or newer",
      "GPL version 2.0",
      "GPL version 2.0 or later",
      "GPL version 2.0 or newer",
      "GPL2.0",
      ## CeCILL is a bit of a mess: the current version is referred to
      ## as "version 2" (http://www.cecill.info/licences.en.html) but
      ## internally uses "Version 2.0 dated 2006-09-05"
      ## (http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt).
      "CeCILL-2.0",
      ## </NOTE>
      "GNU GPL (version 2 or any later version)", 
      "GNU GPL (version 2 or later)",
      "GNU GPL (version 2 or later); see the file COPYING for details", 
      "GNU GPL Version 2",
      "GNU GPL Version 2 (or later)",
      "GNU GPL Version 2 or newer.", 
      "GNU GPL version 2", 
      "GNU GPL version 2 or newer",
      "GNU GPL version 2.",
      "GNU General Public License Version 2 or higher.", 
      "GNU General Public License version 2 or newer",
      "GNU Public License",
      "GPL version 2 or later", 
      "GPL ( version 2 or later)",
      "GPL (GNU Public Licence), Version 2 or later",
      "GPL (Version 2 or above)", 
      "GPL (Version 2 or later)",
      "GPL (version 2 or higher)",
      "GPL (version 2 or later)",
      "GPL (version 2 or later, see the included file GPL)",
      "GPL (version 2 or newer)", 
      "GPL (version 2)",
      "GPL 2",
      "GPL 2 or above", 
      "GPL 2 or later",
      "GPL 3.0 Affero (with attribution)", 
      "GPL AFFERO 3.0 (with citation)",
      "GPL Version 2",
      "GPL Version 2 or higher", 
      "GPL Version 2 or later",
      "GPL Version 2 or later.",
      "GPL Version 2 or newer", 
      "GPL Version 2 or newer.",
      "GPL Version 2.",
      "GPL Version 2. See the LICENSE file for details.", 
      "GPL or LGPL by your choice",
      "GPL v2",
      "GPL v2 (but not later)", 
      "GPL v2 or later",
      "GPL version 2",
      "GPL version 2 (June, 1991)", 
      "GPL version 2 (June, 1991) or later",
      "GPL version 2 (or newer)",
      "GPL version 2 or later (June, 1991)",
      "GPL version 2 or later.",
      "GPL version 2 or newer",
      "GPL version 2 or newer (http://www.gnu.org/copyleft/gpl.html)", 
      "GPL version 2 or newer (see README).",
      "GPL version 2 or newer.", 
      "GPL version 2 or newer. http://www.gnu.org/copyleft/gpl.html", 
      "GPL version 2, or, at your option, any newer version.",
      "GPL version 2.", 
      "GPL version 3 or newer",
      "GPL vesion 2 or newer",
      "GPL2", 
      "Gnu GPL",
      "LGPL (see <http://www.opensource.org/licenses/lgpl-license.php>).", 
      "LGPL 2.1",
      "LGPL >= 2.0",
      "LGPL Version 2 or later.",
      "LGPL version 2",
      "LGPL version 2 or newer",
      "LGPL version 2.1 or later",
      "LGPL2",
      "Mozilla Public License 1.1 (http://www.mozilla.org/MPL/)", 
      "The Artistic License, Version 2.0",
      "X11 (http://www.x.org/Downloads_terms.html)",
      "use under GPL2, or see file LICENCE",
      "GNU GPL",
      "GPL (version 2 or later) See file LICENCE.",
      "GPL 2.",
      "GPL Version 2 (or later)",
      "LGPL (version 2 or later)",
      ## BioC
      "GNU GPL.",
      "GPL (http://www.gnu.org/copyleft/gpl.html)",
      "GPL V2",
      "GPL version 2 (or later)",
      "GPL version 2 or higher",
      "GPL, version 2",
      "GPL2 or later",
      "LGPL version 2.1",
      "caBIG"
)

analyze_license <-
function(x)
{
    .make_results <- function(is_empty = FALSE,
                              is_canonical = FALSE,
                              bad_components = character(),
                              is_verified = FALSE,
                              pointers = NULL)
        list(is_empty = is_empty,
             is_canonical = is_canonical,
             bad_components = bad_components,
             is_verified = is_verified,
             pointers = pointers)


    x <- .strip_whitespace(x)
    if(x == "") {
        ## Not really a lot to check ...
        return(.make_results(is_empty = TRUE))
    }

    ## Try splitting into the individual components.
    components <-
        .strip_whitespace(unlist(strsplit(x, "|", fixed = TRUE)))
    
    ## Now analyze the individual components.
    ok <- regexpr(license_regexps$re_for_component, components) > -1L

    pointers <- if(all(ok)) NULL else {
        ind <- regexpr(re_anchor(license_regexps$re_for_license_file),
                       components) > -1L
        sub("file ", "", components[ind])
    }

    ## Is the version specification "safe"?  For the time being, test
    ## whether the spec is canonical and different from just a pointer
    ## to a license file, or in a table of safe specifications derived
    ## from license specifications of CRAN packages as recorded in Sep
    ## 2007.
    is_verified <-
        ((all(ok)
          && any(regexpr(re_anchor(license_regexps$re_for_license_file),
                         components) == -1L))
         || all(components %in%
                .safe_license_specs_in_standard_repositories))

    .make_results(is_canonical = all(ok),
                  bad_components = components[!ok],
                  is_verified = is_verified,
                  pointers = pointers)
}

analyze_licenses <-
function(x)
{
    x <- as.character(x)
    if((n <- length(x)) == 0L) return(NULL)
    status <- lapply(x, analyze_license)
    ## And now turn into a data frame.
    out <- data.frame(matrix(0, n, 0L))
    for(j in seq_along(status[[1L]]))
        out[[j]] <- sapply(status, "[[", j)
    names(out) <- names(status[[1L]])
    out
}

build_license_db <-
function(dir, unpacked = FALSE)
{
    ## Note that currently (2007-10-20), the license info is *NOT*
    ## written into the repository db (file 'PACKAGES') anymore. 
    
    CRAN <- getOption("repos")["CRAN"]
    if(missing(dir) && substring(CRAN, 1, 7) == "file://")
        dir <- file.path(substring(CRAN, 8), "src", "contrib")

    fields <- c("License", "Maintainer")
    db <- .build_repository_package_db(dir, fields, unpacked = unpacked)
    ## Actually, for Omegehat this is not a good idea as this retains
    ## old versions in the "main" src/contrib directory.  But let's not
    ## worry about this for now ...

    db <- do.call("rbind", db)
    is_bundle <- is.na(db[, "Package"])
    db[is_bundle, "Package"] <- db[is_bundle, "Bundle"]
    
    ## Retain what is needed ...
    data.frame(db[ , c("Package", "Version", fields)],
               stringsAsFactors = FALSE)
}

analyze_licenses_in_license_db <-
function(db)
    cbind(db, analyze_licenses(db$License))

analyze_licenses_in_repository <-
function(dir, unpacked = FALSE, full = TRUE)
{
    db <- build_license_db(dir, unpacked)
    if(!full) {
        ## Only keep the highest available versions.
        ## Such an option might be useful for build_license_db()
        ## itself.
        db <- do.call("rbind",
                      lapply(split(db, db$Package),
                             function(e) {
                                 m <- max(as.numeric_version(e$Version))
                                 e[which(e$Version == m)[1L], ]
                             }))
    }        
    analyze_licenses_in_license_db(db)
}

summarize_license_db <-
function(db, full = FALSE)
{
    packages <- if(full)
        sprintf("%s_%s", db$Package, db$Version)
    else
        db$Package
    packages <- split(packages, db$License)
    licenses <- names(packages)
    out <- data.frame(Licenses = licenses, stringsAsFactors = FALSE)
    ## To get the 'packages' list into a data frame without I() ...
    out$Packages <- packages
    cat(formatDL(out$Licenses,
                 sapply(out$Packages,
                        function(p) paste(unique(p), collapse = " ")),
                 style = "list"),
        sep = "\n\n")
    invisible(out)
}

find_unused_safe_license_specs <-
function(...)
{
    ldb <- do.call("rbind", list(...))
    safe <- .safe_license_specs_in_standard_repositories
    safe[!safe %in% unique(ldb$License)]
}

find_canonical_safe_license_specs <-
function()
{
    grep(license_regexps$re_for_component,
         .safe_license_specs_in_standard_repositories,
         value = TRUE)
}
