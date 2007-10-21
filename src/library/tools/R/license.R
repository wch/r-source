## Read in the license data base, and precompute some variables.

license_db <-
    data.frame(read.dcf(file.path(R.home(),
                                  "share", "licenses", "license.db")),
               stringsAsFactors = FALSE)
## (For the time being we ensure that all fields are in the db file, so
## that no NAs occur when reading in.)
standard_license_short_specs <-
    c(Filter(nzchar, license_db$SSS), "AGPL-3")
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

## License specifications found on CRAN/BioC/Omegahat and manually
## classified as "safe" open/free software licenses.  With ongoing
## standardiation this should gradually be eliminated.
## Last updated: 2007-10-20.

.safe_license_specs_in_standard_repositories <-
    c(## <NOTE>
      ## These really need fixing for a variety of reasons:
      "'GPL'",
      "Apache-Style Software License",
      "Artistic",
      "Artistic License",
      "CeCILL 2 (GNU GPL 2 compatible)",
      "GPL version 2 o newer",
      "LGPL version 2.1 or newer (the releases)",
      "Public Domain, unlimited distribution",
      "Public domain",
      "Standard GNU public license",
      "The Gnu general public liscense, current version, 2/12/2004.", 
      "Unlimited distribution.",
      ## It is really GNU Library General Public License 2
      ## and GNU Lesser General Public License 2.1.
      "Lesser GPL Version 2 or later.",
      ## These are variants of GPL 2.0 which does not exist:
      "GNU GPL v. 2.0 or later",
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
      "GPL2.0",
      ## These are variants of GPL 2.1 which does not exist:
      "GPL version 2.1 or newer",
      ## </NOTE>
      "BSD",
      "BSD.",
      "CeCILL version 2", 
      "GNU GENERAL PUBLIC LICENSE Version 2",
      "GNU GPL (version 2 or any later version)", 
      "GNU GPL (version 2 or later)",
      "GNU GPL (version 2 or later); see the file COPYING for details", 
      "GNU GPL Version 2",
      "GNU GPL Version 2 (or later)",
      "GNU GPL Version 2 or newer.", 
      "GNU GPL version 2", 
      "GNU GPL version 2 or newer",
      "GNU GPL version 2.",
      "GNU GPL2; http://www.fsf.org/licenses/gpl.html", 
      "GNU General Public License",
      "GNU General Public License Version 2 or higher.", 
      "GNU General Public License version 2 or newer",
      "GNU Public License",
      "GPL version 2 or later", 
      "GPL ( version 2 or later)",
      "GPL (GNU Public Licence) version 2 or later", 
      "GPL (GNU Public Licence), Version 2 or later",
      "GPL (Version 2 or above)", 
      "GPL (Version 2 or later)",
      "GPL (see COPYING)", 
      "GPL (v.2 or later)",
      "GPL (version 2 or higher)",
      "GPL (version 2 or higher; see LICENSE)", 
      "GPL (version 2 or later)",
      "GPL (version 2 or later).", 
      "GPL (version 2 or later, see the included file GPL)",
      "GPL (version 2 or newer)", 
      "GPL (version 2)",
      "GPL 2",
      "GPL 2 or above", 
      "GPL 2 or later",
      "GPL 2 or newer",
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
      "GPL version 2 (http://www.gnu.org/licenses/gpl.html)", 
      "GPL version 2 (or newer)",
      "GPL version 2 or greater", 
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
      "LGPL (Lesser GNU Public Licence)",
      "LGPL (Lesser GNU Public License)", 
      "LGPL (see <http://www.opensource.org/licenses/lgpl-license.php>).", 
      "LGPL 2.1",
      "LGPL >= 2.0",
      "LGPL Version 2 or later.",
      "LGPL version 2",
      "LGPL version 2 or newer",
      "LGPL version 2.1 or later",
      "LGPL version 2.1 or newer (the releases)", 
      "LGPL2",
      "MIT",
      "Mozilla Public License 1.1 (http://www.mozilla.org/MPL/)", 
      "The Artistic License, Version 2.0",
      "Unlimited use and distribution (see LICENCE).", 
      "X11 (http://www.x.org/Downloads_terms.html)",
      "use under GPL2, or see file LICENCE",
      "GNU GPL",
      "GNU General Public License (GNU GPL)",
      "GPL version 2 or later",
      "GPL (version 2 or later) See file LICENCE.",
      "GPL 2.",
      "GPL Version 2 (or later)",
      "LGPL (version 2 or later)",
      ## BioC
      "Artistic License 2.0",
      "Artistic License, Version 2.0",
      "Free Software Licence CeCILL",
      "GNU GPL.",
      "GPL (http://www.gnu.org/copyleft/gpl.html)",
      "GPL V2",
      "GPL version 2 (or later)",
      "GPL version 2 or higher",
      "GPL, version 2",
      "GPL2 or later",
      "LGPL version 2.1"
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

    ## Build up a regexp according to the specs.
    ## <FIXME>
    ## Maybe use a dynamic variable for storing things?
    ## </FIXME>
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
    re_for_free_or_open_source_spec <-
        re_or(c(re_or(license_names_or_abbrevs_without_version),
                paste(re_or(license_names_or_abbrevs_with_version),
                      "[[:space:]]*",
                      paste("(", re_for_version_spec, ")*", sep = ""),
                      sep = "")))
    re_for_standard_short_specs <- re_or(standard_license_short_specs)
    re_for_license_file <- "file LICEN[CS]E"
    re_for_component <-
        paste("^",
              re_or(c(re_for_standard_short_specs,
                      re_for_free_or_open_source_spec,
                      re_for_license_file,
                      "Unlimited")),
              "$",
              sep = "")

    x <- .strip_whitespace(x)
    if(x == "") {
        ## Not really a lot to check ...
        return(.make_results(is_empty = TRUE))
    }

    ## Try splitting into the individual components.
    components <-
        .strip_whitespace(unlist(strsplit(x, "|", fixed = TRUE)))
    
    ## Now analyze the individual components.
    ok <- regexpr(re_for_component, components) > -1L

    pointers <- if(all(ok)) NULL else {
        ind <- regexpr(paste("^", re_for_license_file, "$", sep = ""),
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
          && any(regexpr(paste("^", re_for_license_file, "$", sep = ""),
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
function(dir, unpacked = FALSE)
    analyze_licenses_in_license_db(build_license_db(dir, unpacked))

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
    out$Packages <- packages            # Argh, lists and data frames.
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
