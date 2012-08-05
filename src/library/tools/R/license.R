#  File src/library/tools/R/license.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

## <NOTE>
## We want *standardized* license specs so that we can compute on them.
## In particular, we want to know whether licenses are recognizable as
## free (FSF) or open source (OSI) licenses.
## With the current standardization scheme, standardized license specs
## specify free or open source software licenses or refer to LICENSE or
## LICENCE files (requiring inspection by maintainers or installers).
## AGPL is a particular nuisance: the FSF considers it a free software
## license, but additional (e.g., attribution) clauses make it necessary
## for installers to inspect these.
## The R distribution contains a basic free or open source software
## license db, but it would be good to have a more extensible mechanism
## eventually.
##
## See in particular
##   http://www.fsf.org/licensing/licenses
##   http://en.wikipedia.org/wiki/List_of_FSF_approved_software_licences
##   http://en.wikipedia.org/wiki/List_of_OSI_approved_software_licences
## for more information.
## </NOTE>

re_anchor <-
function(s)
    if(length(s)) paste0("^", s, "$") else character()

re_group <-
function(s)
    if(length(s)) paste0("(", s, ")") else character()

re_or <-
function(s, group = TRUE) {
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
        read.dcf(file.path(R.home("share"), "licenses", "license.db"))
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
               unlist(license_db[license_db$Version == "",
                                 c("Name", "Abbrev")],
                      use.names = FALSE))
    license_names_or_abbrevs_with_version <-
        unique(Filter(nzchar,
                      unlist(license_db[license_db$Version != "",
                                    c("Name", "Abbrev")],
                             use.names = FALSE)))

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
              paste0("(", re_for_single_version_spec, ",)*"),
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
                      paste0("(", re_for_version_spec, ")*"),
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
    re_for_license_extension <-
        sprintf("[[:space:]]*\\+[[:space:]]*%s", re_for_license_file)
    ## <NOTE>
    ## Many standard licenses actually do not allow extensions.
    ## Ideally, we would only allow the extension markup for extensible
    ## standard licenses, as identified via an Extensible: TRUE field in
    ## the license db.  But version ranges make this tricky: e.g.,
    ##   GPL (>= 2) + file LICENSE
    ## is not right as GPL-2 does not allow extensions ...
    ## Hence, for now allow the extension markup with all standard
    ## licenses.
    ## </NOTE>
    re_for_component <-
        re_anchor(re_or(c(sprintf("%s(%s)?",
                                  re_or(c(re_for_license_short_spec,
                                          re_for_free_or_open_software_spec)),
                                  re_for_license_extension),
                          re_for_license_file,
                          "Unlimited")))
    list(re_for_component = re_for_component,
         re_for_license_file = re_for_license_file)
}

license_regexps <- .make_license_regexps()

## Standardizable and other free or open source software license specs:

## License specifications found on CRAN/BioC/Omegahat and manually
## classified as standardizable (hence currently free or open source)
## software licenses (even though not standardized/canonical), provided
## as a list of license specs named by the respective standardizations.
## With ongoing standardization this should gradually be eliminated.
## Last updated: 2009-02-19.

## Nasty issues.
## * There really is no GPL version 2.0.
##   Unfortunately, the FSF uses 2.0 in URLs or links
##   (http://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
##   The text clearly says "Version 2, June 1991".
## * There really is no LGPL version 2.0.
##   Unfortunately, the FSF uses 2.0 in URLs or links
##   (http://www.gnu.org/licenses/old-licenses/).
##   The text clearly says "Version 2, June 1991".
## * CeCILL is a bit of a mess: the current version is referred to as
##   "version 2" (http://www.cecill.info/licences.en.html) but
##    internally uses "Version 2.0 dated 2006-09-05"
##    (http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt).

.standardizable_license_specs <-
list("Artistic-2.0" =
     c("The Artistic License, Version 2.0",
       "Artistic 2.0",
       "Artistic-2.0, see http://www.opensource.org/licenses/artistic-license-2.0.php"
       ),

     "CeCILL-2" =
     c("CeCILL-2.0"
       ),

     "GPL" =
     c("GNU Public License",
       "Gnu GPL",
       "GNU GPL",
       "GPL (http://www.gnu.org/copyleft/gpl.html)"
       ),

     "GPL-2" =
     c(## <NOTE>
       ## There is no GPL 2.0, see above.
       "GNU General Public License 2.0.",
       "GPL 2.0",
       "GPL version 2.0",
       "GPL2.0",
       ## </NOTE>
       "GPL Version 2",
       "GNU GPL Version 2",
       "GNU GPL version 2",
       "GNU GPL version 2.",
       "GPL (version 2)",
       "GPL 2",
       "GPL 2.",
       "GPL version 2",
       "GPL version 2 (June, 1991)",
       "GPL version 2.",
       "GPL2",
       ## BioC:
       "GPL V2",
       "GPL, version 2"
       ),

     "GPL-3" =
     c("GPL Version 3",
       "GPL version 3",
       "GNU General Public Licence (GPLv3)",
       "GPL 3",
       "GPL v3"
       ),

     "GPL (>= 2)" =
     c(## <NOTE>
       ## There is no GPL 2.0, see above.
       "GNU GPL v2.0 or greater",
       "GPL 2.0 or higher",
       "GPL 2.0 or newer",
       "GPL version 2.0 or later",
       "GPL version 2.0 or newer",
       ## </NOTE>
       "GNU GPL (version 2 or later)",
       "GNU GPL (version 2 or later); see the file COPYING for details",
       "GNU GPL version 2 or newer",
       "GNU General Public License version 2 or newer",
       "GPL version 2 or later",
       "GPL ( version 2 or later)",
       "GPL (Version 2 or above)",
       "GPL (Version 2 or later)",
       "GPL (version 2 or higher)",
       "GPL (version 2 or later)",
       "GPL (version 2 or later, see the included file GPL)",
       "GPL (version 2 or newer)",
       "GPL 2 or later",
       "GPL 2 or newer",
       "GPL version 2 or any later version",
       "GPL Version 2 or later",
       "GPL Version 2 or later.",
       "GPL Version 2 or newer",
       "GPL Version 2 or newer.",
       "GPL version 2 (June, 1991) or later",
       "GPL version 2 (or newer)",
       "GPL version 2 or later.",
       "GPL version 2 or newer",
       "GPL version 2 or newer (http://www.gnu.org/copyleft/gpl.html)",
       "GPL version 2 or newer (see README).",
       "GPL version 2 or newer.",
       "GPL version 2 or newer. http://www.gnu.org/copyleft/gpl.html",
       "GPL version 2, or, at your option, any newer version.",
       "GPL Version 2 (or later)",
       "GPL version 2 (or later)",
       "GPL version 2 or higher",
       "GPL2 or later",
       "GPL>=2",
       "GNU General Public License (version 2 or later)"
       ),

     "GPL (>= 3)" =
     c("GPL (version 3 or later)",
       "GPL >=3"
       ),

     "GPL | LGPL" =
     c("GPL or LGPL by your choice"
       ),

     "GPL | BSD" =
     c("GPL, BSD"
       ),

     "GPL-2 | file LICENSE" =
     c("use under GPL2, or see file LICENCE"
       ),

     "LGPL" =
     c("LGPL (see <http://www.opensource.org/licenses/lgpl-license.php>).",
       "GNU LGPL (same as wxWidgets)."
       ),

     "LGPL-2" =
     c("LGPL2",
       "LGPL2.0"
       ),

     "LGPL-2.1" =
     c("LGPL version 2.1"
       ),

     "LGPL-3" =
     c("LGPL-v3"
       ),

     "LGPL (>= 2.0)" =
     c(## <NOTE>
       ## There is no LGPL-2.0, see above.
       "LGPL >= 2.0",
       ## </NOTE>
       "LGPL Version 2 or later.",
       "LGPL version 2 or newer",
       "LGPL (version 2 or later)",
       "LGPL version 2 or later"
       ),

     "LGPL (>= 2.1)" =
     c("LGPL version 2.1 or later"
       ),

     "LGPL (>= 3.0)" =
     c("LGPL >=3"
       ),

     "X11" =
     c("X11 (http://www.x.org/Downloads_terms.html)"
       ),

     "Unlimited" =
     c("Unlimited use and distribution."
       )
)

.standardizable_license_specs_db <-
data.frame(ispecs =
           unlist(.standardizable_license_specs),
           ospecs =
           rep.int(names(.standardizable_license_specs),
                   sapply(.standardizable_license_specs,
                          length)),
           stringsAsFactors = FALSE)

## These used to be in .safe_license_specs_in_standard_repositories:
##    "Artistic",
##    "GPL AFFERO 3.0 (with citation)",
##    "caBIG"
##    "Unlimited distribution.",
## These are safe from a distribution point of view, but clearly not
## standardizable ... hence:
## A list of license specs we cannot standardize, but safely classify as
## free or open source software licenses:
.other_free_or_open_license_specs <-
c("Artistic",
  "GPL AFFERO 3.0 (with citation)",
  ## https://cabig-kc.nci.nih.gov/CTMS/KC/index.php/C3PR_caBIG_License
  "caBIG"
  )

.safe_license_specs <-
    c(.standardizable_license_specs_db$ispecs,
      .other_free_or_open_license_specs)

analyze_license <-
function(x)
{
    .make_results <- function(is_empty = FALSE,
                              is_canonical = FALSE,
                              bad_components = character(),
                              is_standardizable = FALSE,
                              is_verified = FALSE,
                              standardization = NA_character_,
                              is_extended = NA,
                              pointers = NULL)
        list(is_empty = is_empty,
             is_canonical = is_canonical,
             bad_components = bad_components,
             is_standardizable = is_standardizable,
             is_verified = is_verified,
             standardization = standardization,
             is_extended = is_extended,
             pointers = pointers)


    x <- .strip_whitespace(x)
    if(is.na(x) || (x == "")) {
        ## Not really a lot to check ...
        ## (Note that non-standardizable license specs are dropped by
        ## writePACKAGES() and friends.)
        return(.make_results(is_empty = TRUE))
    }

    ## Try splitting into the individual components.
    components <-
        .strip_whitespace(unlist(strsplit(x, "|", fixed = TRUE)))

    ## Now analyze the individual components.
    ok <- grepl(license_regexps$re_for_component, components)
    bad_components <- components[!ok]

    ## Is the license specification "safe" in the sense of automatically
    ## verifiable as a free or open source software license?
    ## For the time being, test whether the spec is canonical and
    ## different from just a pointer to a license file, or in the list
    ## of safe specifications derived from license specifications in the
    ## standard repositories.
    is_verified <-
        ((all(ok)
          && !all(grepl(re_anchor(license_regexps$re_for_license_file),
                        components)))
         || all(components %in% .safe_license_specs))

    ## Is the license specification standardizable?
    standardizable <-
        components %in% .standardizable_license_specs_db$ispecs
    is_standardizable <- (all(ok) || all(standardizable))

    standardization <- if(is_standardizable) {
        ## Standardize the ones which are standardizable but not yet
        ## standardized.
        ind <- !ok & standardizable
        if(any(ind))
            components[ind] <-
                .standardize_license_components(components[ind])
        ## Canonicalize the standardized ones a bit more (as we are
        ## rather generous about using whitespace).
        ind <- ok & grepl("\\(", components)
        if(any(ind)) {
            s <- sub("[[:space:]]*\\([[:space:]]*", " \\(",
                     components[ind])
            s <- sub("[[:space:]]*\\)", "\\)", s)            
            s <- gsub("[[:space:]]*,[[:space:]]*", ", ", s)
            ## Really re_or(operators) ...
            s <- gsub("[[:space:]]+(<=?|>=?|==|!=)", " \\1", s)
            components[ind] <-
                gsub(sprintf("[[:space:]]*(%s)",
                             .standard_regexps()$valid_numeric_version),
                     " \\1", s)
        }
        paste(components, collapse = " | ")
    } else NA_character_

    pointers <- NULL
    is_extended <- NA
    ## Analyze components provided that we know we can standardize.
    if(is_standardizable) {
        components <-
            .strip_whitespace(unlist(strsplit(standardization, "|",
                                              fixed = TRUE)))
        ind <- grep(sprintf("%s$",
                            license_regexps$re_for_license_file),
                     components)
        if(length(ind)) {
            pointers <- sub(".*file ", "", components[ind])
            is_extended <-
                any(grepl("+", components[ind], fixed = TRUE))
        } else {
            is_extended <- FALSE
        }
    }

    .make_results(is_canonical = all(ok),
                  bad_components = bad_components,
                  is_standardizable = is_standardizable,
                  is_verified = is_verified,
                  standardization = standardization,
                  is_extended = is_extended,
                  pointers = pointers)
}

.standardize_license_components <-
function(x)
{
    with(.standardizable_license_specs_db,
         ospecs[match(x, ispecs)])
}

analyze_licenses <-
function(x)
{
    x <- as.character(x)
    if(!length(x)) return(NULL)
    ## As analyzing licenses is costly, only analyze the unique specs.
    v <- unique(x)
    status <- lapply(v, analyze_license)
    ## And now turn into a data frame.
    out <- data.frame(matrix(0, length(v), 0L))
    for(j in seq_along(status[[1L]]))
        out[[j]] <- sapply(status, "[[", j)
    names(out) <- names(status[[1L]])
    ## And re-match specs to the unique specs.
    out <- out[match(x, v), ]
    rownames(out) <- NULL
    out
}

build_license_db <-
function(dir, unpacked = FALSE)
{
    ## Note that currently (2007-10-20), the license info is *NOT*
    ## written into the repository db (file 'PACKAGES') anymore.

    CRAN <- getOption("repos")["CRAN"]
    if(missing(dir) && substring(CRAN, 1L, 7L) == "file://")
        dir <- file.path(substring(CRAN, 8L), "src", "contrib")

    fields <- c("License", "Maintainer")
    db <- .build_repository_package_db(dir, fields, unpacked = unpacked)
    ## Actually, for Omegehat this is not a good idea as this retains
    ## old versions in the "main" src/contrib directory.  But let's not
    ## worry about this for now ...

    db <- do.call("rbind", db)

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
        db <- .remove_stale_dups(db)
    }
    analyze_licenses_in_license_db(db)
}

summarize_license_db <-
function(db)
{
    packages <- db$Package
    if(any(duplicated(packages)))
        packages <- sprintf("%s_%s", packages, db$Version)
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
    .safe_license_specs %w/o% unique(ldb$License)
}

find_canonical_safe_license_specs <-
function()
{
    grep(license_regexps$re_for_component,
         .safe_license_specs,
         value = TRUE)
}
