#  File src/library/tools/R/license.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
## FOSS (http://en.wikipedia.org/wiki/Free_and_open-source_software)
## licenses.
##
## A license spec is standardized ("canonical") if it is an alternative
## of component specs which are one of the following:
##
## A. "Unlimited"
## B. "file LICENSE" or "file LICENCE"
## C. A specification based on the R license db
##    * A standard short specification (SSS field)
##    * The name or abbreviation of an unversioned license
##    * The name of abbreviation of a versioned license, optionally
##      followed by a version spec
##    * The name of a versioned license followed by the version
##    * The abbrevation of a versioned license combined with '-',
##   optionally followed by an extension spec as in B (in principle,
##   only if the base license is extensible).
##
## A license spec is standardizable if we know to transform it to
## standardized form.
##
## Note that the R license db also contains non-FOSS licenses, and hence
## information (FOSS field) on the FOSS status of the licenses.
## Ideally, a license taken as FOSS would be approved as free by the FSF
## and as open by the OSI: we also take licenses as FOSS when approved
## by the FSF (and not rejected by the OSI).
##
## See
##   http://www.gnu.org/licenses/license-list.html
##   http://opensource.org/licenses/alphabetical
## fot the FSF and OSI license lists, and also
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

.make_R_license_db <-
function(paths = NULL)
{
    if(is.null(paths))
        paths <- unlist(strsplit(Sys.getenv("R_LICENSE_DB_PATHS"),
                                 .Platform$path.sep, fixed = TRUE))
    paths <- c(paths,
               file.path(R.home("share"), "licenses", "license.db"))
    ldb <- Reduce(function(u, v) merge(u, v, all = TRUE), 
                  lapply(unique(normalizePath(paths)), read.dcf))
    ## Merging matrices gives a data frame.
    ldb <- as.matrix(ldb)
    ldb[is.na(ldb)] <- ""
    ## (Could also keep NAs and filter on is.finite() in subsequent
    ## computations.)
    ## FOSS == "yes" implues Restricts_use = "no":
    ldb[ldb[, "FOSS"] == "yes", "Restricts_use"] <- "no"
    ldb <- data.frame(ldb, stringsAsFactors = FALSE)
    ldb$Labels <- R_license_db_labels(ldb)
    ldb[!duplicated(ldb$Labels), ]
}

R_license_db_labels <-
function(ldb)
{
    if(is.null(ldb)) return(NULL)
    lab <- ldb$SSS
    pos <- which(lab == "")
    abbrevs <- ldb$Abbrev[pos]
    versions <- ldb$Version[pos]
    lab[pos] <- ifelse(nzchar(abbrevs), abbrevs, ldb$Name[pos])
    ind <- nzchar(versions)
    pos <- pos[ind]
    lab[pos] <- sprintf("%s version %s", lab[pos], versions[ind])
    lab
}

R_license_db <- local({
    val <- NULL
    function(new) {
        if(!missing(new))
            val <<- new
        else
            val
    }
})

R_license_db(.make_R_license_db())

.make_R_license_db_vars <-
function()
{
    ## Build license regexps and tables according to the specs.

    ldb <- R_license_db()

    ## Standard short specification (SSS field) from the R license db.
    pos <- which(nzchar(ldb$SSS))
    names(pos) <- ldb$SSS[pos]
    tab_sss <- pos

    has_version <- nzchar(ldb$Version)
    has_abbrev <- nzchar(ldb$Abbrev)

    ## Name or abbreviation of an unversioned license from the R license
    ## db.
    pos <- which(!has_version)
    names(pos) <- ldb$Name[pos]
    tab_unversioned <- pos
    pos <- which(has_abbrev & !has_version)
    tab_unversioned[ldb$Abbrev[pos]] <- pos

    ## Versioned licenses from the R license db.
    ## Style A: Name of abbreviation of a versioned license, optionally
    ##   followed by a version spec
    ## Style B: Name of a versioned license followed by the version.
    ## Style C: Abbrevation of a versioned license combined with '-'.
    pos <- which(has_version)
    names(pos) <- ldb$Name[pos]
    tab_versioned_style_A <- split(pos, names(pos))
    tab_versioned_style_B <- pos
    names(tab_versioned_style_B) <-
        paste(names(pos), ldb$Version[pos])
    pos <- which(has_version & has_abbrev)
    tab_versioned_style_A <-
        c(tab_versioned_style_A, split(pos, ldb$Abbrev[pos]))
    tab_versioned_style_C <- pos
    names(tab_versioned_style_C) <-
        sprintf("%s-%s",
                ldb$Abbrev[pos],
                ldb$Version[pos])

    operators <- c("<", "<=", ">", ">=", "==", "!=")
    re_numeric_version <- .standard_regexps()$valid_numeric_version
    re_single_version_spec <-
        paste0("[[:space:]]*",
               re_or(operators),
               "[[:space:]]*",
               re_group(re_numeric_version),
               "[[:space:]]*")
    re_version_spec <-
        paste0("\\(",
               paste0("(", re_single_version_spec, ",)*"),
               re_single_version_spec,
               "\\)")

    re_sss <- re_or(names(tab_sss))
    re_unversioned <- re_or(names(tab_unversioned))
    re_versioned_style_A <-
        paste0(re_or(names(tab_versioned_style_A)),
               "[[:space:]]*",
               paste0("(", re_version_spec, ")*"))
    ## Let's be nice ...
    re_versioned_style_B <-
        re_or(paste0(ldb$Name[has_version],
                     "[[:space:]]+([Vv]ersion[[:space:]]+)?",
                     ldb$Version[has_version]))
    re_versioned_style_C <- re_or(names(tab_versioned_style_C))

    re_license_in_db <-
        re_or(c(re_sss,
                re_unversioned,
                re_versioned_style_A,
                re_versioned_style_B,
                re_versioned_style_C))

    re_license_file <- "file LICEN[CS]E"
    re_license_extension <-
        sprintf("[[:space:]]*\\+[[:space:]]*%s", re_license_file)

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

    re_component <-
        re_anchor(re_or(c(sprintf("%s(%s)?",
                                  re_license_in_db,
                                  re_license_extension),
                          re_license_file,
                          "Unlimited")))
    list(re_component = re_component,
         re_license_file = re_license_file,
         re_license_extension = re_license_extension,
         re_single_version_spec = re_single_version_spec,
         re_sss = re_sss,
         re_unversioned = re_unversioned,
         re_versioned_style_A = re_versioned_style_A,
         re_versioned_style_B = re_versioned_style_B,
         re_versioned_style_C = re_versioned_style_C,
         tab_sss = tab_sss,
         tab_unversioned = tab_unversioned,
         tab_versioned_style_A = tab_versioned_style_A,
         tab_versioned_style_B = tab_versioned_style_B,
         tab_versioned_style_C = tab_versioned_style_C)
}

R_license_db_vars <- local({
    val <- NULL
    function(new) {
        if(!missing(new))
            val <<- new
        else
            val
    }
})


R_license_db_vars(.make_R_license_db_vars())

R_license_db_refresh_cache <-
function(paths = NULL)
{
    R_license_db(.make_R_license_db(paths))
    R_license_db_vars(.make_R_license_db_vars())
}

## Standardizable license specs:

## License specifications found on CRAN/BioC/Omegahat and manually
## classified as standardizable software licenses (even though not
## standardized/canonical), provided as a list of license specs named by
## the respective standardizations.
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

     "BSL" =
     c("Boost Software License",
       "Boost Software License 1.0",
       "BSL 1.0"
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

analyze_license <-
function(x)
{
    .make_results <- function(is_empty = FALSE,
                              is_canonical = FALSE,
                              bad_components = character(),
                              is_standardizable = FALSE,
                              is_verified = FALSE,
                              standardization = NA_character_,
                              components = NULL,
                              expansions = NULL,
                              extensions = NULL,
                              pointers = NULL,
                              is_FOSS = NA,
                              restricts_use = NA)
        list(is_empty = is_empty,
             is_canonical = is_canonical,
             bad_components = bad_components,
             is_standardizable = is_standardizable,
             is_verified = is_verified,
             standardization = standardization,
             components = components,
             expansions = expansions,
             extensions = extensions,
             pointers = pointers,
             is_FOSS = is_FOSS,
             restricts_use = restricts_use)


    x <- trimws(x)
    if(is.na(x) || (x == "")) {
        ## Not really a lot to check ...
        ## (Note that non-standardizable license specs are dropped by
        ## writePACKAGES() and friends.)
        return(.make_results(is_empty = TRUE))
    }

    pointers <- NULL
    extensions <- NULL
    expansions <- NULL
    is_verified <- FALSE
    is_FOSS <- NA
    restricts_use <- NA

    ## Try splitting into the individual components.
    components <- trimws(unlist(strsplit(x, "|", fixed = TRUE)))

    ## Now analyze the individual components.
    ok <- grepl(R_license_db_vars()$re_component, components)
    bad_components <- components[!ok]
    is_canonical <- all(ok)

    ## Is the license specification standardizable?
    standardizable <-
        components %in% .standardizable_license_specs_db$ispecs
    is_standardizable <- (is_canonical || all(standardizable))

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

    ## Analyze components provided that we know we can standardize.
    if(is_standardizable) {
        verifiable <- function(x, v = "yes")
            !is.null(x) && all(!is.na(x) & (x == v))
        ## (More generally we could test for positive length of x: but
        ## a length test is needed because all(NULL) |=> TRUE.)
        
        expansions <- lapply(components,
                             expand_license_spec_component_from_db)

        ## The license is FOSS if there is one component which is
        ## "Unlimited" or has a positive number of expansions all of
        ## which are FOSS.
        ## If all components have a positive number of expansions where
        ## at least one is not FOSS, the license is not FOSS.
        ## Otherwise we do not know.
        is_FOSS <- if(any(components == "Unlimited")) {
            TRUE
        } else if(any(sapply(expansions,
                             function(e) verifiable(e$FOSS)))) {
            TRUE
        } else if(all(sapply(expansions,
                             function(e) any(e$FOSS == "no")))) {
            FALSE
        } else
            NA

        ## The license is verified (as FOSS) if it was verified as FOSS.
        is_verified <- !is.na(is_FOSS) && is_FOSS

        ## The license does not restrict use if it is verified as FOSS,
        ## or if there is one component with a positive number of
        ## expansions all of which do not restrict use.
        ## If all components have a positive number of expansions where
        ## at least one of which restricts use, the license restricts
        ## use.
        ## Otherwise, we do not know.
        restricts_use <- if(is_verified) {
            FALSE
        } else if(any(sapply(expansions,
                             function(e)
                             (length(e) &&
                              all(e$Restricts_use == "no"))))) {
            FALSE
        } else if(all(sapply(expansions,
                             function(e)
                             any(e$Restricts_use == "yes")))) {
            TRUE
        } else
            NA

        re <- R_license_db_vars()$re_license_file
        pos <- grep(sprintf("%s$", re), components)
        if(length(pos)) {
            elements <- components[pos]
            ## Components with license file pointers.
            pointers <- sub(".*file ", "", elements)
            ## Components with license extensions.
            ind <- grepl("+", elements, fixed = TRUE)
            if(any(ind))
                extensions <-
                    data.frame(components = elements[ind],
                               extensible =
                               sapply(expansions[pos[ind]],
                                      function(e)
                                      verifiable(e$Extensible)),
                               stringsAsFactors = FALSE)
        }

        ## Replace expansions by their labels from the license db.
        ## (As these are unique, we can always easily get the full
        ## expansions back.)
        expansions <- lapply(expansions, `[[`, "Labels")
        ## Components which are "Unlimited" or "file LICEN[CS]E" have
        ## empty expansions:
        ind <- grepl(sprintf("^(Unlimited|%s)$", re), components)
        if(any(ind)) expansions[ind] <- as.list(components[ind])
        ## Components with license extensions have this dropped in the
        ## expansion.
        m <- regexpr(sprintf("\\+ *%s$", re), components)
        ind <- (m > -1L)
        expansions[ind] <-
            Map(paste, expansions[ind], regmatches(components, m))
    }

    .make_results(is_canonical = is_canonical,
                  bad_components = bad_components,
                  is_standardizable = is_standardizable,
                  standardization = standardization,
                  is_verified = is_verified,
                  components = components,
                  expansions = expansions,    
                  extensions = extensions,
                  pointers = pointers,
                  is_FOSS = is_FOSS,
                  restricts_use = restricts_use)
}

.standardize_license_components <-
function(x)
{
    with(.standardizable_license_specs_db,
         ospecs[match(x, ispecs)])
}

analyze_licenses <-
function(x, db = NULL)
{
    x <- as.character(x)
    if(!length(x)) return(NULL)
    ## As analyzing licenses is costly, only analyze the unique specs.
    v <- unique(x)
    out <- as.data.frame(do.call(rbind, lapply(v, analyze_license)),
                         stringsAsFactors = FALSE)
    pos <- match(c("is_empty", "is_canonical", "is_standardizable",
                   "is_verified", "standardization", "is_FOSS",
                   "restricts_use"),
                 names(out))
    out[pos] <- lapply(out[pos], unlist)
    ## And re-match specs to the unique specs.
    out <- out[match(x, v), ]
    rownames(out) <- NULL
    if(!is.null(db)) {
        ## db should be a package db (data frame or character matrix)
        ## with rows corresponding to the elements of x.
        cnms <- colnames(db)
        if(!is.na(pos <- match("License_is_FOSS", cnms))) {
            lif <- db[, pos]
            pos <- which(!is.na(lif))
            out$is_FOSS[pos] <- out$is_verified[pos] <-
                (lif[pos] == "yes")
            ## is_FOSS implies !restricts_use:
            pos <- pos[lif[pos] == "yes"]
            out$restricts_use[pos] <- FALSE
        }
        if(!is.na(pos <- match("License_restricts_use", cnms))) {
            lru <- db[, pos]
            pos <- which(!is.na(lru))
            out$restricts_use[pos] <- (lru[pos] == "yes")
            ## restricts_use implies !is_FOSS:
            pos <- pos[lru[pos] == "yes"]
            out$is_FOSS[pos] <- out$is_verified[pos] <- FALSE
        }
    }
    out
}

build_license_db <-
function(dir, unpacked = FALSE)
{
    CRAN <- getOption("repos")["CRAN"]
    if(missing(dir) && substring(CRAN, 1L, 7L) == "file://")
        dir <- file.path(substring(CRAN, 8L), "src", "contrib")

    fields <- c("License", "License_is_FOSS", "License_restricts_use",
                "Maintainer")
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
{
    results <- cbind(db, analyze_licenses(db$License, db))
    ## Keep License_is_FOSS and License_restricts_use columns for now,
    ## so that we can identify the is_FOSS and restricts_use values
    ## obtained from these.
    results
}

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

expand_license_spec_component_from_db <-
function(x)
{
    ## Determine the license from the db matching a license spec
    ## component.

    ldb <- R_license_db()
    ldb_vars <- R_license_db_vars()

    .numeric_version_meets_constraints_p <-
    function(version, constraints)
    {
        version <- as.numeric_version(version)
        for(term in constraints) {
            re <- ldb_vars$re_single_version_spec
            op <- sub(re, "\\1", term)
            target <- sub(re, "\\2", term)
            if(!eval(parse(text = paste("version", op, "target"))))
                return(FALSE)
        }
        TRUE
    }
    
    if(x == "Unlimited" ||
       grepl(x, ldb_vars$re_license_file))
        return(NULL)

    ## Drop possible license extension.
    x <- sub(ldb_vars$re_license_extension, "", x)

    if(grepl(re_anchor(ldb_vars$re_sss), x)) {
        pos <- ldb_vars$tab_sss[x]
        ldb[pos, ]
    }
    else if(grepl(re_anchor(ldb_vars$re_unversioned), x)) {
        pos <- ldb_vars$tab_unversioned[x]
        ldb[pos, ]
    }
    else if(grepl(re <-
                  re_anchor(ldb_vars$re_versioned_style_A),
                  x)) {
        ## Extract name/abbrev and version spec.
        v <- sub(re, "\\2", x)
        x <- sub(re, "\\1", x)
        ## First, find the matching entries matching the name/abbrev.
        pos <- ldb_vars$tab_versioned_style_A[[x]]
        entries <- ldb[pos, ]
        ## Now determine the entries satisfying the version spec.
        v <- sub("[[:space:]]*\\((.*)\\)[[:space:]]*", "\\1", v)
        if(nzchar(v)) {
            constraints <-
                unlist(strsplit(v, "[[:space:]]*,[[:space:]]*"))
            entries <-
                entries[sapply(entries$Version,
                               .numeric_version_meets_constraints_p,
                               constraints), ]
        }
        entries
    }
    else if(grepl(re_anchor(ldb_vars$re_versioned_style_B),
                  x)) {
        re <- sprintf("[[:space:]]+([Vv]ersion[[:space:]]+)?(%s)",
                      .standard_regexps()$valid_numeric_version)
        x <- sub(re, " \\2", x)
        pos <- ldb_vars$tab_versioned_style_B[x]
        ldb[pos, ]
    }
    else if(grepl(re_anchor(ldb_vars$re_versioned_style_C),
                  x)) {
        pos <- ldb_vars$tab_versioned_style_C[x]
        ldb[pos, ]
    }

}
