.standard_license_abbrevs <-
    c("GPL-2", "GPL-3",
      "LGPL-2", "LGPL-2.1", "LGPL-3",
      "Artistic-1.0", "Artistic-2.0", "BSD",
      "AGPL-3")

## Eventually, we want to have a database of standard licenses as
## classified by the FSF or the OSF.  We have basic tools for extracting
## such databases from the web, but this needs further refinement.
## Longer term, such a database should be put into tools' sysdata.rda.

.fsf_or_opensource_abbrevs <-
    c("GPL", "LGPL", "MPL", "NPL")

## License specifications found on CRAN in Aug 2007 and manually
## classified as "safe" open/free software licenses.  With ongoing
## standardiation this should gradually be eliminated.

.safe_license_specs_found_on_CRAN <-
    c("'GPL'",
      "Apache-Style Software License",
      "Artistic",
      "Artistic License", 
      "BSD",
      "BSD.",
      "CeCILL 2 (GNU GPL 2 compatible)",
      "CeCILL version 2", 
      "GNU GENERAL PUBLIC LICENSE Version 2",
      "GNU GPL (version 2 or any later version)", 
      "GNU GPL (version 2 or later)",
      "GNU GPL (version 2 or later); see the file COPYING for details", 
      "GNU GPL Version 2",
      "GNU GPL Version 2 (or later)",
      "GNU GPL Version 2 or newer.", 
      "GNU GPL v. 2.0 or later",
      "GNU GPL v2.0 or greater",
      "GNU GPL version 2", 
      "GNU GPL version 2 or newer",
      "GNU GPL version 2.",
      "GNU GPL2; http://www.fsf.org/licenses/gpl.html", 
      "GNU General Public License",
      "GNU General Public License 2.0.",
      "GNU General Public License Version 2 or higher.", 
      "GNU General Public License version 2 or newer",
      "GNU Public Licence 2.0 or above at your convenience", 
      "GNU Public License",
      "GNU-license 2.0 or newer",
      "GPL version 2 or later", 
      "GPL ( version 2 or later)",
      "GPL (GNU Public Licence) version 2 or later", 
      "GPL (GNU Public Licence), Version 2 or later",
      "GPL (Version 2 or above)", 
      "GPL (Version 2 or later)",
      "GPL (Version 2.0 or later)",
      "GPL (see COPYING)", 
      "GPL (v.2 or later)",
      "GPL (version 2 or higher)",
      "GPL (version 2 or higher; see LICENSE)", 
      "GPL (version 2 or later)",
      "GPL (version 2 or later) See file LICENCE", 
      "GPL (version 2 or later) See file LICENCE.",
      "GPL (version 2 or later) See file LICENSE.", 
      "GPL (version 2 or later), see also LICENSE file",
      "GPL (version 2 or later).", 
      "GPL (version 2 or later, see the included file GPL)",
      "GPL (version 2 or newer)", 
      "GPL (version 2)",
      "GPL (version 2.0 or later)",
      "GPL 2",
      "GPL 2 or above", 
      "GPL 2 or later",
      "GPL 2 or newer",
      "GPL 2, or newer.",
      "GPL 2.0", 
      "GPL 2.0 or above",
      "GPL 2.0 or higher",
      "GPL 2.0 or later", 
      "GPL 2.0 or newer",
      "GPL 3",
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
      "GPL version 2 o newer",
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
      "GPL version 2.0",
      "GPL version 2.0 or later",
      "GPL version 2.1 or newer", 
      "GPL version 3 or newer",
      "GPL vesion 2 or newer",
      "GPL, version 2 or newer", 
      "GPL, version 2 or newer.",
      "GPL-2, or see file LICENCE",
      "GPL2", 
      "GPL2.0",
      "Gnu GPL",
      "LGPL (Lesser GNU Public Licence)",
      "LGPL (Lesser GNU Public License)", 
      "LGPL (see <http://www.opensource.org/licenses/lgpl-license.php>).", 
      "LGPL 2.1",
      "LGPL >= 2.0",
      "LGPL Version 2 or later.",
      "LGPL Version 2.1 or later", 
      "LGPL version 2",
      "LGPL version 2 or newer",
      "LGPL version 2.1 <http://www.gnu.org/copyleft/lesser.html>", 
      "LGPL version 2.1 or later",
      "LGPL version 2.1 or newer (the releases)", 
      "LGPL2",
      "Lesser GPL Version 2 or later.",
      "MIT",
      "Mozilla Public License 1.1 (http://www.mozilla.org/MPL/)", 
      "Public Domain, unlimited distribution",
      "Public domain",
      "Standard GNU public license", 
      "The Artistic License, Version 2.0",
      "The Gnu general public liscense, current version, 2/12/2004.", 
      "Unlimited distribution.",
      "Unlimited use and distribution (see LICENCE).", 
      "X11 (http://www.x.org/Downloads_terms.html)",
      "use under GPL2, or see file LICENCE"
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
    ## The specs in R-exts currently say a bit sloppily that any license
    ## db name or abbrev can be combined with a version restriction.
    ## But of course, this only makes sense provided that the name of
    ## the license does not include a version ...
    ## <FIXME>
    ## In any case, we currently do not have/use a full database of
    ## open/free software licenses ...
    ##   license_names_or_abbrevs <-
    ##       Filter(nzchar,
    ##              unlist(.opensource_license_db[c("Name", "Abbrv")],
    ##                     use.names = FALSE))
    license_names_or_abbrevs <- .fsf_or_opensource_abbrevs
    ## </FIXME>
    contains_numeric_spec <-
        regexpr(paste("[[:space:]][vV]?",
                      re_for_numeric_version,
                      "$",
                      sep = ""),
                license_names_or_abbrevs) > -1
    re_for_opensource_spec <-
        re_or(c(re_or(license_names_or_abbrevs[contains_numeric_spec]),
                paste(re_or(license_names_or_abbrevs[!contains_numeric_spec]),
                      "[[:space:]]*",
                      paste("(", re_for_version_spec, ")*", sep = ""),
                      sep = "")))
    re_for_standard_abbrevs <- re_or(.standard_license_abbrevs)
    re_for_license_file <- "file LICEN[CS]E"
    re_for_component <-
        paste("^",
              re_or(c(re_for_standard_abbrevs,
                      re_for_opensource_spec,
                      re_for_license_file,
                      "Unlimited")),
              "$",
              sep = "")

    x <- tools:::.strip_whitespace(x)
    if(x == "") {
        ## Not really a lot to check ...
        return(.make_results(is_empty = TRUE))
    }

    ## Try splitting into the individual components.
    components <-
        tools:::.strip_whitespace(unlist(strsplit(x, "|", fixed = TRUE)))
    
    ## Now analyze the individual components.
    ok <- regexpr(re_for_component, components) > -1

    pointers <- if(!all(ok)) NULL else {
        ind <- regexpr(paste("^", re_for_license_file, "$", sep = ""),
                       components) > -1
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
                         components) == -1))
         || all(components %in% .safe_license_specs_found_on_CRAN))

    .make_results(is_canonical = all(ok),
                  bad_components = components[!ok],
                  is_verified = is_verified,
                  pointers = pointers)
}

check_installed_licenses <-
function()
{
    db <- lapply(installed.packages()[, "Package"],
                 function(p) {
                     txt <- packageDescription(p)$License
                     c(list(license = txt), analyze_license(txt))
                 })
    ## And now try turning into a data frame ... argh.
    out <- data.frame(license = sapply(db, "[[", "license"),
                      stringsAsFactors = FALSE)
    for(nm in names(db[[1L]][-1L]))
        out[[nm]] <- sapply(db, "[[", nm)
    out
}
