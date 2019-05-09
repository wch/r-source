make_sysdata_rda <-
function()    
{
    ## Get the codes and descriptions from the
    ##   <http://www.loc.gov/marc/relators/relaterm.html>
    ## web page, and merge with the information on usage for R.

    ## Codes are also listed at
    ##   <http://www.loc.gov/marc/relators/relacode.html>
    ## which also contains discontinued codes, identified by a hyphen
    ## preceding the code.

    ## <http://www.loc.gov/marc/relators/relaterm.html>
    doc <- xml2::read_html("http://www.loc.gov/marc/relators/relaterm.html")
    dt <- trimws(xml2::xml_text(xml2::xml_find_all(doc, "//dl[4]//dt")))
    dd <- trimws(xml2::xml_text(xml2::xml_find_all(doc, "//dl[4]//dd")))

    ## Drop obsolete stuff and pointers to it.
    re <- "(.*) \\[(.*)\\]"
    ind <- grepl(re, dt)
    MARC_relator_db <-
        as.data.frame(cbind(term = sub(re, "\\1", dt[ind]),
                            code = sub(re, "\\2", dt[ind]),
                            description = sub("\n\t UF.*", "", dd[ind]),
                            usage = ""),
                      stringsAsFactors = FALSE)

    MARC_R_usage <-
        c("aut" = "Use for full authors who have made substantial contributions to\nthe package and should show up in the package citation.",
          "com" = "Use for package maintainers that collected code (potentially in\nother languages) but did not make further substantial\ncontributions to the package.",
          "cph" = "Use for all copyright holders.",
          "cre" = "Use for the package maintainer.",
          "ctb" = "Use for authors who have made smaller contributions (such as\ncode patches etc.) but should not show up in the package\ncitation.",
          "ctr" = "Use for authors who have been contracted to write (parts of) the\npackage and hence do not own intellectual property.",
          "dtc" = "Use for persons who contributed data sets for the package.",
          "fnd" = "Use for persons or organizations that furnished financial support\nfor the development of the package",
          "rev" = "Use for persons or organizations responsible for reviewing\n(parts of) the package.",
          "ths" = "If the package is part of a thesis, use for the thesis advisor.", 
          "trl" = "If the R code is merely a translation from another language\n(typically S), use for the translator to R.")

    MARC_R_usage <- data.frame(code = names(MARC_R_usage),
                               usage = c(MARC_R_usage),
                               stringsAsFactors = FALSE,
                               row.names = NULL)

    pos <- match(MARC_relator_db$code, MARC_R_usage$code, nomatch = 0L)
    MARC_relator_db$usage[pos > 0L] <-
        gsub("\n", " ", MARC_R_usage$usage[pos], fixed=TRUE)

    MARC_relator_db_codes_used_with_R <-
        sort(MARC_relator_db[nchar(MARC_relator_db$usage) > 0L, "code"])

    ## Using dump() to provide a plain text representation results in
    ## non-ASCII string constants without appropriate encoding info.
    ## c69417 manually changed to Unicode escapes, which however is not
    ## straightforward to achieve programmatically.  Hence, instead of
    ## using dump() to a text 'MARC.R' file use save() to a binary
    ## 'sysdata.rda' file.

    save(MARC_relator_db,
         MARC_relator_db_codes_used_with_R,
         file = "sysdata.rda",
         compress = TRUE)
}
