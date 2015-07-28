make_sysdata_rda <-
function()    
{
    IANA_URI_scheme_db <- get_IANA_URI_scheme_db()

    IANA_HTTP_status_code_db <- get_IANA_HTTP_status_code_db()

    ## See <http://en.wikipedia.org/wiki/List_of_HTTP_status_codes>.
    table_of_HTTP_status_codes <- IANA_HTTP_status_code_db$Description
    names(table_of_HTTP_status_codes) <- IANA_HTTP_status_code_db$Value

    save(IANA_URI_scheme_db,
         IANA_HTTP_status_code_db,
         table_of_HTTP_status_codes,
         file = "sysdata.rda",
         compress = TRUE)
}
