## Tools for computing on CITATION info.
## Currently only for validation, and hence not in utils.

BibTeX_entry_field_db <-
    list(Article = c("author", "title", "journal", "year"),
         Book = c("author|editor", "title", "publisher", "year"),
         Booklet = c("title"),
         InBook =
         c("author|editor", "title", "chapter", "publisher", "year"),
         InCollection =
         c("author", "title", "booktitle", "publisher", "year"),
         InProceedings = c("author", "title", "booktitle", "year"),
         Manual = c("title"),
         MastersThesis = c("author", "title", "school", "year"),
         Misc = character(),
         PhdThesis = c("author", "title", "school", "year"),
         Proceedings = c("title", "year"),
         TechReport = c("author", "title", "institution", "year"),
         Unpublished = c("author", "title", "note")
         )
## See e.g. lisp/textmodes/bibtex.el in the GNU Emacs sources.

## Keep in step with utils::readCitationFile
get_CITATION_entry_fields <-
function(file, encoding = "ASCII")
{
    ## Assume that bibentry() or citEntry() only occur at top level.

    if(encoding %in% c("latin1", "UTF-8") && !l10n_info()$MBCS) {
        exprs <- tryCatch(parse(file = file, encoding = encoding),
                          error = identity)
    } else {
        con <- file(file, encoding = encoding)
        on.exit(close(con))
        exprs <- tryCatch(parse(con), error = identity)
    }
    if(inherits(exprs, "error")) return()

    ## Try to detect entry type and field names from the calls.
    FOO1 <- FOO2 <- function() match.call(expand.dots = FALSE)
    formals(FOO1) <- formals(utils::citEntry)
    formals(FOO2) <- formals(utils::bibentry)
    ## Could also hard-wire this, of course.
    out <- lapply(exprs,
           function(e) {
               nm <- as.character(e[[1L]])
               if(nm == "citEntry") {
                   e[[1L]] <- as.name("FOO1")
                   e <- as.list(eval(e))
                   entry <- e$entry
                   fields <- names(e$...)
               }
               else if(nm == "bibentry") {
                   e[[1L]] <- as.name("FOO2")
                   e <- as.list(eval(e))
                   entry <- e$bibtype
                   fields <- c(names(e$...), names(e$other)[-1L])
               }
               else return()
               entry <- if(!is.character(entry)) NA_character_ else entry[1L]
               list(entry = entry, fields = fields)
           })

    out <- Filter(Negate(is.null), out)
    entries <- sapply(out, `[[`, 1L)
    fields <- lapply(out, `[[`, 2L)
    out <- data.frame(File = file,
                      Entry = entries,
                      stringsAsFactors = FALSE)
    out$Fields <- fields
    out
}


find_missing_required_BibTeX_fields <-
function(entry, fields)
{
    if(!length(fields)) return(character())
    pos <- match(tolower(entry),
                 tolower(names(BibTeX_entry_field_db)))
    if(is.na(pos)) {
        ## Invalid entry.
        return(NA_character_)
    }
    rfields <- BibTeX_entry_field_db[[pos]]
    if(!length(rfields)) return(character())
    ## Go for legibility/generality rather than efficiency.
    ok <- sapply(strsplit(rfields, "|", fixed = TRUE),
                 function(f) any(f %in% fields))
    rfields[!ok]
}
