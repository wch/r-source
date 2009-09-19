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

get_CITATION_entry_fields <-
function(file, encoding = "unknown")
{
    ## Assume that citEntry() only occurs at top level.

    ## The usual encoding nightmare.
    ## readCitationFile() uses the package encoding, or if this is
    ## unset, latin1.  Let's simply try latin1 and UTF-8 if simple
    ## parsing fails in case there is no given encoding.

    exprs <- tryCatch(parse(file(file, encoding = encoding)),
                      error = identity)
    if(inherits(exprs, "error")) {
        if(encoding != "unknown")
            return()
        exprs <- tryCatch(parse(file(file, encoding = "latin1")),
                          error = identity)
        if(inherits(exprs, "error"))
            exprs <- tryCatch(parse(file(file, encoding = "UTF-8")),
                          error = identity)
        if(inherits(exprs, "error"))
            return()
    }

    ## Argh.  citEntry() has formals
    ##   (entry, textVersion, header = NULL, footer = NULL, ...)
    ## so we cannot simply compute of the names of the citEntry() calls.
    FOO <- function(entry, textVersion, header = NULL, footer = NULL, ...)
        match.call()

    out <- lapply(exprs,
           function(e) {
               if(as.character(e[[1L]]) != "citEntry") return()
               e[[1L]] <- as.name("FOO")
               e <- as.list(eval(e))
               entry <- e$entry
               entry <- if(!is.character(entry)) NA_character_ else entry[1L]
               fields <- names(e)
               ## Retain fields textVersion/header/footer, so these must
               ## be removed in subsequent BibTeX validation computations.
               fields <- fields[is.na(match(fields, c("", "entry")))]
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
    ## Drop non-BibTeX citEntry() fields.
    fields <- tolower(fields[!fields %in%
                             c("textVersion", "header", "footer")])
    ## Go for legibility/generality rather than efficiency.
    ok <- sapply(strsplit(rfields, "|", fixed = TRUE),
                 function(f) any(f %in% fields))
    rfields[!ok]
}
