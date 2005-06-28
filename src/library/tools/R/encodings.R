get_IANA_character_sets <-
function(file = NULL)
{
    ## Master URI is
    ##   http://www.iana.org/assignments/character-sets
    if(is.null(file))
        file <- file.path(R.home("share"), "encodings",
                          "character-sets")
    lines <- readLines(file)
    ## Start with first Name: entry, and end with REFERENCES.
    spos <- min(grep("^Name:", lines))
    epos <- min(grep("^REFERENCES", lines)) - 1
    lines <- lines[spos : epos]
    ## Omit 'Alias: None' and similar lines.
    if(any(ind <- grep("^[[:alnum:]]+:[[:space:]]+None[[:space:]]*$",
                       lines)))
        lines <- lines[-ind]
    entries <- paste(lines, collapse = "\n")
    ## What we now have is in DCF format.  Ideally, we would like to use
    ## read.dcf(), but this (currently?) allows only a single field
    ## entry per record, and we have multiple aliases ...
    con <- textConnection(entries)
    on.exit(close(con))
    db <- read.dcf(con, fields = c("Name", "MIBenum", "Source"))
    ## Now do the dirty work ...
    entries <- strsplit(strsplit(entries, "\n[[:space:]]*\n")[[1]], "\n")
    Aliases <-
        lapply(entries,
               function(u) {
                   if(any(ind <- grep("^Alias:", u)))
                       sapply(strsplit(u[ind], " +"), "[[", 2)
                   else
                       character()
               })
    MIME <-
        sapply(entries,
               function(u) {
                   if(any(ind <- grep("preferred MIME name", u)))
                       sapply(strsplit(u[ind], " +"), "[[", 2)
                   else
                       character()
               })

    data.frame(Name = I(sub(" +.*", "", db[, "Name"])),
               MIBenum = as.integer(db[, "MIBenum"]),
               Source = I(db[, "Source"]),
               Aliases = I(Aliases),
               MIME = I(MIME))
}
