cat <- function(..., file = "", sep = " ", fill = FALSE,
                labels = NULL, append = FALSE)
{
    if(is.character(file))
        if(file == "") file <- stdout()
        else if(substring(file, 1, 1) == "|") {
            file <- pipe(substring(file, 2), "w")
            on.exit(close(file))
        } else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
}
