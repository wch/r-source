contributors <- function()
{
    FILE <- tempfile()
    out <- file(FILE, open = "a")
    cat("R is a project which is attempting to provide a modern piece",
        "of statistical software for the GNU suite of software.\n",
        "The current R is the result of a collaborative effort with",
        "contributions from all over the world.\n\n",
        file = out, sep = "\n")
    writeLines(readLines(file.path(R.home(), "AUTHORS")), out)
    cat("\n", file = FILE, append = TRUE)
    writeLines(readLines(file.path(R.home(), "THANKS")), out)
    close(out)
    file.show(FILE, delete.file = TRUE)
}
