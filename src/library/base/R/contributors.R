contributors <- function()
{
    FILE <- tempfile()
    out <- file(FILE, open = "w")
    on.exit({close(out); unlink(FILE)})
    writeLines(paste("R is a project which is attempting to provide a ",
                     "modern piece of\nstatistical software for the ",
                     "GNU suite of software.\n\n",
                     "The current R is the result of a collaborative ",
                     "effort with\ncontributions from all over the ",
                     "world.\n\n",
                     sep = ""), out)
    writeLines(readLines(file.path(R.home(), "AUTHORS")), out)
    writeLines("", out)
    writeLines(readLines(file.path(R.home(), "THANKS")), out)
    file.show(FILE, delete.file = TRUE)
}
