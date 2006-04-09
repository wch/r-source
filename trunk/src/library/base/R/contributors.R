contributors <- function()
{
    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    writeLines(paste("R is a project which is attempting to provide a ",
                     "modern piece of\nstatistical software for the ",
                     "GNU suite of software.\n\n",
                     "The current R is the result of a collaborative ",
                     "effort with\ncontributions from all over the ",
                     "world.\n\n",
                     sep = ""), outConn)
    writeLines(readLines(file.path(R.home("doc"), "AUTHORS")), outConn)
    writeLines("", outConn)
    writeLines(readLines(file.path(R.home("doc"), "THANKS")), outConn)
    close(outConn)
    file.show(outFile, delete.file = TRUE)
}
