contributors <- function()
{
    FILE <- tempfile()
    on.exit(unlink(FILE))
    cat("R is a project which is attempting to provide a modern piece",
        "of statistical software for the GNU suite of software.\n",
        "The current R is the result of a collaborative effort with",
        "contributions from all over the world.\n\n",
        file = FILE, sep = "\n")
    file.append(FILE, file.path(R.home(), "AUTHORS"))
    cat("\n", file = FILE, append = TRUE)
    file.append(FILE, file.path(R.home(), "THANKS"))
    file.show(FILE)
}
