.show_help_on_topic_as_HTML <-
function(file, topic)
{
    browseURL(file)
    writeLines(paste("Help for", sQuote(topic), "is shown in the browser"))
    return(invisible())
}

.show_help_on_topic_offline <-
function(file, topic)
{
    FILE <- "Rdoc" # must be in the current dir
    con <- paste(FILE, ".tex", sep = "")
    cat("\\documentclass[",
        getOption("papersize"),
        "paper]{article}",
        "\n",
        "\\usepackage[",
        if(nchar(opt <- Sys.getenv("R_RD4DVI"))) opt else "ae",
        "]{Rd}",
        "\n",
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\begin{document}\n",
        file = con, sep = "")
    file.append(con, file)
    cat("\\end{document}\n", file = con, append = TRUE)
    cmd <- paste('"',
                 paste(R.home(), "bin", "helpPRINT", sep="/"),
                 '"', sep="")
    texpath <- chartr("\\", "/",
                      file.path(R.home(), "share", "texmf"))
    system(paste(cmd, FILE, topic, texpath), wait = FALSE)
    return(invisible())
}
