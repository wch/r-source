bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report")
{
    body <- paste("\\n<<insert bug report here>>\\n\\n\\n\\n",
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
		  paste(names(version), version, sep=" = ", collapse="\\n "),
                  "\\n\\n",
                  win.version(),
		  "\\n\\n",
                  "Locale:\\n",
                  Sys.getlocale(),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")

    disclaimer <-
        paste("# R for Windows will not send your bug report automatically.\n",
              "# Please copy the bug report (after finishing it) to\n",
              "# your favorite email program and send it to\n#\n",
              "#       ", address, "\n#\n",
              "######################################################\n",
              "\n\n", sep = "")

    cat(disclaimer, file=file)
    body <- gsub("\\\\n", "\n", body)
    cat(body, file=file, append=TRUE)
    file.edit(file)
    cat("The unsent bug report can be found in file", tools::file_path_as_absolute(file), "\n")
    invisible()
}

win.version <- function() .Internal(win.version())
