bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report")
{
    methods <- c("mailx", "gnudoit", "none", "ess")

    method <-
	if(is.null(method)) "none"
	else methods[pmatch(method, methods)]

    body <- paste("\\n<<insert bug report here>>\\n\\n\\n\\n",
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
		  paste(names(R.version),R.version, sep=" = ",collapse="\\n "),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")

    if(method == "gnudoit") {
	cmd <- paste("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", body, "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'",
		     sep="")
	system(cmd)
    }
    else if(method=="none"){

        disclaimer <-
            paste("# Your mailer is set to \"none\" (default on Windows),\n",
                  "# hence we cannot send the bug report directly from R.\n",
                  "# Please copy the bug report (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")


        cat(disclaimer, file=file)
	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=TRUE)
	system(paste(getOption("editor"), file))
        cat("The unsent bug report can be found in file", file, "\n")
    }
    else if(method == "mailx"){

        if(missing(subject)) stop("'subject' missing")

	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=FALSE)
	system(paste(getOption("editor"), file))

        if(is.character(ccaddress) && nchar(ccaddress)>0) {
            cmdargs <- paste("-s '", subject, "' -c", ccaddress,
                             address, "<", file, "2>/dev/null")
        }
        else
            cmdargs <- paste("-s '", subject, "'", address, "<",
                             file, "2>/dev/null")

        status <- 1

        cat("Submit the bug report? ")
        answer <- readline()
        answer <- grep("y", answer, ignore.case=TRUE)
        if(length(answer)>0){
            cat("Sending email ...\n")
            status <- system(paste("mailx", cmdargs))
            if(status > 0)
                status <- system(paste("Mail", cmdargs))
            if(status > 0)
                status <- system(paste("/usr/ucb/mail", cmdargs))

            if(status==0) unlink(file)
            else{
                cat("Sending email failed!\n")
                cat("The unsent bug report can be found in file",
                    file, "\n")
            }

        }
        else
            cat("The unsent bug report can be found in file",
                file, "\n")

    }
    else if(method=="ess"){
	body <- gsub("\\\\n", "\n", body)
	cat(body)
    }
}
