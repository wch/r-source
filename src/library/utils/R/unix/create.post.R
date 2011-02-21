#  File src/library/utils/R/unix/create.post.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

bug.report.info <- function() {
    paste("R Version:\\n ",
    paste(names(R.version),R.version, sep=" = ",collapse="\\n "),
    if (nzchar(Sys.getenv("R_GUI_APP_VERSION")))
      paste("\\n\\nGUI:\\n R-GUI ",Sys.getenv("R_GUI_APP_VERSION"),
	    " (",Sys.getenv("R_GUI_APP_REVISION"),")",sep='')
    else
      ""
    ,
    "\\n\\n",
    "Locale:\\n",
    Sys.getlocale(),
    "\\n\\n",
    "Search Path:\\n ",
    paste(search(), collapse=", "),
    "\\n", sep="", collapse="")
}

create.post <- function(instructions = "\\n",
                        description = "post",
                        subject = "",
                        ccaddress = getOption(ccaddress),
                        method = getOption("mailer"),
                        address = "the relevant mailing list",
                        file = "R.post",
                        info = NULL)
{
    method <-
	if(is.null(method)) "none"
	else match.arg(method, c("mailx", "gnudoit", "none", "ess"))

    body <- paste(instructions,
		  "--please do not edit the information below--\\n\\n",
		  info,
		  bug.report.info(),
		  sep="", collapse="")

    none_method <- function() {
        disclaimer <-
            paste("# Your mailer is set to \"none\",\n",
                  "# hence we cannot send the, ", description, " directly from R.\n",
                  "# Please copy the ", description, " (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")

        cat(c(disclaimer, gsub("\\\\n", "\n", body)), file=file)
        cat("The", description, "is being opened for you to edit.\n")
        file.edit(file)
        cat("The unsent ", description, " can be found in file",
            sQuote(file), "\n", sep ="")
    }

    if(method == "gnudoit") {
	cmd <- paste("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", body, "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'",
		     sep="")
	system(cmd)
    } else if(method=="none") {
        none_method()
    } else if(method == "mailx") {
        if(missing(address)) stop("must specify 'address'")
        if(missing(subject)) stop("'subject' missing")
        if(length(ccaddress) != 1L) stop("'ccaddress' must be of length 1")

	cat(gsub("\\\\n", "\n", body), file=file)
        cat("The", description, "is being opened for you to edit.\n")
        file.edit(file)

        if(is.character(ccaddress) && nzchar(ccaddress)) {
            cmdargs <- paste("-s", shQuote(subject),
                             "-c", shQuote(ccaddress),
                             shQuote(address),
                             "<", file, "2>/dev/null")
        }
        else
            cmdargs <- paste("-s", shQuote(subject),
                             shQuote(address), "<",
                             file, "2>/dev/null")
        status <- 1L
        answer <- readline(paste("Email the ", description, " now? (yes/no) ",
                                 sep = ""))
        answer <- grep("yes", answer, ignore.case=TRUE)
        if(length(answer)) {
            cat("Sending email ...\n")
            status <- system(paste("mailx", cmdargs), , TRUE, TRUE)
            if(status)
                status <- system(paste("Mail", cmdargs), , TRUE, TRUE)
            if(status)
                status <- system(paste("/usr/ucb/mail", cmdargs), , TRUE, TRUE)

            if(status == 0L) unlink(file)
            else {
                cat("Sending email failed!\n")
                cat("The unsent", description, "can be found in file",
                    sQuote(file), "\n")
            }
        } else
            cat("The unsent", description, "can be found in file", file, "\n")
    } else if(method == "ess") {
	body <- gsub("\\\\n", "\n", body)
	cat(body)
    } else if(method == "mailto") {
        if (missing(address)) stop("must specify 'address'")
        if (!nzchar(subject)) subject <- "<<Enter Meaningful Subject>>"
        if(length(ccaddress) != 1L) stop("'ccaddress' must be of length 1")
        cat("The", description, "is being opened in your default mail program\nfor you to complete and send.\n")
        arg <- paste("mailto:", address,
                     "?subject=", subject,
                     if(is.character(ccaddress) && nzchar(ccaddress))
                         paste("&cc=", ccaddress, sep=""),
                     "&body=", body0 <- gsub("\\\\n", "\n", body), sep = "")
        open_prog <- if(grepl("-apple-darwin", R.version$platform)) "open" else "xdg-open"
 	res <- system2(open_prog, shQuote(arg))
        if(res) {
            cat("opening the mailer failed, so reverting to 'mailer=\"none\"'\n")
            none_method()
        }
    }
}
