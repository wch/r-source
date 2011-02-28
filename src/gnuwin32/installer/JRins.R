#  File src/gnuwin32/installer/JRins.R
#
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

### JRins.R Rversion srcdir MDISDI HelpStyle Internet Producer

.make_R.iss <- function(RW, srcdir, MDISDI=0, HelpStyle=1, Internet=0,
                       Producer = "R-core")
{
    have32bit <- file_test("-d", file.path(srcdir, "bin", "i386"))
    have64bit <- file_test("-d", file.path(srcdir, "bin", "x64"))

    ## need DOS-style paths
    srcdir = gsub("/", "\\", srcdir, fixed = TRUE)

    Rver <- readLines("../../../VERSION")[1L]
    Rver <- sub("Under .*$", "Pre-release", Rver)
    SVN <- sub("Revision: ", "", readLines("../../../SVN-REVISION"))[1L]
    Rver0 <- paste(sub(" .*$", "", Rver), SVN, sep = ".")


    con <- file("R.iss", "w")
    cat("[Setup]\n", file = con)

    if (have64bit) {
        regfile <- "reg3264.iss"
        types <- "types3264.iss"
        cat("ArchitecturesInstallIn64BitMode=x64\n", file = con)
    } else { # 32-bit only
        regfile <- "reg.iss"
        types <- "types32.iss"
    }
    suffix <- "win"

    cat(paste("OutputBaseFilename=", RW, "-", suffix, sep = ""),
        paste("AppName=R for Windows ", Rver, sep = ""),
        paste("AppVerName=R for Windows ", Rver, sep = ""),
        paste("AppVersion=", Rver, sep = ""),
        paste("VersionInfoVersion=", Rver0, sep = ""),
        paste("DefaultDirName={code:UserPF}\\R\\", RW, sep = ""),
        paste("InfoBeforeFile=", srcdir, "\\COPYING", sep = ""),
        if(Producer == "R-core") "AppPublisher=R Development Core Team"
        else paste("AppPublisher=", Producer, sep = ""),
        file = con, sep = "\n")

    writeLines(readLines("header1.iss"), con)

    lines <- readLines(regfile)
    lines <- gsub("@RVER@", Rver, lines)
    lines <- gsub("@Producer@", Producer, lines)
    writeLines(lines, con)

    lines <- readLines(types)
    if(have64bit && !have32bit) {
        lines <- lines[-c(3,4,6,13,23)]
        lines <- gsub("user(32)* ", "", lines)
        lines <- gsub("compact ", "", lines)
    }
    writeLines(lines, con)

    lines <- readLines("code.iss")
    lines <- gsub("@MDISDI@", MDISDI, lines)
    lines <- gsub("@HelpStyle@", HelpStyle, lines)
    lines <- gsub("@Internet@", Internet, lines)
    writeLines(lines, con)

    writeLines(c("", "", "[Files]"), con)

    setwd(srcdir)
    files <- sub("^./", "",
                 list.files(".", full.names = TRUE, recursive = TRUE))
    for (f in files) {
	dir <- sub("[^/]+$", "", f)
	dir <- paste("\\", gsub("/", "\\", dir, fixed = TRUE), sep = "")
	dir <- sub("\\\\$", "", dir)

	## These manuals are on the Rgui menu, so should always be installed
        if (f %in%  c("doc/manual/R-FAQ.html",
                      "doc/html/rw-FAQ.html",
                      "share/texmf/Sweave.sty"))
            component <- "main"
	else if (grepl("^doc/html", f) || grepl("^library/[^/]*/html", f))
	    component <- "main"
	else if (grepl("^doc/manual/[^/]*\\.html", f))
	    component <- "html"
	else if (f %in% c("doc/manual/R-data.pdf", "doc/manual/R-intro.pdf"))
	    component <- "manuals/basic"
	else if (f %in% c("doc/manual/R-admin.pdf",
                          "doc/manual/R-exts.pdf",
                          "doc/manual/R-ints.pdf",
                          "doc/manual/R-lang.pdf"))
	    component <- "manuals/technical"
	else if (f == "doc/manual/refman.pdf")
	    component <- "manuals/refman"
	else if (grepl("^doc/manual", f) && f != "doc/manual/R-FAQ.pdf")
	    component <- "manuals"
	else if (grepl("^library/[^/]*/tests", f) || grepl("^tests", f))
	    	component <- "tests"
	else if (grepl("^Tcl/(bin|lib)64", f))
	    component <- "tcl/64"
	else if (have64bit &&
                 (grepl("^Tcl/bin", f) ||
                  grepl("^Tcl/lib/(dde1.3|reg1.2|Tktable)", f)))
	    component <- "tcl/32"
	else if (grepl("^Tcl/doc/.*chm$", f))
	    component <- "tcl/chm"
	else if (grepl("^Tcl/lib/tcl8.5/tzdata", f))
	    component <- "tcl/tzdata"
	else if (grepl("^Tcl/.*\\.msg$", f))
	    component <- "tcl/msg"
	else if (grepl("^Tcl", f))
	    component <- "tcl/noarch"
	else if (grepl("^library/grid/doc", f) ||
                 grepl("^library/survival/doc", f) ||
                 grepl("^library/Matrix/doc", f))
	    component <- "manuals/libdocs"
	else if (grepl("^share/locale", f) ||
                 grepl("^library/[^/]*/po", f))
	    component <- "trans"
	else if (grepl("/i386/", f))
            component <- "i386"
	else if (grepl("/x64/", f))
            component <- "x64"
	else
            component <- "main"

        if (component == "x64" && !have64bit) next

        f <- gsub("/", "\\", f, fixed = TRUE)
        cat('Source: "', srcdir, '\\', f, '"; ',
            'DestDir: "{app}', dir, '"; ',
            'Flags: ignoreversion; ',
            'Components: ', component,
            file = con, sep = "")
        if(f %in% c("etc\\Rprofile.site", "etc\\Rconsole"))
            cat("; AfterInstall: EditOptions()", file = con)
        cat("\n", file = con)
    }

    close(con)
}


args <- commandArgs(TRUE)
do.call(".make_R.iss", as.list(args))

