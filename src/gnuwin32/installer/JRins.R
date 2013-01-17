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

### JRins.R Rversion srcdir MDISDI HelpStyle Internet Producer ISDIR

.make_R.iss <- function(RW, srcdir, MDISDI=0, HelpStyle=1, Internet=0,
                       Producer = "R-core", ISDIR)
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
        if(Producer == "R-core") "AppPublisher=R Core Team"
        else paste("AppPublisher=", Producer, sep = ""),
        file = con, sep = "\n")

    ## different versions of the installer have different translation files
    lines <- readLines("header1.iss")
    check <- grepl("Languages\\", lines, fixed = TRUE)
    langs <- sub(".*\\\\", "", lines[check])
    langs <- sub('"$', "", langs)
    avail <- dir(file.path(ISDIR, "Languages"), pattern = "[.]isl$")
    drop <- !(langs %in% avail)
    if(any(drop))
        lines <- grep(paste0("(", paste(langs[drop], collapse = "|"), ")"),
                      lines, value = TRUE, invert = TRUE)
    writeLines(lines, con)

    lines <- readLines(regfile)
    lines <- gsub("@RVER@", Rver, lines)
    lines <- gsub("@Producer@", Producer, lines)
    writeLines(lines, con)

    lines <- readLines(types)
    if(have64bit && !have32bit) {
        lines <- lines[-c(3,4,10)]
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

	component <- if (grepl("^Tcl/(bin|lib)64", f)) "x64"
	else if (have64bit &&
                 (grepl("^Tcl/bin", f) ||
                  grepl("^Tcl/lib/(dde1.3|reg1.2|Tktable)", f))) "i386"
	else if (grepl("/i386/", f)) "i386"
	else if (grepl("/x64/", f)) "x64"
	else if (grepl("(/po$|/po/|/msgs$|/msgs/|^library/translations)", f))
            "translations"
	else "main"

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

