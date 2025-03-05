#  File src/gnuwin32/installer/JRins.R
#
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

### JRins.R Rversion srcdir MDISDI HelpStyle Producer ISDIR

.make_R.iss <- function(RW, srcdir, MDISDI=0, HelpStyle=1,
                       Producer = "R-core", ISDIR)
{
    ## The layout of 64-bit Intel builds is different so that it matches
    ## previous versions of R which supported sub-architectures (32-bit and
    ## 64-bit Intel) and installing files for both at the same time.

    havex64 <- file_test("-d", file.path(srcdir, "bin", "x64"))

    ## need DOS-style paths
    srcdir = gsub("/", "\\", srcdir, fixed = TRUE)

    Rver <- readLines("../../../VERSION")[1L]
    Rver <- sub("Under .*$", "Pre-release", Rver)
    ## This is now over 2^16, so truncate
    SVN <- sub("Revision: ", "", readLines("../../../SVN-REVISION"))[1L]
    SVN <- as.character(as.numeric(SVN) - 50000L)
    Rver0 <- paste(sub(" .*$", "", Rver), SVN, sep = ".")


    con <- file("R.iss", "w")
    cat("[Setup]\n", file = con)

    aarch64 <- FALSE
    if (havex64) { # 64-bit Intel only
        regfile <- "reg64.iss"
        types <- "types64.iss"
        cat("ArchitecturesInstallIn64BitMode=x64\n", file = con)
    } else {
        regfile <- "reg.iss"
        types <- "types.iss"

        fout <- system(
                    paste("file", shQuote(file.path(srcdir, "bin", "R.exe"))),
                    intern=TRUE)
        if (grepl("x86-64", fout, fixed = TRUE))
            cat("ArchitecturesInstallIn64BitMode=x64 arm64\n", file = con)
        else if (grepl("Aarch64", fout, fixed = TRUE)
                 || grepl("ARM64", fout, fixed = TRUE)) {
            cat("ArchitecturesInstallIn64BitMode=arm64\n", file = con)
            cat("ArchitecturesAllowed=arm64\n", file = con)
            aarch64 <- TRUE
        }
    }
    suffix <- "win"
    if (aarch64) {
       # To distinguish aarch64 version from x86_64 version installed on
       # Windows/aarch64 in ARP entries and in shortcuts
       RverA <- paste0(Rver, " (aarch64)")

       # Inno Setup installs an x86_64 application on Windows 11/aarch64
       # into "Program Files (x86)", but that doesn't seem to follow Windows
       # conventions, so use a suffix just in case it is fixed at some
       # point, to avoid naming conflicts.
       dsuffix <- "-aarch64"
    } else {
       RverA <- Rver
       dsuffix <- ""
    }

    cat(paste("OutputBaseFilename=", RW, "-", suffix, sep = ""),
        paste("AppName=R for Windows ", RverA, sep = ""),
        paste("AppVerName=R for Windows ", RverA, sep = ""),
        paste("AppVersion=", Rver, sep = ""),
        paste("VersionInfoVersion=", Rver0, sep = ""),
        paste("DefaultDirName={code:UserPF}\\R", dsuffix, "\\", RW, sep = ""),
        paste("InfoBeforeFile=", srcdir, "\\doc\\COPYING", sep = ""),
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
    lines <- gsub("@RVERA@", RverA, lines)
    lines <- gsub("@Producer@", Producer, lines)
    writeLines(lines, con)

    lines <- readLines(types)
    writeLines(lines, con)

    lines <- readLines("code.iss")
    lines <- gsub("@MDISDI@", MDISDI, lines)
    lines <- gsub("@HelpStyle@", HelpStyle, lines)
    writeLines(lines, con)

    writeLines(c("", "", "[Files]"), con)

    setwd(srcdir)
    files <- sub("^./", "",
                 list.files(".", full.names = TRUE, recursive = TRUE))
    for (f in files) {
	dir <- sub("[^/]+$", "", f)
	dir <- paste("\\", gsub("/", "\\", dir, fixed = TRUE), sep = "")
	dir <- sub("\\\\$", "", dir)

	component <- if (havex64 && grepl("/x64/", f)) "x64"
	else if (grepl("(/po$|/po/|/msgs$|/msgs/|^library/translations)", f))
            "translations"
	else "main"

        if (component == "x64" && !havex64) next # should not happen anymore
        
        if (havex64) {
            # Skip the /bin front ends, they are installed below
            if (grepl("bin/R.exe$", f)) next
            if (grepl("bin/Rscript.exe$", f)) next
        }
        
        f <- gsub("/", "\\", f, fixed = TRUE)
        if (havex64 && grepl("Rfe\\.exe$", f)) {
            bindir <- gsub("/", "\\", dirname(dir), fixed = TRUE)
            cat('Source: "', srcdir, '\\', f, '"; ',
                'DestDir: "{app}', bindir, '"; ',
                'DestName: "R.exe"; ',
                'Flags: ignoreversion; ',
                'Components: ', component,
                '\n',
                file = con, sep = "")   
            cat('Source: "', srcdir, '\\', f, '"; ',
                'DestDir: "{app}', bindir, '"; ',
                'DestName: "Rscript.exe"; ',
                'Flags: ignoreversion; ',
                'Components: ', component,
                '\n',
                file = con, sep = "")            
        }

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
