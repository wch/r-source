#  File share/R/massage-examples.R
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

## based on massage-Examples.pl which was
## Copyright (C) 2001--2007 R Development Core Team

## Usage: Rscript massage-Examples.R --args pkgname [dir | files]

## Given a list of files of the form .../.../<name>.R, produce one large
## file, i.e., write to stdout, concatenating the files together with
## 1) Putting a HEADER in front
## 2) Wrapping every file in order to be more order independent
## 3) appending a FOOTER ...

args <- commandArgs(TRUE)
pkg <- args[1]
if(length(args) > 1 && file_test("-d", args[2])) {
    dir <- args[2]
    files <- sort(Sys.glob(file.path(dir, "*.R")))
} else files <- args[-1]

lines <- readLines(file.path(R.home(), "share", "R", "examples-header.R"))
cat(sub("@PKG@", pkg, lines), sep = "\n")

if(pkg == "tcltk") {
    cat("require('tcltk') || q()\n\n")
} else if(pkg != "base") cat("library('", pkg, "')\n\n", sep = "")

cat("assign(\".oldSearch\", search(), pos = 'CheckExEnv')\n")
cat("assign(\".oldNS\", loadedNamespaces(), pos = 'CheckExEnv')\n")

for(file in files) {
    nm <- sub("\\.R$", "", basename(file))
    ## make a syntactic name out of the filename
    nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE)
    if (pkg == "graphics" && nm == "text") next
    if(!file.exists(file)) stop("file ", file, " cannot be opened")
    lines <- readLines(file)
    have_examples <- length(grep("_ Examples _|### \\*+ Examples", lines)) > 0L
    ## skip comment lines
    com <- grep("^#", lines)
    lines1 <- if(length(com)) lines[-com] else lines
    have_par <- length(grep("[^a-zA-Z0-9.]par\\(|^par\\(", lines1)) > 0L
    have_contrasts <- length(grep("options\\(contrasts", lines1)) > 0L

    if(have_examples)
        cat("cleanEx(); nameEx(\"", nm, "\")\n", sep = "")

    cat("### * ", nm, "\n\n", sep = "")
    cat("flush(stderr()); flush(stdout())\n\n")
    dont_test <- FALSE
    for (line in lines) {
        if(length(grep("^## No test:", line))) dont_test <- TRUE
        if(!dont_test) cat(line, "\n", sep = "")
        if(length(grep("^## End\\(No test\\)", line))) dont_test <- FALSE
    }

    if(have_par)
        cat("graphics::par(get(\"par.postscript\", pos = 'CheckExEnv'))\n")
    if(have_contrasts)
        cat("options(contrasts = c(unordered = \"contr.treatment\",",
            "ordered = \"contr.poly\"))\n", sep="")
}

cat(readLines(file.path(R.home(), "share", "R", "examples-footer.R")), sep="\n")
q("no")
