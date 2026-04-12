#  File src/library/utils/tests/Stangle-tst.R
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

## Testing Stangle

library(utils)

StangleR <- function(file, ...) {
    dots <- list(...)
    if(!file.exists(file))
        stop("File ", sQuote(file), " does not exist in ", getwd())
    out <- capture.output(Stangle(file, ...))
    if (!is.null(dots$split) && dots$split) {
        v <- grepv("^[0-9]+ : ", out)
        if (!length(v))
            stop("No scripts tangled from ", sQuote(file), " in ", getwd())
        patF <- paste0(sub("\\.[RSrs]nw$", "", file), "-.+\\.R")
        tglF <- regmatches(v, regexpr(patF, v))
        if (any(fne <- !file.exists(tglF)))
            stop("File(s) ",
                 paste(sQuote(tglF[fne]), collapse = ", "),
                 " do not exist in ", getwd())
        setNames(lapply(tglF, readLines), tglF)
    }
    else {
        tglF <- sub("\\.[RSrs]nw$", ".R", file)
        if (!file.exists(tglF))
            stop("File ", sQuote(tglF), " does not exist in ", getwd())
        readLines(tglF)
    }
}

### ------------------------------------ 1 ----------------------------------
t1 <- StangleR("swv-keepSrc-1.Rnw")
if (FALSE) # look at it
writeLines(t1)

## This may have to be updated when the *.Rnw changes:
## 1. correct number of chunks
## 2. two blank lines before every chunk (except the first one)
## 3. no blank lines at the end of the script (starting with 4.6.0)
ch.mark <- grep("^### code chunk number", t1)
stopifnot(exprs = {
    length(ch.mark) == 7
    t1[rep(ch.mark[-1L], each = 2) - c(3, 2)] == ""
    tail(t1, 2) != ""
})

### ------------------------------------ 2 ----------------------------------
## repeat with 'split = TRUE'
t2 <- StangleR("swv-keepSrc-1.Rnw", split = TRUE)

## This may have to be updated when the *.Rnw changes:
## 1. one chunk per file
## 2. no blank lines at the end of every script (starting with 4.6.0)
stopifnot(exprs = {
    lengths(lapply(t2, grep, pattern = "^### code chunk number")) == 1
    sapply(t2, function(x) tail(x, 2) != "")
})

### ------------------------------------ 3 ----------------------------------
## ignored chunks with global options
t3 <- list(StangleR("ignore-on-tangle-global.Rnw", ignore.on.tangle = TRUE),
           StangleR("ignore-on-tangle-global.Rnw", ignore = TRUE),
           StangleR("ignore-on-tangle-global.Rnw", tangle = FALSE))

## scripts contain nothing else than the first two header lines (that
## can't be removed with 'split = FALSE')
stopifnot(lengths(t3) == 2)

### ------------------------------------ 4 ----------------------------------
## ignored chunks with chunk options
t4 <- StangleR("ignore-on-tangle-chunk.Rnw")

## script contains nothing else than the first two header lines (that
## can't be removed with 'split = FALSE')
stopifnot(length(t4) == 2)

### ------------------------------------ 5 ----------------------------------
## chunk separator changed with global option
t5 <- list(StangleR("chunk-sep-global.Rnw"),
           StangleR("chunk-sep-global.Rnw", chunk.sep = "\n"),
           StangleR("chunk-sep-global.Rnw", chunk.sep = "false"))

## see the .Rnw file for the expected number of blank lines in the script
bl <- 2 * c(2, 1, 0) + 1
stopifnot(sapply(t5, function(x) sum(x == "")) == bl)

### ------------------------------------ 6 ----------------------------------
## 'chunk.sep = TRUE' equivalent to default value
t6def <- StangleR("chunk-sep-global.Rnw")
t6 <-    StangleR("chunk-sep-global.Rnw", chunk.sep = TRUE)
stopifnot(identical(t6def, t6))

### ------------------------------------ 7 ----------------------------------
## chunk separator changed with chunk options
t7 <- StangleR("chunk-sep-chunk.Rnw", annotate = FALSE)

## see the .Rnw file for the expressions that should be on consecutive
## lines (that is, without chunk separator)
expr <- c("x <- 42", "x + 2")
stopifnot(diff(match(expr, t7)) == 1)

### ------------------------------------ 8 ----------------------------------
## extensions of output files with split = TRUE set using global
## option to ".txt"
out <- capture.output(Stangle("swv-keepSrc-1.Rnw", split = TRUE, extension = "txt"))

## extension of all output files is ".txt"
files <- sapply(strsplit(grepv("^[0-9]+ :", out), " "), "[", 3)
stopifnot(grepl("\\.txt$", files))

### ------------------------------------ 9 ----------------------------------
## extensions of output files with split = TRUE set using global
## option to default value (the engine)
out <- capture.output(Stangle("swv-keepSrc-1.Rnw", split = TRUE, extension = TRUE))

## extension of all output files is ".R"
files <- sapply(strsplit(grepv("^[0-9]+ :", out), " "), "[", 3)
stopifnot(grepl("\\.R$", files))

### ----------------------------------- 10 ----------------------------------
## extensions of output files with split = TRUE set using chunk options
Stangle("extension-chunk.Rnw", split = TRUE)

## This may have to be updated when the *.Rnw changes:
## suffix of output files are "foo.sh" and "bar"
stopifnot(file.exists(paste0("extension-chunk-", c("foo.sh", "bar"))))

### ----------------------------------- 11 ----------------------------------
## Rtangle chunk options taking their their values from objects are
## replaced by their default values, with a warning
tools::assertWarning(t11 <- StangleR("objs-in-opts.Rnw", annotate = FALSE), verbose = TRUE)

## script contains only empty or uncommented lines (other than the
## header in the first line)
stopifnot(grepl("^($|[^#])", t11[-1L]))
