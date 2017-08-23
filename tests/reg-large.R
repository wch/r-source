#### Regression Tests that need "much" memory
#### (and are slow even with enough GBytes of mem)

### FIXME: Run "occasionally" / "in parallel" for 'make check-all' ?

Sys.memGB <- function (kind = "MemTotal")
{
    mm <- drop(read.dcf("/proc/meminfo", fields = kind))
    if (any(is.na(mm))) {
        warning("Non-existing 'kind': ", names(mm)[is.na(mm)][1])
        0
    } else if (!all(grepl(" kB$", mm)))  {
        warning("Memory info ", dQuote(kind),
                " is not returned in 'kB' aka kiloBytes")
        0
    } else
        as.numeric(sub(" kB$", "", mm))/(1000 * 1024)
}

if(file.exists("/proc/meminfo")) { # e.g. on Linux
    availableGB <- Sys.memGB("MemAvailable")
} else {
    availableGB <- 0 # unless we add something better here
}

### Testing  readLines()  *large* file with embedded nul aka `\0'
##
## takes close to one minute and ~ 10 GB RAM
if(availableGB > 11) withAutoprint({
    ## File construction originally by Bill Dunlap, Cc: R-help,
    ##    Subject: Re: [R] readLines without skipNul=TRUE causes crash
    ##    Date: Mon, 17 Jul 2017 08:36:55 -0700
    tf <- tempfile(); file <- file(tf, "wb")
    txtLine <- c(rep(as.raw(32:127), 2^5), charToRaw("\n")) # <- want many lines
    system.time({
        for(i in 1:(2^15-1)) writeBin(rep_len(txtLine,    2^16), file)
        for(i in 1:(2^15-1)) writeBin(rep_len(as.raw(0L), 2^16), file)
    })
    close(file)
    log2(file.size(tf)) ## 31.99996
    ## now, this gave a segmentation fault, PR#17311 :
    system.time( x <- readLines(tf) ) # depending on disk,.. takes 15-50 seconds
    ##                ---------
    str(ncx <- nchar(x, "bytes"))
    ## int [1:688108] 3072 3072 3072 3072 3072 3072 3072 3072 ...
    tail(ncx) # ... 3072 3072 3072 1003
    table(ncx) # mostly 3072, then some 4075 and the last one
    head(iL <- which(ncx == 4075))
    stopifnot(diff(iL) == 21)
})


### Testing PR#17992  c() / unlist() name creation for large vectors
## Part 1
if(availableGB > 21) system.time({
    res <- c(a=raw(2), raw(2^31-1))
}) ## 36 sec elapsed (ada-16, ~ 120 GB available) after fix
## In R <= 3.4.1, took  51 sec elapsed, and gave Error .. :
##  attempt to set index 18446744071562067968/2147483649 in SET_STRING_ELT
##
if(FALSE) { # object.size() itself is taking a lot of time!
    os <- object.size(res)
} else {
    os <- structure(19327353184, class = "object_size")
    print(os, units = "GB") # 18
}
rm(res); gc() # for the next step

### Testing PR#17992  c() / unlist() name creation for large vectors
## Part 2 (https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17292#c4):
if(availableGB > 37) system.time({
    res <- c(a = list(rep(c(b=raw(1)), 2^31-2), raw(2)), recursive=TRUE)
})
## 437 sec elapsed (ada-16, ~ 120 GB available) after fix
## In R <= 3.4.1, took  475 sec  elapsed, and gave Error .. :
##    could not allocate memory (2048 Mb) in C function 'R_AllocStringBuffer'
## ((and that error msg is incorrect because of int overflow))
str(res) # is fast!
## Named raw [1:2147483648] 00 00 00 00 ...
## - attr(*, "names")= chr [1:2147483648] "a.b" "a.b" "a.b" "a.b" ...
gc() # back to ~ 18 GB


## Large string's encodeString() -- PR#15885
if(availableGB > 4) system.time(withAutoprint({
    txt <- strrep("test me:", 53687091); object.size(txt) # 429'496'824 bytes
    nc <- nchar(txt); ## NB this is larger than maximal integer:
    nc*5L+8L # NA + Warning   'NAs produced by integer overflow'
    en <- encodeString(txt)
    ## encodeString() seg.faulted in R <= 3.4.1
    stopifnot(identical(txt,en)) # encoding did not change simple ASCII
}))
## 52 sec elapsed [nb-mm4, 8 GB]


## pretty(x, n) for n = <large> or  large diff(range(x) gave overflow in C code
if(availableGB > 6) system.time(withAutoprint({
    r <- pretty(c(-1,1)*1e300, n = 449423288, min.n = 1)
    head(r) ; length(r) # was only 21 in  R < 3.5.0
    stopifnot(all.equal(length(r), 400000001, tol = 0.1))
})) ## 4.8 sec.

n <- 2.2e9 ; n / .Machine$integer.max # 1.024 ==> need  long vectors!
system.time(ii <- seq_len(n))           #   user  system elapsed
                                        #  2.592   6.979   9.593
system.time(i2 <- ii[-n])               # 16.057  25.361  41.548 (slow!)
## In R <= 3.4.1 :
## Program received signal SIGSEGV, Segmentation fault.
## 0x00000000005a0daf in realSubscript (call=0x3f01408, stretch=<optimized out>,
##     nx=2200000000, ns=1, s=0x426db18) at ../../../R/src/main/subscript.c:691
## 691			    LOGICAL(indx)[ix] = 0;

