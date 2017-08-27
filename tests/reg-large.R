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
if(availableGB > 11) local(withAutoprint({
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
}))
## + 2 warnings


### Testing PR#17992  c() / unlist() name creation for large vectors
## Part 1
if(availableGB > 21) system.time({
    res <- c(a=raw(2), raw(2^31-1))
}) ## 36--44 sec elapsed (ada-16, ~ 120 GB available) after fix
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
rm(res); gc()

## Large string's encodeString() -- PR#15885
if(availableGB > 4) system.time(local(withAutoprint({
    txt <- strrep("test me:", 53687091); object.size(txt) # 429'496'824 bytes
    nc <- nchar(txt); ## NB this is larger than maximal integer:
    nc*5L+8L # NA + Warning   'NAs produced by integer overflow'
    en <- encodeString(txt)
    ## encodeString() seg.faulted in R <= 3.4.1
    stopifnot(identical(txt,en)) # encoding did not change simple ASCII
})))
## 52 sec elapsed [nb-mm4, 8 GB]


## pretty(x, n) for n = <large> or  large diff(range(x) gave overflow in C code
if(availableGB > 6) system.time(withAutoprint({
    r <- pretty(c(-1,1)*1e300, n = 449423288, min.n = 1)
    head(r) ; length(r) # was only 21 in  R < 3.5.0
    stopifnot(all.equal(length(r), 400000001, tol = 0.1))
})) ## 4.8 sec.
rm(r); gc()

if(availableGB > 17) withAutoprint({
    n <- 2.2e9; n/.Machine$integer.max  # 1.024 ==> need  long vectors!
    system.time(ii <- seq_len(n))       #   user  system elapsed
                                        #  2.592   6.979   9.593
    system.time(i2 <- ii[-n])           # 16.057  25.361  41.548 (slow!)
    ## NB: keep n, i, i2 for "below"
})
## In R <= 3.4.1 :
## Program received signal SIGSEGV, Segmentation fault.
## 0x00000000005a0daf in realSubscript (call=0x3f01408, stretch=<optimized out>,
##     nx=2200000000, ns=1, s=0x426db18) at ../../../R/src/main/subscript.c:691
## 691			    LOGICAL(indx)[ix] = 0;

if(availableGB > 60) withAutoprint({
    system.time( x <- ii/n )            #  7.29 user; 14.36 elapsed
    system.time( y <- sin(pi*x) )       # 50.5  user; 57.4  elapsed
                                        # and using  VIRT = 33.1 G   RES = 32.9 G
    ## default n (= "nout") = 50:
    system.time(ap1 <- approx(x,y, ties = "ordered"))# 15.6 user; 25.8 elapsed
    stopifnot(is.list(ap1), names(ap1) == c("x","y"), length(ap1$x) == 50,
              all.equal(ap1$y, sin(pi*ap1$x), tol= 1e-15))
    rm(ap1); gc() ## keep x,y,n,i2
})

if(availableGB > 211) withAutoprint({ ## continuing from above
    ## both large (x,y) *and* large output (x,y):
    system.time(xo <- x+1/(2*n))     # 9.0 elapsed
    system.time(ap  <- approx(x,y, ties = "ordered", xout = xo))
                                        # elapsed 561.4; using ~ 67 GB
    gc() # showing max.used ~ 109106 Mb
    stopifnot(is.list(ap), names(ap) == c("x","y"), length(ap$x) == n,
	      is.na(ap$y[n])) # because ap$x[n] > 1, i.e., outside of [0,1]
    stopifnot(all.equal(ap$y[i2], sin(pi*xo[i2]), tol= 1e-15))
    rm(ap); gc() # showing max.used ~ 210356 Mb
    ## only large x,y :
    system.time(ss <- spline(x,y, ties = "ordered", n = 1e4))
                                        # elapsed 126 s; using ~ 180 GB
    system.time(
    stopifnot(is.list(ss), names(ss) == c("x","y"), length(ss$x) == 1e4,
              all.equal(ss$y, sin(pi*ss$x), tol= 1e-15))
    )
})

