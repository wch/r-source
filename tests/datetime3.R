## Date-time egression tests for R >= 4.3.0
## originally added to reg-tests-1d.R

.pt <- proc.time()

options(warn = max(1, getOption("warn")))

if(!nzchar(Sys.getenv("_R_CHECK_DATETIME3_NO_TZ_"))) withAutoprint({
  ## For some inter-platform reproducibility, try to set timezone
  ## even though  Sys.setenv(..) does *NOT* always work
  myTZ <- "Australia/Melbourne"
  (TZenvOrig <- Sys.getenv("TZ"))
  Sys.setenv(TZ = myTZ)
  Sys.getenv("TZ")
  TZok <- Sys.getenv("TZ") == myTZ
  if(!TZok) {
      print(sessionInfo())
      warning("'TZ' environment variable could *not* be set on this platform")
      ## maybe even:  quit("no")
  }
})

## 0-length Date and POSIX[cl]t:  PR#71290
D <- structure(17337, class = "Date") # Sys.Date() of "now"
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "Date of length 0"))
D <- structure(1497973313.62798, class = c("POSIXct", "POSIXt")) # Sys.time()
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "POSIXct of length 0"))
D <- as.POSIXlt(D)
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "POSIXlt of length 0"))
## They printed as   '[1] "Date of length 0"'  etc in R < 3.5.0


## rep.POSIXt(*, by="n  DSTdays") - PR#17342
x <- seq(as.POSIXct("1982-04-15 05:00", tz="US/Central"),
         as.POSIXct("1994-10-15",       tz="US/Central"), by="360 DSTdays")
stopifnot(length(x) == 13, diff((as.numeric(x) - 39600)/86400) == 360)
## length(x) was 1802 and ended in many NA's in R <= 3.4.2

## print.POSIX[cl]t() - not correctly obeying "max.print" option
op <- options(max.print = 50, width = 85)
cc <- capture.output(print(dt <- .POSIXct(154e7 + (0:200)*60)))
c2 <- capture.output(print(dt, max = 6))
writeLines(tail(cc, 4))
writeLines(c2)
stopifnot(expr = {
    grepl("omitted 151 entries", tail(cc, 1))
                  !anyDuplicated(tail(cc, 2))
    grepl("omitted 195 entries", tail(c2, 1))
}); options(op)
## the omission had been reported twice because of a typo in R <= 3.5.1


## as.Date() from POSIXct and POSIXlt should retain names
(ch <- setNames(paste0("1994-10-", 11:15), letters[1:5]))
d1 <- as.Date(ch, tz = "UTC")
ct <- as.POSIXct(ch)
d2 <- as.Date(ct, tz = "UTC") # fast path
lt <- as.POSIXlt(ch, tz = "UTC")
(d3 <- as.Date(lt))
stopifnot(identical(names(ch), names(d1)),
          identical(names(ch), names(d2)),
          identical(names(ch), names(d3)))
## in R <= 4.1.1, names got lost whenever as.Date.POSIXlt() was called


## hist() of a single date or date-time
dt <- as.POSIXlt("2021-10-13", "UTC")
hist(dt,          "secs", plot = FALSE)
hist(as.Date(dt), "days", plot = FALSE)
## failed in R <= 4.1.2 with Error in seq_len(1L + max(which(breaks < maxx)))


## format.POSIX[cl]t() after print.POSIXct()
dt <- "2012-12-12 12:12:12"
x <- as.POSIXct(dt, tz = "GMT")
stopifnot(identical(format(x), dt))
op <- options(warn=1)# allow
(Sys.t <- Sys.timezone()) # may occasionally warn (and work)
options(op)
someCET <- paste("Europe", c("Berlin", "Brussels", "Copenhagen", "Madrid",
                             "Paris", "Rome", "Vienna", "Zurich"), sep="/")
if(Sys.t %in% someCET)
    stopifnot(identical(print(format(x, tz = "")), "2012-12-12 13:12:12"))
## had failed for almost a month in R-devel & R-patched


## as.Date(<nonfinite_POSIXlt>) :
D <- .Date(c(7:20)*1000)
D[15:18] <- c(Inf, -Inf, NA, NaN); D
stopifnot( identical(D, as.Date(as.POSIXlt(D))) )
## non-finite POSIXlt gave all  NA in R <= 4.2.1
##
## POSIX[cl]t: keeping names, also w/ factors; is.finite() ...
(D <- setNames(D, LETTERS[seq_along(D)]))
fD <- factor(D)
stopifnot(exprs = {
    identical(fD, as.factor(D))
    identical(names(D), names(fD))
    ## identical(D, as.Date(fD)) -- FIXME
    identical(D, as.Date(Dct <- as.POSIXct(D))) # also checks names(.) are kept
    identical(D, as.Date(Dlt <- as.POSIXlt(D)))
    identical(as.character(D), as.character(Dlt))
    identical(      format(D),       format(Dlt) -> frmD)
    identical(names(D), names(frmD))
    (DeD <- Dlt == Dct)[ok <- is.finite(D)]
    identical(is.na(DeD), is.na(D))
    identical(as.character(D), unname(frmD))
    identical(unname(ok), is.finite(as.numeric(D)))
    identical(ok, is.finite(Dct))
    identical(ok, is.finite(Dlt))     # now works for POSIXlt
    identical(is.nan(D), is.nan(Dlt))
    identical(is.infinite(D), is.infinite(Dlt))
    identical(D == -Inf, Dlt == -Inf)
})
## is.finite() now works for POSIXlt


## as.POSIX?t(<POSIX?t>, tz=*) now works, too:
stopifnot(inherits(Dct, "POSIXct"),
          inherits(Dlt, "POSIXlt"))
Sys.timezone() #  "Australia/Melbourne"   (set above)
mtz <- "Etc/GMT-5" # was UTC-5
head(Dct2  <- as.POSIXct(Dct, tz = mtz), 3)
head(Dlt2  <- as.POSIXlt(Dlt, tz = mtz), 3) ## these three POSIXlt "are different"
head(Dlct2 <- as.POSIXlt(Dct2),          3)
head(Dlct  <- as.POSIXlt(Dct) ,          3)
no_tz <- function(.) `attr<-`(., "tzone", NULL)
stopifnot(exprs = {
    identical(mtz, attr(Dct2, "tzone"))
    identical(mtz, attr(Dlt2, "tzone"))

    (Dct2 - Dct)[ok] == 0
    identical(no_tz(Dct2), no_tz(Dct))
    identical(no_tz(Dlt2), no_tz(Dlt))
    ## However (!!)
    (Dct2  - Dct)[ok] == 0
    ## Have 2 groups" which are "equal":  { Dlt "==" Dlct "==" Dlct2 } and  Dlt2  which differs by 5
    (Dlt2  - Dlt )[ok] == -5L # !!!
    (Dlct2 - Dlt )[ok] == 0L
    (Dlct  - Dlt )[ok] == 0L
    (Dlct  - Dlt2)[ok] == 5L
})
## both methods return(x)ed immediately, when class "matched"
op <- options(OutDec = ",") # the "infamous default" in some places should *not* have an effect:
xf <- as.POSIXlt(chf <- c("2007-07-27 16:11:03.000002",
                          "2011-10-01 12:34:56.3",
                          "2022-10-02 13:14:15.9876543210123456"))
dd <- setNames(, c(0:6, 11:15))
t(sapply(dd, as.character.POSIXt, x = xf)) # get at most 13 dits after ".", because
as.character(xf[3]$sec) # does get these but not more; hence:
chf[3] <- sub("3456$", "3", chf[3])
stopifnot(exprs = {
    identical(as.character(xf), chf)
    identical(as.character(xf, OutDec = ","), sub("[.]", ",", chf))
    identical(as.character(xf, digits = 5)[-3], sub("[.]00*2$","", chf[-3]))
})
## failed for ~ 1 day in R-devel
(CharleMagne.crowned <- as.POSIXlt(ISOdate(774,7,10)))
stopifnot(identical(as.character(CharleMagne.crowned),
                    "774-07-10 12:00:00"))
options(op) # reset


## as.POSIX[cl]t(<Date>, tz = *)
isUTC <- function(tz)
    switch(tz,
           "UTC" =, "GMT" = , "Etc/UTC" = , "Etc/GMT" = , "GMT0" = , "GMT+0" = , "GMT-0" = TRUE,
           FALSE)
identical3 <- function(a,b,c) identical(a,b) && identical(b,c)
datePOSIXchk <- function(d, tz) {
    stopifnot(inherits(d, "Date"), is.character(tz))
    UTC. <- isUTC(tz)
    ## cat(sprintf("\ntz = '%s'%s, Date = '%s':\n      ------------",
    ##             tz, if(UTC.)"(= UTC)" else "", paste(format(d), collapse=", ")))
    PCdate <- as.POSIXct(d, tz = tz); PLpc <- as.POSIXlt(PCdate); PLpcz <- as.POSIXlt(PCdate, tz = tz)
    PLdate <- as.POSIXlt(d, tz = tz); PCpl <- as.POSIXct(PLdate); PCplz <- as.POSIXct(PLdate, tz = tz)
    m <- rbind(PLdate = format(PLdate, usetz=TRUE)
      , PCdate = format(PCdate, usetz=TRUE)
      , PLpc   = format(PLpc,   usetz=TRUE)
      , PLpcz  = format(PLpcz,  usetz=TRUE)
      , PCpl   = format(PCpl,   usetz=TRUE)
      , PCplz  = format(PCplz,  usetz=TRUE)
    )
    colnames(m) <- rep("", ncol(m))
    print(m[c(1:2,5L), ]) # print() those three which are "typically" different
    ##
    diffD <- PLdate - PCdate
    cat("PLdate - PCdate:", capture.output(diffD[1]), "\n")
    ##
    if(length(delta <- unique(diffD)) != 1L) {
        cat(sprintf("# {unique diffD values} (typically 1), here %d:\n", length(delta)))
        attributes(delta) <- attributes(diffD) ## class, units
        print(delta)
    }
    stopifnot(exprs = {
        PLpc == PCdate
        PLpc == PLpcz
        ## Not  identical3(PCdate, PLpc, PLpcz), but identically formatted:
        identical3(m["PCdate",], m["PLpc",], m["PLpcz",])
        ##
        identical(PCpl, PCplz) # and typically *not* identical to  PLdate, but still equal:
        PCpl == PLdate
        ##
        PLdate - PLpc  == diffD
        PLdate - PLpcz == diffD
        PCpl  - PCdate == diffD
        PCplz - PCdate == diffD
    })
    if(UTC.) ## UTC-equivalent timezone
        stopifnot(exprs = {
            delta == 0
            ## and the two groups (of 3 each) are equal, too
            PLdate == PCdate
        })
}
##
d1 <- as.Date(c("2000-02-29", "2001-04-01"))
otz <- OlsonNames()

## BST is a deliberate unknown: most platforms would use UTC, some warning
##   (see ?Sys.timezone).
## EST5EDT is a legacy non-continent/ocean name
## Europe/Dublin is unusual as it has 'winter time' not DST with
##   'Irish Standard Time' being used in summer (at least in 2022)
## Europe/Kyiv became a primary name in Aug 2022.
## 'Time difference quoted here is from UTC aka GMT, in hours.
for(tz in c("GMT", "EST5EDT", "BST", "Pacific/Auckland",
            "Africa/Cairo", "Asia/Jerusalem ", "America/Jamaica",
            "Africa/Conakry", "Asia/Calcutta", "Asia/Seoul", "Asia/Shanghai",
            "Asia/Tokyo", "Canada/Newfoundland", "Europe/Dublin",
            "Europe/Vienna", "Europe/Kyiv", "Europe/Moscow")) {
    cat("\n")
    if(!(tz %in% otz)) {
        cat(tz, "is not in this platform's OlsonNames()\n")
        next
    }
    cat("Using", sQuote(tz), "\n")
    datePOSIXchk(d1, tz)
}
## several of the identities datePOSIXchk() failed in R <= 4.2.x
## unnecessarily passing 'origin'
ct <- as.POSIXct(.Date(19000), origin="1970-01-01")
stopifnot(identical(as.Date(ct), .Date(19000)))
## as.POSIXct.Date() passed on 'origin' raising an error for ~25 hours

## as.POSIXct.default() dealing with an *extraneous*  origin = ".."
(D <- .Date(19000))
## NB: The following depends on the timezone, see Sys.timezone() above
cE <- as.POSIXct(D, tz="EST")
lE <- as.POSIXlt(D, tz="EST")
ct   <- as.POSIXct(cE)
ct50 <- as.POSIXct(cE, origin="1950-1-1", tz = "NZ") ## <-- failed for 1.5 days
lt50 <- as.POSIXlt(lE, origin="1950-1-1", tz = "NZ") ##   (ditto)
stopifnot(exprs = {
    identical(ct, structure(1641600000, class = c("POSIXct", "POSIXt"), tzone = "EST")) # no tzone in R <= 4.2.x
    identical(ct, cE)
    identical(ct50, `attr<-`(ct, "tzone", "NZ")) # ct50 had no "tzone"        in R <= 4.2.x
    identical(lt50, `attr<-`(lE, "tzone", "NZ")) # lt50 had     tzone = "UTC" in R <= 4.2.x
    identical(as.character(lE), "2022-01-08")
})
## worked (but partly  differently!) in R <= 4.2.x


## as.POSIXct(<numeric>) & as.POSIXlt(*) :
for(nr in list(1234, -1:1, -1000, NA, c(NaN, 1, -Inf, Inf),
               -2^(20:33), 2^(20:33)))
    for(tz in c("", "GMT", "NZ", "Pacific/Fiji")) {
        cat("testing in", sQuote(tz),"\n")
        n <- as.numeric(nr)
        stopifnot(identical(n, as.numeric(print(as.POSIXct(nr, tz=tz)))),
                  identical(n, as.numeric(      as.POSIXlt(nr, tz=tz))))
    }
## did not work without specifying 'origin'  in  R <= 4.2.x


## small options("scipen") producing exponential format
cdt <- "2007-07-27 16:11:00.000000000000006"; (dt <- as.POSIXlt(cdt))
op <- options(scipen = 0, OutDec = ",")
cbind(ccdt <- c(as.character(dt), as.character(dt, digits=15)))
stopifnot(grepl(":00\\.0{13}", ccdt), getOption("OutDec") == ",")
cdt == ccdt[2] # TRUE on all platforms?
options(op)# reset
## accidentally used exponential format (and changed OutDec opt) for a while


## as.Date(.) now also takes default origin 1970-1-1:
stopifnot(exprs = {
  identical(D1 <- as.Date(20000), as.Date(20000, origin = "1970-01-01"))
  inherits(D1, "Date")
  identical(as.character(D1), "2024-10-04")
  inherits(D2 <- as.Date(20000, origin="1960-1-1"), "Date")
  D2 + 3653 == D1
  identical(c(D2,D1), seq(D2, length.out=2, by = "10 years"))
})
## 'origin' was not optional in R <= 4.2.x


## length(<ragged POSIXlt>)
## Ex. of "partially filled" with NA's, *not* evenly recycling, out-of-range, fractional sec
## However, isdst is not known and depends on the time zone.
## Using -1L says so: 1L failed in time zones without DST on glibc.
## NB: this has only 9 elements
dlt <- .POSIXlt(list(sec = c(-999, 10000 + c(1:10,-Inf, NA)) + pi,
                     min = 45L, hour = c(21L, 3L, NA, 4L),
                     mday = 6L, mon  = c(0:11, NA, 1:2),
                     year = 116L, wday = 2L, yday = 340L, isdst = -1L))
f1 <- format(dlt[1], "%Y-%m-%d %H:%M:%OS3") # PR#18448
stopifnot(f1 == "2016-01-06 21:28:24.141")  # gave "... 21:28:-995.858" in R <= 4.2.2
dct   <- as.POSIXct(dlt)
dltN  <- as.POSIXlt(dct) # "normalized POSIXlt" (with *lost* accuracy), but *added* tz-info:
data.frame(unclass(dltN)); str(attributes(dltN)[-1], no.list=TRUE)
dltv2 <- local({ x <- dltN
    length(x$min ) <- 4; length(x$hour) <- 4; length(x$mday ) <- 2
    length(x$mon ) <- 9; length(x$year) <- 5; length(x$isdst) <- 1
    length(x$yday) <- 1; length(x$wday) <- 1; length(x$ zone) <- 1; length(x$gmtoff) <- 1
    x })
dltm3 <- dlt; dltm3$mon <- c(11L, NA, 3L)
dltI  <- dltm3; dltI$sec <- c(-Inf, 0:2, NaN, NA, Inf)
c((n <- length(dlt)), (n2 <- length(dltv2)), (n3 <- length(dltm3)))
stopifnot(n == 15L, n2 == 15L, n3 == 13L)
## always returned  length(*$sec)  in R <= 4.2.x
## smallest possible lt which shows format.POSIXlt() bug :
lt. <- local({ l <- 1L
    .POSIXlt(list(sec = -Inf, min = l, hour = l, mday = l,
                  mon = l, year = l, wday = l, yday = l, isdst = l))
})
stopifnot(format(lt.) == "-Inf")# was NA, which is wrong after allowing non-finite
ltI <- .POSIXlt(list(sec = c(-Inf, 0, NaN, NA, Inf), min = 45L, hour = 21L, mday = 6L,
                     mon = 0:11, year = 116L, wday = 2L, yday = 340L, isdst = 1L))
ctI  <- as.POSIXct(ltI) # typically differs from
ctIu <- as.POSIXct(ltI, "UTC")
stopifnot(identical3(format(ctIu), format(as.POSIXlt(ctIu)), format(balancePOSIXlt(ltI))))
##
##  More dealing with such ragged out-of-range POSIXlt:
##  (with console output, if only to compare platform dependencies)
dctm3  <- as.POSIXct(dltm3)
dltNm3 <- as.POSIXlt(dctm3)
data.frame(unclass(dltN))
##' For a POSIXlt, check if it is "ragged"
is.raggedPOSIXlt <- function(x) { stopifnot(inherits(x,"POSIXlt"))
    n <- lengths(unclass(x)); any(n[[1]] != n) }
##' create "normalized" POSIXlt *not* losing fractional seconds accuracy
.POSIXltNormalize <- function(x, tz="UTC") { ## for some tz="UTC" is needed, for others tz=""
    stopifnot(is.numeric(s <- x$sec))
    tzA <- attr(x, "tzone")
    n <- length(ct <- as.POSIXct(x, tz=tz))
    x <- as.POSIXlt(ct) # and restore "tzone" attrib. & the precise seconds (recycling carefully!)
    ifin <- is.finite(s <- rep_len(s, n)) & is.finite(x$sec)
    x$sec[ifin] <- s[ifin] %% 60
    if(!is.null(tzA)) attr(x, "tzone") <- tzA
    x
}
dlt2N   <- .POSIXltNormalize(dlt, tz="") # normalized POSIXlt - with accuracy kept
dlt2Nu  <- .POSIXltNormalize(dlt)        # (ditto; in "UTC")
dlt2Nm3 <- .POSIXltNormalize(dltm3, tz="")
all.equal(dlt2N  $sec, dltN  $sec, tolerance = 0) # .. small (2e-9) difference
all.equal(dlt2Nm3$sec, dltNm3$sec, tolerance = 0) # .. (ditto, slightly different)
## new balancePOSIXlt() :
## Should be identity when a POSIXlt is already balanced:
(lt1 <- as.POSIXlt(ch <- "2001-02-03 04:05")) # "... AEDT" (Southern Summer)
stopifnot(identical(lt1, balancePOSIXlt(lt1)))
## failed initially
## The hard cases from above:
dltB   <- balancePOSIXlt(dlt)
dltv2B <- balancePOSIXlt(dltv2)
dltBm3 <- balancePOSIXlt(dltm3)
dltL <- unclass(dltB)
data.frame(dltL)
stopifnot(exprs = {
    is.list(dltL)
    lengths(dltL) == n
    identical(dltL, balancePOSIXlt(dlt, class=FALSE))
    is.raggedPOSIXlt(dlt)
    is.raggedPOSIXlt(dltv2)
    is.raggedPOSIXlt(dltm3)
   !is.raggedPOSIXlt(dltN)
   !is.raggedPOSIXlt(dlt2N)
   !is.raggedPOSIXlt(dlt2Nm3)
   !is.raggedPOSIXlt(dltB)
   !is.raggedPOSIXlt(dltv2B)
   !is.raggedPOSIXlt(dltBm3)
    ## equal with default tolerance:
    all.equal(dlt2N,   dltN)
    all.equal(dlt2Nm3, dltNm3)
    identical(as.POSIXct(dlt2N),   as.POSIXct(dltN))
    identical(as.POSIXct(dlt2Nm3), as.POSIXct(dltNm3))
})
## First show (in a way it also works for older R), then check :
oldR <- getRversion() < "4.2.2"
(dd <- data.frame(dlt, dltN, asCT = dct, na = is.na(dlt),
                  fin = if(oldR) rep_len(NA, n3) else is.finite(dlt)))
## Look at *all* current "POSIXlt" objects:
str(lts <- local({ oG <- as.list(.GlobalEnv)
    oG[vapply(oG, inherits, TRUE, "POSIXlt")] }))
## Check if/when  balancePOSIXlt(.) is the identity for  "POSIXlt" objects
blts <- lapply(lts, balancePOSIXlt)
all.equal(blts, lts) # on Lnx Fedora 36 "ltI", "dlt", "dltI" and "dltm3" have been shifted by one hour - ok (??)
## all the others are *deemed* equal by the "tolerant"  all.equal.POSIXt()
(nmsLT <- setdiff(names(lts), c("ltI", "dlt", "dltI", "dltm3")))
at.b <- lapply(blts, attributes)
(cNms <- setdiff(names(at.b[[1]]), "balanced"))
stopifnot(exprs = {
    all.equal(lts[nmsLT], blts[nmsLT], tolerance = 0)
    ## now blts are all balanced of course; lts only partly
    vapply(at.b, `[[`, TRUE, "balanced")
    identical(lapply(lapply( lts, attributes), `[`, cNms),
              lapply(at.b,                     `[`, cNms))
})
chlts <- lapply( lts, as.character)
cblts <- lapply(blts, as.character)
stopifnot(identical(chlts, cblts))
## only now that as.char..() uses balanceP..()


## Indexing --
which(ina <- is.na(dlt))
stopifnot(exprs = {
    all.equal(format(ltI[-1]), format(ltI)[-1]) # ltI[-1] gave an error
    all.equal(format(ltI[-2]), format(ltI)[-2])
    identical(which(ina), c(3L, 7L, 11L, 13L, 15L))
    !anyNA(dlt[!ina])
    format(dlt[!ina]) == format(dlt)[!ina] # dlt[!ina] was mostly NA
    !anyNA(dltm3[!is.na(dltm3)])
    identical(as.POSIXct(dlt[-3], "UTC"),
              as.POSIXct(dlt, "UTC")[-3])
})
## subassigning  [<- :
tmp <- dlt
tmp[1] <- tmp[15]
stopifnot(exprs = {
    identical(print(format(tmp)), # had badly failed
              format(dlt)[c(15, 2:15)])
    identical(tmp[1], dlt[15])
    identical(tmp[-1], dlt[-1])
})
## badly failed in ragged case, before balancePOSIXlt() was used


##
## After fixes in as.Date.POSIXlt (and also as.Date.POSIXt)  methods
dD  <- as.Date(dlt)
dDc <- as.Date(dct)
dd2 <- data.frame(lt = dltN, ct = dct, dD, dDc, d.D = (dD - dDc), d.POSIX = (dlt - dct))
print(width = 101, dd2) ## look at [9,] -- do the date parts correspond ?
dDm3  <- as.Date(dltm3)
dDcm3 <- as.Date(dctm3)
nD  <- unclass(dD)
nDc <- unclass(dDc)
cbind(nD, nDc, diff = nD-nDc)
ifi  <- is.finite(dct)
ifi3 <- is.finite(dctm3)
stopifnot(exprs = {
    all.equal(dD, dDc, tolerance = 1e-4)
    (dDm3 - dDcm3)[ifi3] %in% 0:1
      (dD - dDc  )[ifi]  %in% 0:1
      (nD - nDc  )[ifi]  %in% 0:1
    is.na((dD   - dDc  )[!ifi])
    is.na((dDm3 - dDcm3)[!ifi3])
})
## as.Date.POSIXlt() failed badly for such ragged cases in  R <= 4.2.x


## ragged, including names
(nlt <- c(P = as.POSIXlt("2000-1-2 3:45")))
r3n <- rep(nlt, 3)
lt2 <- nlt; lt2$min <- 45:49; lt2  # (names print wrongly)
names(lt2) # "P" is maybe ok
(b2 <- balancePOSIXlt(lt2))# now correct
t3 <- lt2; t3$year <- c(P = 100L, Q = 101L)
t3 # "works", not printing recycled names (FIXME?)
as.POSIXct(t3) # ditto FIXME
(b3 <- balancePOSIXlt(t3))
t4 <- lt2; names(t4) <- n4 <- c("P", "Q", "", "S", "T"); t4
t5 <- lt2; names(t5) <- n4[-5] ; t5 # works; last name is <NA>
bare <- function(x) ## drop all attributes but names:
    `attributes<-`(x, if(!is.null(n <- names(unclass(x)))) list(names=n))
stopifnot(exprs = {
    identical("P", names(nlt))
    identical(nlt, balancePOSIXlt(nlt))
    length(r3n) == 3
    identical(nlt, r3n[3])
    identical(names(r3n), rep("P", 3))
    length(lt2) == 5
    identical(names(b2), rep("P", 5))
    identical(bare(b2), bare(balancePOSIXlt(lt2, fill.only = TRUE)))# (here)
    identical(names(b3), rep_len(c("P","Q"), length(b3)))
    identical(n4, names(t4))
    identical(n4, names(balancePOSIXlt(t4, fill.only = TRUE)))
    identical(nn <- c(n4[-5], NA), names(t5))
    identical(nn, names(b5 <- balancePOSIXlt(t5)))
    identical(bare(b5), bare(balancePOSIXlt(t5, fill.only = TRUE))) # (here)
})
## names(.) were not recycled correctly in original balanceP..()


## moves from strptime.Rd
stopifnot(identical("Inf", format(.POSIXct(Inf)))) # (was NA in R <= 4.1.x)
notF <- c(-Inf,Inf,NaN,NA)
(fF <- format(tnF <- .POSIXct(notF))) # was all NA, now the last is still NA (not "NA")
stopifnot(identical(as.character(notF[-4]), fF[-4])) # [4] may change!
##
## balancePOSIXlt() *not* fixing  {zone, isdst, gmtoff}
cht <- "2022-10-20 15:09"
sec <- (0:18)*47^3
(lt. <- local({ t <- as.POSIXlt(cht          ); t$sec <- sec; t }))
(lte <- local({ t <- as.POSIXlt(cht, tz=""   ); t$sec <- sec; t }))
(ltU <- local({ t <- as.POSIXlt(cht, tz="UTC"); t$sec <- sec; t }))
(ct.. <- as.POSIXct(lt.          )) # good, localtime, switch CEST -> CET
(ct.e <- as.POSIXct(lt., tz=""   ))
(ct.U <- as.POSIXct(lt., tz="UCT")) #  *very* bad: all NA but [14] ( = "1969-12-31 .." )
(cte. <- as.POSIXct(lte          ))
(ctee <- as.POSIXct(lte, tz=""   ))
(cteU <- as.POSIXct(lte, tz="UCT")) #  *very* bad: all NA but [14] ( = "1969-12-31 .." )
(ctU. <- as.POSIXct(ltU          )) # good, all UTC
(ctUe <- as.POSIXct(ltU, tz=""   )) #"good", localtime, if not-int, shifted by 1 hour
(ctUU <- as.POSIXct(ltU, tz="UCT")) # "good", all UTC, but shifted by 1-2 hours (int <-> non-int)
table(dUe <- ctUe - ct..) # all 0 for int-tzone,  all 1 otherwise !!
table(dUU <- ctUU - ct..) # all 11 x '1' and 8 x '2' for int-tzone,  all  '2'  otherwise !
stopifnot(exprs = {
    all.equal(ct.e, ct.., check.tzone=FALSE)
    identical(cte., ct..)
    all.equal(ctee, ct.., check.tzone=FALSE)
})
(b1 <- balancePOSIXlt(lt., fill.only=TRUE))
(b2 <- balancePOSIXlt(lt.))
stopifnot(b1 == b2)



## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
