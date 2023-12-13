### Tests of iconv, especially 'sub'

## Status:
str(l10n_info()) # platform specific (-> help page)
		 # also changes from Sys.setenv / Sys.setlocale
Sys.getlocale()
(iconv_version <- extSoftVersion()[["iconv"]])
known_iconv <- iconv_version != "unknown"  # musl's iconv is "unknown"

xU <- "a\xE7\xFAcar" # "açúcar" (Portuguese)
(x <- xU) # (..\xe7..)
Encoding(x) <- "latin1"
stopifnot(Encoding(x) == "latin1")
x
xx <- iconv(x, "latin1", "UTF-8")
xx
stopifnot(Encoding(xx) == "UTF-8") # iconv() uses mark = TRUE by default
## encoding does *not* matter, even though they differ internally:
stopifnot(identical(xx, x), xx == x)

chkEQpr <- function(x, TR) stopifnot(print(x) == TR)
chkEQpr(charToRaw(xx), as.raw(c(0x61, 0xc3, 0xa7, 0xc3, 0xba, 0x63, 0x61, 0x72)))

## iconv() with substitution:
iconv(c(x, xx), to = "ASCII", sub = NA) # default
## often both NA, but could still use substitution ("a**car" with musl's iconv)
stopifnot(length(tools::showNonASCII(c(x, xx))) == 2L) # robust via string comparison
## output for most iconvs (at least GNU libiconv, glibc, win_iconv, macOS >= 14):
## 1: a<e7><fa>car
## 2: a<c3><a7><c3><ba>car
## musl:
## 1: a**car
## 2: a****car

if (known_iconv) withAutoprint({
    chkEQpr(iconv(x, "latin1", "ASCII", "?"   ), "a??car")
    chkEQpr(iconv(x, "latin1", "ASCII", ""    ), "acar")
    chkEQpr(iconv(x, "latin1", "ASCII", "byte"), "a<e7><fa>car")
    chkEQpr(iconv(xx, "UTF-8", "ASCII", "Unicode"), "a<U+00E7><U+00FA>car")
    chkEQpr(iconv(xx, "UTF-8", "ASCII", "c99"    ), "a\\u00e7\\u00facar")
    chkEQpr(charToRaw(iconv(xx, "UTF-8", "ASCII", "c99")),
            sapply(c("a", "\\","u", "0","0","e","7",
                     "\\","u", "0","0","f","a",  "c","a","r"), charToRaw))
})

z <- "\U1f600"
chkEQpr(charToRaw(z), as.raw(c(0xf0, 0x9f, 0x98, 0x80)))
if (known_iconv) withAutoprint({
    chkEQpr(iconv(z, "UTF-8", "ASCII", "byte"), "<f0><9f><98><80>")
    chkEQpr(iconv(z, "UTF-8", "ASCII", "Unicode"), "<U+1F600>")
    chkEQpr(iconv(z, "UTF-8", "ASCII", "c99"    ), "\\U0001f600")
})

## write out to compare with GNU libiconv's iconv on e.g. macOS < 14
## The reading can only work in a UTF-8 locale
if(startsWith(iconv_version, 'GNU libiconv') &&
   l10n_info()[["UTF-8"]]) {
    writeLines(c(xx, z), "test.txt")
    zz <- system2("iconv", c("-f", "UTF-8", "-t", "c99", "test.txt"),
                  stdout = TRUE)
    unlink('test.txt')
    stopifnot(zz == iconv(c(xx, z), "UTF-8", "ASCII", "c99"))
    message('sub = "c99" agrees with GNU libiconv')
} else message('sub = "c99" agrees with GNU libiconv -- SKIPPED')

##------------- former  ./encodings.R  -----------------------------------

## from iconv.Rd , things not above already:

# Extracts from R help files
(x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher"))
iconv(x, "latin1", "ASCII//TRANSLIT")
iconv(x, "latin1", "ASCII", sub="byte")

## tests of re-encoding in .C
require("tools")
(x. <- "a\xE7\xFAcar")
Renctest <- tools:::C_Renctest
(x.en <- .C(Renctest, x.)[[1]])
x <- x.; Encoding(x) <- "latin1"; x
(xen <- .C(Renctest, x)[[1]])
(xx <- iconv(x, "latin1", "UTF-8"))
(xxen <- .C(Renctest, xx)[[1]])
## TODO: check these {all TRUE in UTF-8  but only 1st in "C" locale}
identical(x., x.en)
identical(x ,  xen)
identical(xx, xxen)
##
c(x.= Encoding(x.), x.en= Encoding(x.en),
  x = Encoding(x),  xen = Encoding(xen),
  xx= Encoding(xx), xxen= Encoding(xxen)) -> encs
encs #  (unk unk  latin1 unk   UTF-8 unk)   in UTF-8  *and*  C locale (but not latin1)
## TODO: s/all/stopifnot/  {here and below}
all(encs == local({ u <- "unknown"; l <- "latin1"
                    lu <- if(l10n_info()[["Latin-1"]]) l else u
                    c(u, lu, l, u, "UTF-8", u) }))

## tests of match length in  delimMatch(x, delim = c("{", "}"))
(x <- c("a{bc}d", "{a\xE7b}"))
dM1 <- if(!l10n_info()[["MBCS"]]) { # not for multibyte locale
            delimMatch(x) # fine w/ LC_ALL=C ; otherwise Error: "invalid multibyte string"
 } else try(delimMatch(x))
if(is.numeric(dM1)) all(print(dM1), structure(2:1, match.length = 4:5)) ## TODO stopifnot
(xx <- iconv(x, "latin1", "UTF-8"))
str(dMx <- delimMatch(xx))
## 4 12 in "C" (4 5 in UTF-8, latin1, ?) -- was 5 6 in latin1, 5 5 in UTF-8
(ok <- with(l10n_info(), `UTF-8` || `Latin-1`)) # when else?
mlength <- if(ok) 4:5 else c(4L, 12L)
all(identical(dMx, structure(2:1, match.length = mlength)))
