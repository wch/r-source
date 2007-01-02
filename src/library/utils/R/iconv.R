
## If you were wondering what these language codes stand for, see
## ftp://ftp.ilog.fr/pub/Users/haible/utf8/ISO_639
localeToCharset <- function(locale = Sys.getlocale("LC_CTYPE"))
{
    guess <- function(en)
    {
        if(en %in% c("aa", "af", "an", "br", "ca", "da", "de", "en",
                         "es", "et", "eu", "fi", "fo", "fr", "ga", "gl",
                         "gv", "id", "is", "it", "kl", "kw", "ml", "ms",
                         "nb", "nn", "no", "oc", "om", "pt", "so", "sq",
                         "st", "sv", "tl", "uz", "wa", "xh", "zu"))
                return("ISO8859-1")
        if(en %in% c("bs", "cs", "hr", "hu", "pl", "ro", "sk", "sl"))
            return("ISO8859-2")
        if(en %in% "mt") return("ISO8859-3")
            if(en %in% c("mk", "ru")) return("ISO8859-5")
        if(en %in% "ar") return("ISO8859-6")
        if(en %in% "el") return("ISO8859-7")
        if(en %in% c("he", "iw", "yi")) return("ISO8859-8")
        if(en %in% "tr") return("ISO8859-9")
        if(en %in% "lg") return("ISO8859-10")
        if(en %in% c("lt", "lv", "mi")) return("ISO8859-13")
        if(en %in% "cy") return("ISO8859-14")
        if(en %in% "uk") return("KOI8-U")
        if(en %in% "ja") return("EUC-JP")
        if(en %in% "ko") return("EUC-KR")
        if(en %in% "th") return("TIS-620")
        if(en %in% "tg") return("KOI8-T")
        if(en %in% "ka") return("GEORGIAN-PS")
        if(en %in% "kk") return("PT154")
        ## not safe to guess for zh
        return(NA_character_)
    }
    if(locale %in% c("C", "POSIX")) return("ASCII")
    if(.Platform$OS.type == "windows") {
        x <- strsplit(locale, ".", fixed=TRUE)[[1]]
        if(length(x) != 2) return(NA_character_)
        ## PUTTY suggests mapping Windows code pages as
        ## 1250 -> ISO 8859-2
        ## 1251 -> KOI8-U
        ## 1252 -> ISO 8859-1
        ## 1253 -> ISO 8859-7
        ## 1254 -> ISO 8859-9
        ## 1255 -> ISO 8859-8
        ## 1256 -> ISO 8859-6
        ## 1257 -> ISO 8859-13
        switch(x[2],
               "1250" = return("ISO8859-2"),
              # this is quite wrong "1251" = return("KOI8-U"),
               "1252" = return("ISO8859-1"),
               "1253" = return("ISO8859-7"),
               "1254" = return("ISO8859-9"),
               "1255" = return("ISO8859-8"),
               "1256" = return("ISO8859-6"),
               "1257" = return("ISO8859-13")
               )
        return(paste("CP", x[2], sep=""))
    } else {
        ## Assume locales are like  en_US[.utf8[@euro]]
        x <- strsplit(locale, ".", fixed=TRUE)[[1]]
        enc <- if(length(x) == 2) gsub("@.*$o", "", x[2]) else ""
        if(enc == "UTF-8") enc <- "utf8" # for AIX
        if(nchar(enc) && enc != "utf8") {
            enc <- tolower(enc)
            known <-
                c("ISO8859-1", "ISO8859-2", "ISO8859-3", "ISO8859-6",
                  "ISO8859-7", "ISO8859-8", "ISO8859-9", "ISO8859-10",
                  "ISO8859-13", "ISO8859-14", "ISO8859-15",
                  "CP1251", "CP1255", "EUC-JP", "EUC-KR", "EUC-TW",
                  "GEORGIAN-PS", "KOI8-R",  "KOI8-U", "TCVN",
                  "BIG5" , "GB2312", "GB18030", "GBK",
                  "TIS-620", "SHIFT_JIS", "GB2312", "BIG5-HKSCS")
            names(known) <-
                c("iso88591", "iso88592", "iso88593", "iso88596",
                  "iso88597", "iso88598", "iso88599", "iso885910",
                  "iso885913", "iso885914", "iso885915",
                  "cp1251", "cp1255", "eucjp", "euckr", "euctw",
                  "georgianps", "koi8r", "koi8u", "tcvn",
                  "big5" , "gb2312", "gb18030", "gbk",
                  "tis-620", "sjis", "eucn", "big5-hkscs")
            if(enc %in% names(known)) return(as.vector(known[enc]))
            if(length(grep("^cp-", enc)))  # old Linux
                return(sub("cp-([0-9]+)", "CP\\1", enc))
            if(enc == "EUC") {
                ## let's hope it is a ll_* name.
                if(length(grep("^[[:alpha:]]{2}_", x[1]))) {
                    ll <- substr(x[1], 1, 2)
                    return(switch(ll, "jp"="EUC-JP", "kr"="EUC-KR",
                                  "zh"="GB2312"))
                }
            }
        }
        ## let's hope it is a ll_* name.
        if(length(grep("^[[:alpha:]]{2}_", x[1]))) {
            ll <- substr(x[1], 1, 2)
            if(enc == "utf8") return(c("UTF-8", guess(ll)))
            else return(guess(ll))
        }
        return(NA_character_)
    }
}
