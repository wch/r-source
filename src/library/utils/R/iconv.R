iconv <- function(x, from, to) .Internal(iconv(x, from, to))

iconvlist <- function() sort(.Internal(iconv(NULL, "", "")))

localeToCharset <- function(locale = Sys.getlocale("LC_CTYPE"))
{
    guess <- function(en)
    {
        if(en %in% c("aa", "af", "an", "br", "ca", "da", "de", "en",
                         "es", "et", "eu", "fi", "fo", "fr", "ga", "gl",
                         "gv", "id", "is", "it", "kl", "kw", "ms", "nb",
                         "ml", "nn", "no", "oc", "om", "pt", "so", "sq",
                         "st", "sv", "tl", "uz", "wa", "xh", "zu"))
                return("ISO8859-1")
        if(en %in% c("bs", "cs", "hr", "hu", "pl", "ro", "sk", "sl"))
            return("ISO8859-2")
        if(en %in% "mt") return("ISO8859-3")
            if(en %in% c("mk", "ru")) return("ISO8859-5")
        if(en %in% "ar") return("ISO8859-6")
        if(en %in% "el") return("ISO8859-7")
        if(en %in% c("he", "iw")) return("ISO8859-8")
        if(en %in% "tr") return("ISO8859-9")
        if(en %in% "lg") return("ISO8859-10")
        if(en %in% c("lt", "lv", "mi")) return("ISO8859-13")
        if(en %in% "cy") return("ISO8859-14")
        return(as.character(NA))
    }
    if(.Platform$OS.type == "windows") {
        x <- strsplit(locale, ".", fixed=TRUE)[[1]]
        if(length(x) != 2) return(as.character(NA))
        else return(paste("CP", x[2], sep=""))
    } else {
        ## Assume locales are like  en_US[.utf8[@euro]]
        x <- strsplit(locale, ".", fixed=TRUE)[[1]]
        enc <- if(length(x) == 2) gsub("@.*$o", "", x[2]) else ""
        if(nchar(enc) && enc != "utf8") {
            known <-
                c("ISO8859-1", "ISO8859-2", "ISO8859-3", "ISO8859-6",
                  "ISO8859-7", "ISO8859-8", "ISO8859-9", "ISO8859-10",
                  "ISO8859-13", "ISO8859-14", "ISO8859-15",
                  "CP1251", "CP1255", "EUC-JP",
                  "EUC-KR", "EUC-TW", "GEORGIAN-PS", "KOI8-U", "TCVN",
                  "BIG5" , "GB2312", "GB18030", "GBK")
            names(known) <-
                c("iso88591", "iso88592", "iso88593", "iso88596",
                  "iso88597", "iso88598", "iso88599", "iso885910",
                  "iso885913", "iso885914", "iso885915",
                  "cp1251", "cp1255", "eucjp",
                  "euckr", "euctw", "georgianps", "koi8u", "tcvn",
                  "big5" , "gb2312", "gb18030", "gbk")
            if(enc %in% names(known)) return(as.vector(known[enc]))
        }
        ## let's hope it is a en_* name.
        if(length(grep("^[[:alpha:]]{2}_", x[1]))) {
            en <- substr(x[1], 1, 2)
            if(enc == "utf8") return(c("UTF-8", guess(en)))
            else return(guess(en))
        }
        return(as.character(NA))
    }
}
