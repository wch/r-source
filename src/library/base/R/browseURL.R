browseURL <- function(url, browser = getOption("browser")) {
    if(!is.character(url) || !(length(url) == 1) || (nchar(url) == 0))
        stop("url must be a non-empty character string")
    if(!is.character(browser)
       || !(length(browser) == 1)
       || (nchar(browser) == 0))
        stop("browser must be a non-empty character string")
    switch(.Platform$OS.type,
           "windows" = return(shell.exec(url)),
           "mac" = stop("don't know how to browse URLs on the Mac"))
    remoteCmd <-
        switch(basename(browser),
               "netscape" =, "mozilla" =, "opera" =, {
                   paste("-remote \"openURL(",
                         ## Quote ',' and ')' ...
                         gsub("([,)])", "%\\1", url), ")\"",
                         sep = "")
               },
               "kfmclient" = paste("openURL", url),
               "galeon" = paste("-x", url),
               "gnome-moz-remote" =, "open" =, url)
    system(paste(browser, remoteCmd, "2>/dev/null ||",
                 browser, url, "&"))
}
