toHTML <- function(x, ...) {
  UseMethod("toHTML")
}

HTMLheader <- function(title, logo=TRUE, up=NULL, top=file.path(Rhome, "doc/html/index.html"),
                       Rhome="", headerTitle = paste("R:", title), outputEncoding = "UTF-8") {
    result <- c('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
        paste('<html><head><title>', headerTitle, '</title>', sep=''),
        paste('<meta http-equiv="Content-Type" content="text/html; charset=',
              mime_canonical_encoding(outputEncoding), '">', sep=''),
        paste('<link rel="stylesheet" type="text/css" href="', 
              file.path(Rhome, 'doc/html/R.css'), '">', sep=''),
        '</head><body>',
	paste('<h1>', title))
    if (logo)
    	result <- c(result, paste('<img class="toplogo" src="',
	      file.path(Rhome, 'doc/html/logo.jpg'), '" alt="[R logo]"></h1>', sep=''))
    result <- c(result, '</h1>', '<hr>')
    if (!is.null(up) || !is.null(top)) {
    	result <- c(result, '<div align="center">')
    	if (!is.null(up)) 
    	    result <- c(result, 
    	        paste('<a href="', up, '"><img src="', 
    	              file.path(Rhome, 'doc/html/left.jpg'), 
    	              '" alt="[Up]" width="30" height="30" border="0"></a>',
    	              sep=''))
    	if (!is.null(top))
    	    result <- c(result,
    	    	paste('<a href="', top, '"><img src="',
    	    	      file.path(Rhome, 'doc/html/up.jpg'), 
    	    	      '" alt="[Top]" width="30" height="30" border="0"></a>',
    	    	      sep=''))
    	result <- c(result, '</div>')
    }
    result
}

toHTML.packageIQR <- function(x, ...)
{
    db <- x$results

    # Re-encode as utf-8
    x$title <- iconv(x$title, to="UTF-8")
    x$footer <- iconv(x$footer, to="UTF-8")
    db <- iconv(db, to="UTF-8")
    
    ## Split according to Package.    
    out <- if(nrow(db) == 0L)
         NULL
    else
        lapply(split(1 : nrow(db), db[, "Package"]),
               function(ind) db[ind, c("Item", "Title"),
                                drop = FALSE])
                                
    result <- HTMLheader(paste('R', x$title), Rhome="../..", up="html/00Index.html")
    
    for(pkg in names(out)) {
        result <- c(result, 
                    paste('<h2>', htmlify(x$title), ' in package &lsquo;',
                                  htmlify(pkg), '&rsquo;</h2>', sep = ''),
                    '<table cols="2" width="100%">',
                    paste('<tr>\n',
                          ' <td align="left" valign="top" width="10%">\n',
                          htmlify(out[[pkg]][, "Item"]),
                          '\n </td>\n <td align="left" valign="top" width="90%">\n',
                          htmlify(out[[pkg]][, "Title"]),
                          '\n </td>\n</tr>\n', sep = ''),
                    '</table>')
    }
    if(!is.null(x$footer))
    	result <- c(result, '<p>',
    	                    htmlify(x$footer),
    	                    '</p>')
    result <- c(result, '</body></html>')
    result
}
