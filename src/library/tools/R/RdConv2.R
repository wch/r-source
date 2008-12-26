RdTags <- function(Rd) {
    sapply(Rd, function(element) attr(element, "Rd_tag"))
}

isBlankRd <- function(x)
    length(grep("^[[:blank:]]*\n?$", x)) == length(x) # newline optional

isBlankLineRd <- function(x) 
    attr(x, "srcref")[2] == 0 &&  # Starts in column 1
    length(grep("^[[:blank:]]*\n", x)) == length(x)   # newline required
    
stopRd <- function(block, ...) {
    srcref <- attr(block, "srcref")
    if (is.null(srcref)) stop(...)
    else {
    	loc <- paste(basename(attr(srcref, "srcfile")$filename),":",srcref[1],sep="")
    	if (srcref[1] != srcref[3]) loc <- paste(loc, "-", srcref[3], sep="")
    	stop(call.=FALSE, loc,":",...)
    }
}
    
preprocessRd <- function(blocks, defines) {
    # Process ifdef's.
    tags <- RdTags(blocks)
    while (length(ifdef <- which(tags %in% c("#ifdef", "#ifndef")))) {
	ifdef <- ifdef[1]
	target <- blocks[[ifdef]][[1]][[1]]
	# The target will have picked up some whitespace and a newline
	target <- gsub("[[:blank:][:cntrl:]]*", "", target)
	all <- 1:length(tags)
	before <- all[all < ifdef]
	after <- all[all > ifdef]

	if ((target %in% defines) == (tags[ifdef] == "#ifdef")) {
	    tags <- c(tags[before], RdTags(blocks[[ifdef]][[2]]), tags[after])
	    blocks <- c(blocks[before], blocks[[ifdef]][[2]], blocks[after])
	} else {
	    tags <- c(tags[before], tags[after])
	    blocks <- c(blocks[before], blocks[after])
	}
    } 
    # Save the tags
    attr(blocks, "RdTags") <- tags
    blocks
}    
    
sectionOrder <- c("\\title"=1, "\\name"=2, "\\description"=3, "\\usage"=4, "\\synopsis"=4,
                   "\\arguments"=5, "\\format"=5, "\\details"=6, "\\value"=7, "\\note"=7, "\\section"=7,
                   "\\author" = 8, "\\references"=8, "\\source"=8, "\\seealso"=9, "\\examples"=10)
                   
sectionTitles <- c("\\description"="Description", "\\usage"="Usage", "\\synopsis"="Usage",
                   "\\arguments"="Arguments", "\\format"="Format", "\\details"="Details", "\\note"="Note",
                   "\\section"="section", "\\author"="Author(s)", "\\references"="References", "\\source"="Source", 
                   "\\seealso"="See Also", "\\examples"="Examples", "\\value"="Value")

Rd2HTML <- function(Rd, out="", package="", defines=.Platform$OS.type) {
    # These correspond to HTML wrappers
    HTMLTags <- c("\\bold"="B",
    	          "\\cite"="CITE",
                  "\\code"="code", 
                  "\\command"="CODE",
                  "\\dfn"="DFN",
                  "\\emph"="EM", 
                  "\\kbd"="KBD",
                  "\\preformatted"="PRE",
                  "\\special"="PRE",
                  "\\strong"="STRONG",
                  "\\var"="VAR")
    # These have simple substitutions
    HTMLEscapes <- c("\\R"='<font face="Courier New,Courier" color="#666666"><b>R</b></font>',
    		     "\\cr"="<br>",
    		     "\\dots"="...",
    		     "\\ldots"="...")
    # These correspond to idiosyncratic wrappers
    HTMLLeft <- c("\\acronym"='<acronym><span class="acronym">',
    		  "\\dontrun"='## Not run:',
    		  "\\donttest"="",
    		  "\\env"='<span class="env">',    		  
                  "\\file"='&lsquo;<span class="file">',
                  "\\option"='<span class="option">',
                  "\\pkg"='<span class="pkg">',
                  "\\samp"='<span class="samp">',
                  "\\sQuote"="&lsquo;",
                  "\\dQuote"="&ldquo;")
    HTMLRight <- c("\\acronym"='</span></acronym>',
    		   "\\dontrun"='## End(Not run)',
    		   "\\donttest"="",
    		   "\\env"="</span>",
                   "\\file"='</span>&rsquo;',
                   "\\option"="</span>",
                   "\\pkg"="</span>",
                   "\\samp"="</span>",
                   "\\sQuote"="&rsquo;",
                   "\\dQuote"="&rdquo;")
                   
    addParaBreaks <- function(x) {
	if (isBlankLineRd(x)) x <- "\n</p>\n<p>\n"
	x
    }
    
    # FIXME: what other substitutions do we need?
    htmlify <- function(x) {
	x <- gsub("&", "&amp;", x)
	x <- gsub("<", "&lt;", x)
	gsub(">", "&gt;", x)
    }

    writeWrapped <- function(tag, block) {
    	cat("<", HTMLTags[tag],">", sep="", file=con)
    	writeContent(block, tag)
    	cat("</", HTMLTags[tag],">", sep="", file=con)
    }
    
    writeLink <- function(tag, block) { # FIXME This doesn't handle aliases, and 
                                        # doesn't cover all variations
    	option <- attr(block, "Rd_option")
    	cat('<a href="', file=con)
    	if (!is.null(option))
    	    cat('../../', option, '/html/', sep="", file=con)
    	writeContent(block, tag)
    	if (tag == "\\linkS4class")
    	    cat('-class', file=con)
    	cat('.html">', file=con)
    	writeContent(block, tag)
    	cat('</a>', file=con)
    }
    
    writeComment <- function(txt) {
       	txt <- sub("^%", "", txt)
       	txt <- gsub("--", "- - ", txt)
       	txt <- gsub(">", "&gt;", txt)
	cat("<!-- ", txt, " -->\n", file=con)
    }
    
    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
	UNKNOWN = ,
	VERB = ,
	RCODE = cat(htmlify(block), file=con),
	TEXT = cat(addParaBreaks(htmlify(block)), file=con), 
	COMMENT = {},
	LIST =,
	"\\describe"=,
	"\\enumerate"=,
	"\\itemize"=writeContent(block, tag),
	"\\bold"=,
	"\\cite"=,
	"\\code"=, 
	"\\command"=,
	"\\dfn"=,
	"\\emph"=, 
	"\\kbd"=,
	"\\preformatted"=,
	"\\special"=,
	"\\strong"=,
	"\\var"= writeWrapped(tag, block),
	"\\linkS4class" =,
	"\\link" = writeLink(tag, block),
	"\\email" = cat('<a href="mailto:', block[[1]], '">', htmlify(block[[1]]), '</a>', sep="", file=con),
	"\\url" = cat('<a href="', block[[1]], '">', htmlify(block[[1]]), '</a>', sep="", file=con),
	"\\cr" =,
	"\\dots" =,
	"\\ldots" =,
	"\\R" = cat(HTMLEscapes[tag], file=con),
	"\\acronym" =,
	"\\dontrun" =,
	"\\donttest" =,
	"\\env" =,
	"\\file" =,
	"\\option" =,
	"\\pkg" =,
	"\\samp" =,
	"\\sQuote" =,
	"\\dQuote" = {
	    cat(HTMLLeft[tag], file=con)
	    writeContent(block, tag)
	    cat(HTMLRight[tag], file=con)
	},
	"\\enc" =  # FIXME:  this should sometimes use the first arg 
	    writeContent(block[[2]], tag),
	"\\eqn" = {
	    if (is.null(RdTags(block)))  # the two arg form has no tags
	   	block <- block[[2]]
	    cat("<i>", file=con)
	    writeContent(block, tag)
	    cat("</i>", file=con)
	},
	"\\deqn" = {
	    if (is.null(RdTags(block)))  # the two arg form has no tags
	   	block <- block[[2]]
	    cat('</p><p align="center"><i>', file=con)
	    writeContent(block, tag)
	    cat('</i></p><p>', file=con)
	},
	"\\dontshow" =,
	"\\testonly" = {}, # do nothing
	"\\method" =,
	"\\S3method" = {
	    cat('## S3 method for class &lsquo;', file=con)
	    writeContent(block[[2]], tag)
	    cat('&rsquo;:\n', file=con)
	    writeContent(block[[1]], tag)
	},
	"\\S4method" = {
	    cat('## S4 method for signature &lsquo;', file=con)
	    writeContent(block[[2]], tag)
	    cat('&rsquo;:\n', file=con)
	    writeContent(block[[1]], tag)
	},
	"\\tabular" = writeTabular(block),
        stopRd(block, "Tag ", tag, " not recognized."))
    }
    
    writeTabular <- function(table) {
    	format <- table[[1]]
    	content <- table[[2]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, "\\tabular format must be simple text")
    	format <- strsplit(format[[1]], "")[[1]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, "Unrecognized \\tabular format: ", table[[1]][[1]])
        format <- c(l="left", c="center", r="right")[format]
        
        content <- preprocessRd(content, defines)
        tags <- attr(content, "RdTags")

        cat('\n</p>\n<table summary="Rd table">\n', file=con)
        newrow <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	cat("<tr>\n ", file=con)
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format))
                    stopRd(table, "Only ", length(format), " columns allowed in this table.")                    
            	cat('<td align="', format[col], '">', sep="", file=con)
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	cat('</td>', file=con)
            	newcol <- TRUE
            },
            "\\cr" = {
            	if (!newcol)
            	    cat('</td>', file=con)
            	cat('\n</tr>\n', file=con)
            	newrow <- TRUE
            },
            writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        if (!newcol)
            cat('</td>', file=con)
        if (!newrow)
            cat('\n</tr>\n', file=con)
        cat('\n</table><p>\n', file=con)
    }        
            
    writeContent <- function(blocks, blocktag) {
        inlist <- FALSE
        
	blocks <- preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")
	
	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
            "\\item" = {
    	    	if (!inlist) {
    	    	    switch(blocktag,
    	    	    "\\arguments"=cat('\n<table summary="R argblock">\n', file=con),
    	    	    "\\itemize"=cat("<ul>\n", file=con),
    	    	    "\\enumerate"=cat("<ol>\n", file=con),
    	    	    "\\value"=,
    	    	    "\\describe"=cat("<dl>\n", file=con))
    	    	    inlist <- TRUE
    		} else {
    		    if (blocktag %in% c("\\itemize", "\\enumerate"))
    		    	cat("</li>", file=con)
    		}
    		switch(blocktag,
    		"\\arguments"={
    		    cat('<tr valign="top"><td><code>', file=con)
    		    writeContent(block[[1]], tag)
    		    cat('</code></td>\n<td>\n', file=con)
    		    writeContent(block[[2]], tag)
    		    cat('</td></tr>', file=con)
    		},
    		"\\value"=,
    		"\\describe"= {
    		    cat("<dt>", file=con)
    		    writeContent(block[[1]], tag)
    		    cat("</dt><dd>", file=con)
    		    writeContent(block[[2]], tag)
    		    cat("</dd>", file=con)
    		},
    		"\\enumerate"=,
    		"\\itemize"= cat("<li>", file=con))
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    	    	    switch(blocktag,
    	    	    "\\arguments"=cat("</table>\n", file=con),
    	    	    "\\value"=,
    	    	    "\\describe"=cat("</dl>\n", file=con))    	    
    		    inlist <- FALSE
    		}
    		writeBlock(block, tag, blocktag)
    	    })
	}
	if (inlist)
	    switch(blocktag,
		"\\arguments"=cat("</table>\n", file=con),
		"\\itemize"=cat("</ul>\n", file=con),
		"\\enumerate"=cat("</ol>\n", file=con),
		"\\value"=,
		"\\describe"=cat("</dl>\n", file=con))  
    }
    
    writeSection <- function(section, tag) {
    	cat("\n\n<h3>", file=con)    	
    	if (tag == "\\section") {
    	    title <- section[[1]]
    	    section <- section[[2]]
    	    writeContent(title, tag)
    	} else 
    	    cat(sectionTitles[tag], file=con)
    	if (tag %in% c("\\examples","\\synopsis","\\usage"))
    	    para <- "pre" else para <- "p"     	
    	cat("</h3>\n\n<", para, ">", sep="", file=con)
    	writeContent(section, tag)
    	cat("</", para, ">\n", sep="", file=con)
    }
    
    if (is.character(Rd)) Rd <- parse_Rd(Rd)
    
    if (is.character(out)) {
        if(out == "") con <- stdout()
        else {
	    con <- file(out, "w")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }
    
    # Process top level ifdef's.
    Rd <- preprocessRd(Rd, defines)
    sections <- attr(Rd, "RdTags")

    # Print initial comments
    # for (i in seq_along(sections)) {
    # 	if (sections[i] != "COMMENT") break
    #	writeComment(Rd[[i]])
    #}
    
    version <- which(sections == "\\Rdversion")
    if (length(version) == 1 && as.numeric(version[[1]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1)
    	stopRd(Rd[[version[2]]], "Only one \\Rdversion declaration is allowed")
    
    # Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]))))
    	stopRd(Rd[sections == "TEXT"][[bad[1]]], "All text must be in a section")
    	
    # Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding", 
                            "\\keyword", "\\alias", "\\Rdversion")
    Rd <- Rd[!drop]
    sections <- sections[!drop]
    
    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1]]], "Section ", sections[which(bad)[1]], " unrecognized.")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:3],c("\\title", "\\name", "\\description"))) 
    	stopRd(Rd, "Sections \\title, \\name and \\description must exist and be unique in Rd files.")
    
    title <- Rd[[1]]
    name <- Rd[[2]]
    tags <- RdTags(name)
    if (length(tags) > 1 || tags != "TEXT") stopRd(name, "\\name must only contain simple text.")
    
    name <- htmlify(name[[1]])
    
    cat('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>R: ', sep="", file=con)
    writeContent(title, "\\title")
    cat('</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">\n',
        '<link rel="stylesheet" type="text/css" href="../../R.css">\n',
        '</head><body>\n\n',
        '<table width="100%" summary="page for ', name, ' {', package, 
        '}"><tr><td>',name,' {', package, '}</td><td align="right">R Documentation</td></tr></table>\n',
        '<h2>', sep="", file=con)
    writeContent(title, "\\title")
    cat('</h2>\n', file=con)
    
    for (i in seq_along(sections)[-(1:2)])
    	writeSection(Rd[[i]], sections[i])
    
    version <- packageDescription(package, fields="Version")
    cat('\n',
        '<hr><div align="center">[Package <em>', package, 
        '</em> version ', version, ' <a href="00Index.html">Index]</a></div>\n',
        '</body></html>\n',
        sep='', file=con)
    return(out)
}

checkRd <- function(Rd, defines=.Platform$OS.type, unknownOkay = FALSE, listOkay = FALSE) {

    checkWrapped <- function(tag, block) {
    	checkContent(block, tag)
    }
    
    checkLink <- function(tag, block) { # FIXME This doesn't handle aliases, and 
                                        # doesn't cover all variations
    	option <- attr(block, "Rd_option")
    	checkContent(option, tag)
    	checkContent(block, tag)
    }
    
    checkBlock <- function(block, tag, blocktag) {
	switch(tag,
	UNKNOWN = if (!unknownOkay) stopRd(block, "Unrecognized macro ", block[[1]]),
	VERB = ,
	RCODE = ,
	TEXT = , 
	COMMENT = {},
	LIST = if (!listOkay && length(block)) stopRd(block, "Unnecessary braces"),
	"\\describe"=,
	"\\enumerate"=,
	"\\itemize"=,
	"\\bold"=,
	"\\cite"=,
	"\\code"=, 
	"\\command"=,
	"\\dfn"=,
	"\\emph"=, 
	"\\kbd"=,
	"\\preformatted"=,
	"\\special"=,
	"\\strong"=,
	"\\var"= checkContent(tag, block),
	"\\linkS4class" =,
	"\\link" = checkLink(tag, block),
	"\\email" =,
	"\\url" =,
	"\\cr" =,
	"\\dots" =,
	"\\ldots" =,
	"\\R" = {},
	"\\acronym" =,
	"\\dontrun" =,
	"\\donttest" =,
	"\\env" =,
	"\\file" =,
	"\\option" =,
	"\\pkg" =,
	"\\samp" =,
	"\\sQuote" =,
	"\\dQuote" = checkContent(block, tag),
	"\\method" =,
	"\\S3method" =,
	"\\S4method" =,
	"\\enc" = {
	    checkContent(block[[1]], tag)
	    checkContent(block[[2]], tag)
	},
	"\\eqn" =,
	"\\deqn" = {
	    if (is.null(RdTags(block))) { # the two arg form has no tags
	        checkContent(block[[1]])
	   	checkContent(block[[2]])
	    } else checkContent(block, tag)
	},
	"\\dontshow" =,
	"\\testonly" = checkContent(block, tag),
	"\\tabular" = checkTabular(block),
        stopRd(block, "Tag ", tag, " not recognized."))
    }
    
    checkTabular <- function(table) {
    	format <- table[[1]]
    	content <- table[[2]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, "\\tabular format must be simple text")
    	format <- strsplit(format[[1]], "")[[1]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, "Unrecognized \\tabular format: ", table[[1]][[1]])
        content <- preprocessRd(content, defines)
        tags <- attr(content, "RdTags")

        newrow <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format))
                    stopRd(table, "Only ", length(format), " columns allowed in this table.")                    
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	newcol <- TRUE
            },
            "\\cr" = {
            	newrow <- TRUE
            },
            checkBlock(content[[i]], tags[i], "\\tabular"))
        }
    }        
            
    checkContent <- function(blocks, blocktag) {
        inlist <- FALSE
        
	blocks <- preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")
	
	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
            "\\item" = {
    	    	if (!inlist) inlist <- TRUE
    		switch(blocktag,
    		"\\arguments"={
    		    checkContent(block[[1]], tag)
    		    checkContent(block[[2]], tag)
    		},
    		"\\value"=,
    		"\\describe"= {
    		    checkContent(block[[1]], tag)
    		    checkContent(block[[2]], tag)
    		},
    		"\\enumerate"=,
    		"\\itemize"= {})
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {   	    
    		    inlist <- FALSE
    		}
    		checkBlock(block, tag, blocktag)
    	    })
	}
    }
    
    checkSection <- function(section, tag) {
    	if (tag == "\\section") {
    	    title <- section[[1]]
    	    section <- section[[2]]
    	    checkContent(title, tag)
    	}
    	checkContent(section, tag)
    }
    
    checkUnique <- function(tag) {
    	which <- which(sections == tag)
    	if (length(which) < 1)
    	    stopRd(Rd, "Must have a ", tag)
    	else if (length(which) > 1)
    	    stopRd(Rd[[which[2]]], "Only one ", tag, " is allowed")
    }
    
    if (is.character(Rd)) Rd <- parse_Rd(Rd)
    
    # Process top level ifdef's.
    Rd <- preprocessRd(Rd, defines)
    sections <- attr(Rd, "RdTags")

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1 && as.numeric(version[[1]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1)
    	stopRd(Rd[[version[2]]], "Only one \\Rdversion declaration is allowed")
    	
    checkUnique("\\title")
    checkUnique("\\name")
    checkUnique("\\description")
    
    name <- Rd[[which(sections == "\\name")]]
    tags <- RdTags(name)
    if (length(tags) > 1 || tags != "TEXT") stopRd(name, "\\name must only contain simple text.")

    for (i in seq_along(sections))
    	checkSection(Rd[[i]], sections[i])
    
    TRUE
}
   