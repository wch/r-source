RdTags <- function(Rd) {
    sapply(Rd, function(element) attr(element, "Rd_tag"))
}

# FIXME: what other substitutions do we need?
htmlify <- function(x) {
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
}

sectionOrder <- c("\\title"=1, "\\name"=2, "\\description"=3, "\\usage"=4, "\\synopsis"=4,
                   "\\arguments"=5, "\\details"=6, "\\value"=7, "\\note"=7, "\\section"=7,
                   "\\author" = 8, "\\references"=8, "\\source"=8, "\\seealso"=9, "\\examples"=10)
                   
sectionTitles <- c("\\description"="Description", "\\usage"="Usage", "\\synopsis"="Usage",
                   "\\arguments"="Arguments", "\\details"="Details", "\\note"="Note",
                   "\\section"="section", "\\author"="Author(s)", "\\references"="References", "\\source"="Source", 
                   "\\seealso"="See Also", "\\examples"="Examples", "\\value"="Value")
                   
Rd2HTML <- function(Rd, con="", package="", defines="windows") {
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
    		  "\\dontrun"='## Not run:\n',
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
                  
    
    processIfdefs <- function(blocks, tags) {
    	# Process ifdef's.
    	while (length(ifdef <- which(tags %in% c("#ifdef", "#ifndef")))) {
    	    ifdef <- ifdef[1]
    	    target <- blocks[[ifdef]][[1]]
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
	return(list(blocks, tags))
    }
    
    writeWrapped <- function(tag, block) {
    	cat("<", HTMLTags[tag],">", sep="", file=con)
    	writeContent(block)
    	cat("</", HTMLTags[tag],">", sep="", file=con)
    }
    
    writeLink <- function(block) {
    	option <- attr(block, "Rd_option")
    	cat('<a href="', file=con)
    	if (!is.null(option))
    	    cat('../../', option, '/html/', sep="", file=con)
    	writeContent(block)
    	cat('.html">', file=con)
    	writeContent(block)
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
	VERB =,
	RCODE =, 
	TEXT = cat(htmlify(block), sep="", file=con),
	COMMENT = {},
	WHITESPACE = cat(block, file=con),
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
	"\\link" = writeLink(block),
	"\\email" = cat('<a href="mailto:', block[[1]], '">', htmlify(block[[1]]), '</a>', sep="", file=con),
	"\\url" = cat('<a href="', block[[1]], '">', htmlify(block[[1]]), '</a>', sep="", file=con),
	"\\cr" =,
	"\\dots" =,
	"\\ldots" =,
	"\\R" = cat(HTMLEscapes[tag], file=con),
	"\\cr{}" =,
	"\\dots{}" =,
	"\\ldots{}" =,
	"\\R{}" = cat(HTMLEscapes[sub("..$", "", tag)], file=con),
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
	    writeContent(block)
	    cat(HTMLRight[tag], file=con)
	},
	"\\eqn" = {
	    if (is.null(RdTags(block)))  # the two arg form has no tags
	   	block <- block[[2]]
	    cat("<i>", file=con)
	    writeContent(block)
	    cat("</i>", file=con)
	},
	"\\deqn" = {
	    if (is.null(RdTags(block)))  # the two arg form has no tags
	   	block <- block[[2]]
	    cat('</p><p align="center"><i>', file=con)
	    writeContent(block)
	    cat('</i></p><p>', file=con)
	},
	"\\dontshow" =,
	"\\testonly" = {}, # do nothing
	"\\method" =,
	"\\S3method" = {
	    cat('## S3 method for class &lsquo;', file=con)
	    writeContent(block[[2]])
	    cat('&rsquo;:\n', file=con)
	    writeContent(block[[1]])
	},
	"\\S4method" = {
	    cat('## S4 method for signature &lsquo;', file=con)
	    writeContent(block[[2]])
	    cat('&rsquo;:\n', file=con)
	    writeContent(block[[1]])
	},
	"\\tabular" = writeTabular(block),
        stop("Tag ", tag, " not recognized."))
    }
    
    writeTabular <- function(table) {
    	format <- table[[1]]
    	content <- table[[2]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stop("\\tabular format must be simple text")
    	format <- strsplit(format[[1]], "")[[1]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stop("Unrecognized \\tabular format: ", table[[1]][[1]])
        format <- c(l="left", c="center", r="right")[format]
        
        tags <- RdTags(content)        
        content <- processIfdefs(content, tags)
        tags <- content[[2]]
        content <- content[[1]]

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
                    stop("Only ", length(format), " columns allowed in this table.")                    
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
        tags <- RdTags(blocks)
        
	blocks <- processIfdefs(blocks, tags)
	tags <- blocks[[2]]
	blocks <- blocks[[1]]
	
	while (length(ifdef <- which(tags %in% c("#ifdef", "#ifndef")))) {
	    ifdef <- ifdef[1]
	    target <- blocks[[ifdef]][[1]]
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
        for (i in seq_along(tags)) {
            tag <- tags[i]
            # eat whitespace following dots
    	    if (i > 1 && tag == "WHITESPACE"
    	              && tags[i-1] %in% c("\\dots", "\\ldots")) next            
            block <- blocks[[i]]
            switch(tag,
            "\\item" = {
    	    	if (!inlist) {
    	    	    switch(blocktag,
    	    	    "\\arguments"=,
    	    	    "\\value"=,
    	    	    "\\itemize"=cat("<ul>\n", file=con),
    	    	    "\\enumerate"=cat("<ol>\n", file=con),
    	    	    "\\describe"=cat("<dl>\n", file=con))
    	    	    inlist <- TRUE
    		}
    		switch(blocktag,
    		"\\arguments"=,
    		"\\value"=,
    		"\\describe"= {
    		    cat("<dt>", file=con)
    		    writeContent(block[[1]], tag)
    		    cat("</dt><dd>", file=con)
    		    writeContent(block[[2]], tag)
    		    cat("</dd>", file=con)
    		},
    		"\\enumerate"=,
    		"\\itemize"= {
    		    cat("<li>", file=con)
    		    writeContent(block, tag)
    		    cat("</li>", file=con)
    		})
    	    },
    	    COMMENT=,
    	    WHITESPACE=writeBlock(block, tag, blocktag),
    	    { # default
    	    	if (inlist) {
    	    	    switch(blocktag,
    	    	    "\\arguments"=,
    	    	    "\\value"=,
    	    	    "\\itemize"=cat("</ul>\n", file=con),
    	    	    "\\enumerate"=cat("</ol>\n", file=con),
    	    	    "\\describe"=cat("</dl>\n", file=con))    	    
    		    inlist <- FALSE
    		}
    		writeBlock(block, tag, blocktag)
    	    })
	}
	if (inlist)
	    switch(blocktag,
		"\\arguments"=,
		"\\value"=,
		"\\itemize"=cat("</ul>\n", file=con),
		"\\enumerate"=cat("</ol>\n", file=con),
		"\\describe"=cat("</dl>\n", file=con))  
    }
    
    writeSection <- function(section, tag) {
        
    	if (tag == "\\section") {
    	    title <- section[[1]]
    	    tags <- RdTags(title)
    	    if (length(tags) != 1 || tags != "TEXT") 
    	    	stop("Section title must be simple text")
    	    title <- title[[1]]
    	    section <- section[[2]]
    	} else 
    	    title <- sectionTitles[tag]
    	    
    	if (tag %in% c("\\examples","\\synopsis","\\usage"))
    	    para <- "pre" else para <- "p"
    	   
    	cat("\n<h3>", title, "</h3>\n\n<", para, ">", sep="", file=con)
    	writeContent(section, tag)
    	cat("\n</", para, ">\n", sep="", file=con)
    }
    
    if (is.character(Rd)) Rd <- parse_Rd(Rd)
    
    if (is.character(con)) {
        if(con == "") con <- stdout()
        else {
	    con <- file(con, "w")
	    on.exit(close(con))
	}
    }
    
    sections <- RdTags(Rd)
    
    # Process top level ifdef's.
    Rd <- processIfdefs(Rd, sections)
    sections <- Rd[[2]]
    Rd <- Rd[[1]]

    # Print initial comments
    for (i in seq_along(sections)) {
    	if (sections[i] != "COMMENT") break
        writeComment(Rd[[i]])
    }
    
    # Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "WHITESPACE", "\\concept", "\\docType", "\\encoding", 
                            "\\keyword", "\\alias")
    Rd <- Rd[!drop]
    sections <- sections[!drop]
    
    sortorder <- sectionOrder[sections]
    if (any(is.na(sortorder)))
    	stop("Section ", sections[which(is.na(sortorder))], " unrecognized.")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:3],c("\\title", "\\name", "\\description"))) 
    	stop("Sections \\title, \\name and \\description must exist and be unique in Rd files.")
    
    title <- Rd[[1]]
    tags <- RdTags(title)
    title <- Rd[ tags != "WHITESPACE" ]
    tags <- tags[ tags != "WHITESPACE" ]
    if (!identical(tags, "TEXT")) stop("\\title section must only contain text.")
    
    name <- Rd[[2]]
    tags <- RdTags(name)
    if (!identical(tags, "TEXT")) stop("\\name must only contain text.")
    
    title <- htmlify(title[[1]])
    name <- htmlify(name[[1]])
    
    cat('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>', title, '</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">\n',
        '<link rel="stylesheet" type="text/css" href="../../R.css">\n',
        '</head><body>\n\n',
        '<table width="100%" summary="page for ', name, ' {', package, 
        '}"><tr><td>',name,' {', package, '}</td><td align="right">R Documentation</td></tr></table>\n',
        '<h2>', title, '</h2>\n', sep="", file=con)
    
    for (i in seq_along(sections)[-(1:2)])
    	writeSection(Rd[[i]], sections[i])
    
    version <- packageDescription(package, fields="Version")
    cat('\n',
        '<hr><div align="center">[Package <em>', package, 
        '</em> version ', version, ' <a href="00Index.html">Index]</a></div>\n',
        '</body></html>\n',
        sep='', file=con)
}
    