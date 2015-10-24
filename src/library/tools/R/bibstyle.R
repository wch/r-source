#  File src/library/tools/R/bibstyle.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

# Functions for making Rd and human readable versions of bibentry records.

# Clean up LaTeX accents and braces
cleanupLatex <- function(x) {
    if (!length(x)) return(x)
    latex <- tryCatch(parseLatex(x), error = function(e)e)
    if (inherits(latex, "error")) {
    	x
    } else {
    	deparseLatex(latexToUtf8(latex), dropBraces=TRUE)
    }
}

makeJSS <- function()
    local({

	# First, some utilities

	collapse <- function(strings)
	    paste(strings, collapse="\n")

	# Add a period if there's no sentence punctuation already
	addPeriod <- function(string)
	    sub("([^.?!])$", "\\1.", string)

	# Separate args by sep, add a period at the end.
	sentence <- function(..., sep = ", ") {
	    strings <- c(...)
	    if (length(strings)) {
		addPeriod(paste(strings, collapse = sep))
	    }
	}

	# Now some simple markup

	plain <- function(pages)
	    if (length(pages)) collapse(pages)

	plainclean <- function(s) plain(cleanupLatex(s))

	emph <- function(s)
	    if (length(s)) paste0("\\emph{", collapse(s), "}")

        emphclean <- function(s) emph(cleanupLatex(s))

	# This creates a function to label a field by adding a prefix or suffix (or both)

	label <- function(prefix=NULL, suffix=NULL, style=plain) {
	    force(prefix); force(suffix); force(style)
	    function(s)
		if (length(s)) style(paste0(prefix, collapse(s), suffix))
	}

	labelclean <- function(prefix=NULL, suffix=NULL, style=plain) {
	    f <- label(prefix, suffix, style)
	    function(s) f(cleanupLatex(s))
	}

	# Now the formatters for each particular field.  These take
	# a character vector; if length zero, they return NULL, otherwise
	# a single element character vector putting everything together

	fmtAddress <- plainclean
	fmtBook <- emphclean
	fmtBtitle <- emphclean
	fmtChapter <- labelclean(prefix="chapter ")
	fmtDOI <- label(prefix="\\doi{", suffix="}")
	fmtEdition <- labelclean(suffix=" edition")
	fmtEprint <- plain
	fmtHowpublished <- plainclean
	fmtISBN <- label(prefix = "ISBN ")
	fmtISSN <- label(prefix="ISSN ")
	fmtInstitution <- plainclean
	fmtNote <- plainclean
	fmtPages <- label(prefix="pp. ")
	fmtSchool <- plainclean
	fmtTechreportnumber <- labelclean(prefix="Technical Report ")
	fmtUrl <- label(prefix="\\url{", suffix="}")
	fmtTitle <- function(title)
	    if (length(title))
                paste0("\\dQuote{",
                      addPeriod(collapse(cleanupLatex(title))), "}")

	fmtYear <- function(year) {
	    if (!length(year)) year <- "????"
	    paste0("(", collapse(year), ")")
	}

	# Now some more complicated ones that look at multiple fields
	volNum <- function(paper) {
	    if (length(paper$volume)) {
		result <- paste0("\\bold{", collapse(paper$volume), "}")
		if (length(paper$number))
		    result <- paste0(result, "(", collapse(paper$number), ")")
		result
	    }
	}

	## Format one person object in short "Murdoch DJ" format
	shortName <- function(person) {
	    if (length(person$family)) {
		result <- cleanupLatex(person$family)
		if (length(person$given))
		    paste(result,
			  paste(substr(sapply(person$given, cleanupLatex),
				       1, 1), collapse=""))
		else result
	    }
	    else
		paste(cleanupLatex(person$given), collapse=" ")
	}

	# Format all authors for one paper
	authorList <- function(paper) {
	    names <- sapply(paper$author, shortName)
	    if (length(names) > 1)
		result <- paste( paste(names[-length(names)], collapse=", "),
			    "and", names[length(names)])
	    else
		result <- names
	    result
	}

	# Format all editors for one paper
	editorList <- function(paper) {
	    names <- sapply(paper$editor, shortName)
	    if (length(names) > 1)
		result <- paste( paste(names[-length(names)], collapse=", "),
			    "and", names[length(names)], "(eds.)")
	    else if (length(names))
		result <- paste(names, "(ed.)")
	    else
		result <- NULL
	    result
	}

	extraInfo <- function(paper) {
	    result <- paste(c(fmtDOI(paper$doi), fmtNote(paper$note),
		  fmtEprint(paper$eprint), fmtUrl(paper$url)), collapse=", ")
	    if (nzchar(result)) result
	}

	bookVolume <- function(book) {
	    result <- ""
	    if (length(book$volume))
		result <- paste("volume", collapse(book$volume))
	    if (length(book$number))
		result <- paste(result, "number", collapse(book$number))
	    if (length(book$series))
		result <- paste(result, "series", collapse(book$series))
	    if (nzchar(result)) result
	}

	bookPublisher <- function(book) {
	    if (length(book$publisher)) {
		result <- collapse(book$publisher)
		if (length(book$address))
		    result <- paste(result, collapse(book$address), sep = ", ")
		result
	    }
	}

	procOrganization <- function(paper) {
	    if (length(paper$organization)) {
		result <- collapse(cleanupLatex(paper$organization))
		if (length(paper$address))
		    result <- paste(result, collapse(cleanupLatex(paper$address)), sep =", ")
		result
	    }
	}

	formatArticle <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	             sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		     fmtTitle(paper$title),
		     sentence(fmtBook(paper$journal), volNum(paper), fmtPages(paper$pages)),
		     sentence(fmtISSN(paper$issn), extraInfo(paper))))
	}

	formatBook <- function(book) {
	    authors <- authorList(book)
	    if(!length(authors))
		authors <- editorList(book)

	    collapse(c(fmtPrefix(book),
	               sentence(authors, fmtYear(book$year), sep = " "),
		       sentence(fmtBtitle(book$title), bookVolume(book), fmtEdition(book$edition)),
		       sentence(bookPublisher(book)),
		       sentence(fmtISBN(book$isbn), extraInfo(book))))
	}

	formatInbook <- function(paper) {
	    authors <- authorList(paper)
	    editors <- editorList(paper)
	    if(!length(authors)) {
		authors <- editors
		editors <- NULL
	    }
	    collapse(c(fmtPrefix(paper),
	               sentence(authors, fmtYear(paper$year), sep =" "),
		       fmtTitle(paper$title),
		       paste("In", sentence(editors, fmtBtitle(paper$booktitle), bookVolume(paper),
					    fmtChapter(paper$chapter),
					    fmtEdition(paper$edition), fmtPages(paper$pages))),
		       sentence(bookPublisher(paper)),
		       sentence(fmtISBN(paper$isbn), extraInfo(paper))))
	}

	formatIncollection <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	               sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       fmtTitle(paper$title),
		       paste("In", sentence(editorList(paper), fmtBtitle(paper$booktitle), bookVolume(paper),
					    fmtEdition(paper$edition), fmtPages(paper$pages))),
		       sentence(bookPublisher(paper)),
		       sentence(fmtISBN(paper$isbn), extraInfo(paper))))
	}

	formatInProceedings <- function(paper)
	    collapse(c(fmtPrefix(paper),
	               sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       fmtTitle(paper$title),
		       paste("In", sentence(editorList(paper), fmtBtitle(paper$booktitle), bookVolume(paper),
					    fmtEdition(paper$edition), fmtPages(paper$pages))),
		       sentence(procOrganization(paper)),
		       sentence(fmtISBN(paper$isbn), extraInfo(paper))))

	formatManual <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	               sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       sentence(fmtBtitle(paper$title), bookVolume(paper), fmtEdition(paper$edition)),
		       sentence(procOrganization(paper)),
		       sentence(fmtISBN(paper$isbn), extraInfo(paper))))
	}

	formatMastersthesis <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	               sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       sentence(fmtBtitle(paper$title)),
		       sentence("Master's thesis", fmtSchool(paper$school), fmtAddress(paper$address)),
		       sentence(extraInfo(paper))))
	}

	formatPhdthesis <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	    	       sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       sentence(fmtBtitle(paper$title)),
		       sentence("PhD thesis", fmtSchool(paper$school), fmtAddress(paper$address)),
		       sentence(extraInfo(paper))))
	}

	formatMisc <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	               sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       fmtTitle(paper$title),
		       sentence(fmtHowpublished(paper$howpublished)),
		       sentence(extraInfo(paper))))
	}

	formatProceedings <- function(book) {
	    if (is.null(book$editor)) editor <- "Anonymous (ed.)"
	    else editor <- editorList(book)
	    collapse(c(fmtPrefix(book), # not paper
	    	       sentence(editor, fmtYear(book$year), sep = " "),
		       sentence(fmtBtitle(book$title), bookVolume(book)),
		       sentence(procOrganization(book)),
		       sentence(fmtISBN(book$isbn), fmtISSN(book$issn),
				extraInfo(book))))
	}

	formatTechreport <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	    	       sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       fmtTitle(paper$title),
		       sentence(fmtTechreportnumber(paper$number),
				fmtInstitution(paper$institution),
				fmtAddress(paper$address)),
		       sentence(extraInfo(paper))))
	}

	formatUnpublished <- function(paper) {
	    collapse(c(fmtPrefix(paper),
	    	       sentence(authorList(paper), fmtYear(paper$year), sep = " "),
		       fmtTitle(paper$title),
		       sentence(extraInfo(paper))))
	}

	sortKeys <- function(bib) {
	    result <- character(length(bib))
	    for (i in seq_along(bib)) {
		authors <- authorList(bib[[i]])
		if (!length(authors))
		    authors <- editorList(bib[[i]])
		if (!length(authors))
		    authors <- ""
		result[i] <- authors
	    }
	    result
	}

	# Replace this if you want a bibliography style
	# that puts a prefix on each entry, e.g. [n]
	# The formatting routine will have added a field .index
	# as a 1-based index within the complete list.

	fmtPrefix <- function(paper) NULL

	cite <- function(key, bib, ...)
	    utils::citeNatbib(key, bib, ...) # the defaults are JSS style

	environment()
    })

bibstyle <- local({
    styles <- list(JSS = makeJSS())
    default <- "JSS"
    function(style, envir, ..., .init = FALSE, .default=TRUE) {
        newfns <- list(...)
        if (missing(style) || is.null(style)) {
            if (!missing(envir) || length(newfns) || .init)
            	stop("Changes require specified 'style'")
            style <- default
        } else {
	    if (!missing(envir)) {
		stopifnot(!.init)
		styles[[style]] <<- envir
	    }
	    if (.init) styles[[style]] <<- makeJSS()
	    if (length(newfns) && style == "JSS")
		stop("The default JSS style may not be modified.")
	    for (n in names(newfns))
		assign(n, newfns[[n]], envir=styles[[style]])
            if (.default)
            	default <<- style
        }
        styles[[style]]
    }
})

getBibstyle <- function(all = FALSE) {
    if (all)
    	names(environment(bibstyle)$styles)
    else
    	environment(bibstyle)$default
}

toRd.bibentry <- function(obj, style=NULL, ...) {
    obj <- sort(obj, .bibstyle=style)
    style <- bibstyle(style)
    env <- new.env(hash = FALSE, parent = style)
    bib <- unclass(obj)
    result <- character(length(bib))
    for (i in seq_along(bib)) {
    	env$paper <- bib[[i]]
    	result[i] <- with(env,
    	    switch(attr(paper, "bibtype"),
    	    Article = formatArticle(paper),
    	    Book = formatBook(paper),
    	    InBook = formatInbook(paper),
    	    InCollection = formatIncollection(paper),
    	    InProceedings = formatInProceedings(paper),
    	    Manual = formatManual(paper),
    	    MastersThesis = formatMastersthesis(paper),
    	    Misc = formatMisc(paper),
    	    PhdThesis = formatPhdthesis(paper),
    	    Proceedings = formatProceedings(paper),
    	    TechReport = formatTechreport(paper),
    	    Unpublished = formatUnpublished(paper),
    	    paste("bibtype", attr(paper, "bibtype"),"not implemented") ))
    }
    result
}
