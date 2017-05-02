#  File src/library/utils/R/askYesNo.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2017 The R Core Team
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

askYesNo <- function(msg, default = TRUE, 
                     prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))),
		     ...) {
    if (is.character(prompts) && length(prompts) == 1)
	prompts <- strsplit(prompts, "/")[[1]]
    if (!is.character(prompts) || length(prompts) != 3) {
    	fn <- match.fun(prompts)
    	return(fn(msg = msg, default = default, prompts = prompts, ...))
    }
    choices <- tolower(prompts)
    if (is.na(default)) choices[3L] <- prompts[3L]
    else if (default) choices[1L] <- prompts[1L]
    else choices[2L] <- prompts[2L] 
    msg1 <- paste0("(", paste(choices, collapse = "/"), ") ")
    # readline truncates the prompt; try to avoid that
    if (nchar(paste0(msg, msg1)) > 250) {
	cat(msg, "\n")
	msg <- msg1
    } else
    	msg <- paste0(msg, " ", msg1)
    ans <- readline(msg)
    match <- pmatch(tolower(ans), tolower(choices))
    if (!nchar(ans)) default
    else if (is.na(match)) stop("Unrecognized response ", dQuote(ans))
    else c(TRUE, FALSE, NA)[match]
}
