menu <- function(choices, graphics = FALSE, title = "")
{
    nc <- length(choices)
    cat(title, "\n")
    if(nc) for (i in 1:nc)
	cat(i, ":", choices[i]," \n", sep = "")
    repeat {
	cat("Selection: ")
	ind <- .Internal(menu(as.character(choices)))
	if(ind <= nc)
	    return(ind)
	cat("Enter an item from the menu, or 0 to exit\n")
    }
}
