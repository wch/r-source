menu <- function(choices, graphics = FALSE, title = "")
{
    nc <- length(choices)
    cat(title, "\n")
    for (i in seq(length=nc))
	cat(i, ":", choices[i]," \n", sep = "")
    repeat {
	ind <- .Internal(menu(as.character(choices)))
	if(ind <= nc)
	    return(ind)
	cat("Enter an item from the menu, or 0 to exit\n")
    }
}
