citation <- function() {
    cat("To cite R in publications, use\n\n")
    msg <- paste("R Development Core Team (", version$year, "). ", 
                 "R: A language and environment for statistical computing. ",
                 "R Foundation for Statistical Computing, Vienna, Austria. ",
                 "ISBN 3-900051-00-3, URL http://www.R-project.org.",
                 sep="")
    writeLines(strwrap(msg, prefix="  "))
    cat("\n")
    msg <- paste("We have invested a lot of effort in creating R,",
                 "please cite it when using it for data analysis.")
    writeLines(strwrap(msg))
    cat("\nA BibTeX entry for LaTeX users is\n\n")
    cat("  @Manual{,\n")
    cat("     title        = {R: A language and environment for\n")
    cat("                     statistical computing},\n")
    cat("     author       = {{R Development Core Team}},\n")
    cat("     organization = {R Foundation for Statistical Computing},\n")
    cat("     address      = {Vienna, Austria},\n")
    cat("     year         = ",version$year,",\n",sep="")
    cat("     note         = {ISBN 3-900051-00-3},\n")
    cat("     url          = {http://www.R-project.org}\n")
    cat("   }\n\n")

}
