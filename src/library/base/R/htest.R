print.htest<-function(x, digits = 4, quote = T, prefix = "")
{
        cat("\n\t", x$method, "\n\n")
        cat("data: ", x$data.name, "\n")
        if(!is.null(x$statistic))
                cat(names(x$statistic), " = ", format(round(x$statistic, 4)), 
                        ", ", sep = "")
	if(!is.null (x$parameter))	
		cat(paste(names(x$parameter), " = ", format(round(x$parameter,
			3)), ",", sep = ""), "")
        cat("p-value =", format.pval(x$p.value, digits= digits), "\n")
        if(!is.null(x$alternative)) {
                if(!is.null(x$null.value)) {
                        if(length(x$null.value) == 1) {
                                if (x$alternative == "two.sided" )
					alt.char <- "not equal to"
				else if( x$alternative == "less" )
					alt.char <- "less than"
				else if( x$alternative == "greater" )
                                  	alt.char <- "greater than"
                                cat("alternative hypothesis:", "true", names(x$
                                  null.value), "is", alt.char, x$null.value, 
                                  "\n")
                        }
                        else {
                                cat("alternative hypothesis:", x$alternative, 
                                  "\n")
                                cat("null values:\n")
                                print(x$null.value)
                        }
                }
                else cat("alternative hypothesis:", x$alternative, "\n")
        }
        if(!is.null(x$conf.int)) {
                cat(format(100 * attr(x$conf.int, "conf.level")), 
                        "percent confidence interval:\n", format(c(x$conf.int[1
                        ], x$conf.int[2])), "\n")
        }
        if(!is.null(x$estimate)) {
                cat("sample estimates:\n")
                print(x$estimate)
        }
        cat("\n")
        invisible(x)
}
