## *ANY* print method should return its argument invisibly!


##-     nn <- names(x)
##-
##-     for (i in 1:NCOL(x)) {
##- 	xr <- x[[i]]
##- 	if (substr(nn[i],1,2) == "Pr") {
##- 	    x[[i]] <- format.pval(xr, digits = max(1, min(5, digits - 1)), na="")
##- 	    if(signif.stars)
##- 		x$Signif <- c(symnum(xr[!is.na(xr)], corr = FALSE,
##- 				     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
##- 				     symbols = c("***", "**", "*", ".", " ")),
##- 			      "") ## 'nterms' ~= 'Residuals' have no P-value
##-
##- 	} else if (!is.factor(xr) && is.numeric(xr)) {
##- 	    cxr <- format(zapsmall(xr, digits=digits), digits=digits)
##- 	    cxr[is.na(xr)] <- ""
##- 	    x[[i]] <- cxr
##- 	}
##-     }
##-     print.data.frame(x)


