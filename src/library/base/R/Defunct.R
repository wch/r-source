#  File src/library/base/R/Defunct.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

.Defunct <- function(new, package=NULL, msg) {
    fname <- as.character(sys.call(sys.parent())[[1L]])
    if (missing(msg)) {
	msg <- gettextf("'%s' is defunct.\n", fname[length(fname)])
	if(!missing(new))
	    msg <- c(msg, gettextf("Use '%s' instead.\n", new))
	msg <- c(msg,
		 if(!is.null(package))
		 gettextf("See help(\"Defunct\") and help(\"%s-defunct\").",
			  package)
		 else gettext("See help(\"Defunct\")"))
    }
    else msg <- as.character(msg)
    msg <- paste(msg, collapse = "")

    if (missing(new)) new <- NULL
    stop(errorCondition(msg, old = fname, new = new, package = package,
                        class = "defunctError"))
}

## Version <- function() .Defunct("R.Version")
## provide <- function(package) .Defunct()

## <entry>
## Deprecated in 1.2.0
## Defunct in 1.3.0
# getenv <- function(...) .Defunct("Sys.getenv")
## </entry>

## <entry>
## Deprecated in 1.2.3
## Defunct in 1.3.0
## Removed in 1.4.0: conflicts with lattice
## dotplot <- function(...) .Defunct()
## stripplot <- function(...) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.3.0
## Defunct in 1.4.0
## read.table.url <- function(url, method, ...) .Defunct("read.table(url())")
## scan.url <- function(url, file = tempfile(), method, ...)
##     .Defunct("scan(url())")
## source.url <- function(url, file = tempfile(), method, ...)
##     .Defunct("source(url())")
## httpclient <- function(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
##                        file=tempfile(), drop.ctrl.z=TRUE)
##     .Defunct()
## parse.dcf <- function(text = NULL, file = "", fields = NULL,
##                       versionfix = FALSE) .Defunct("read.dcf")
## </entry>

## <entry>
## Deprecated in 1.4.0
## Defunct in 1.5.0
# .Alias <- function(expr) .Defunct()
## </entry>

## <entry>
## Deprecated in 1.6.0
## Defunct in 1.7.0
## machine <- function() .Defunct()
## Machine <- function() .Defunct(".Machine")
## Platform <- function() .Defunct(".Platform")
## restart <- function() .Defunct("try")
## </entry>

## <entry>
## Deprecated in 1.7.0
## Defunct in 1.8.0
## printNoClass <- function(x, digits = NULL, quote = TRUE, na.print = NULL,
##                          print.gap = NULL, right = FALSE, ...)
##     .Defunct()
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
## codes <- function(x, ...) .Defunct()
## codes.factor <- function(x, ...) .Defunct("unclass")
## codes.ordered <- function(x, ...) .Defunct("unclass")
## `codes<-` <- function(x, ..., value) .Defunct()
# removed in 2.9.1, as it caused confusion for an S4 class union of this name.
#print.atomic <- function(x, quote = TRUE, ...) .Defunct("print.default")
## </entry>

## <entry>
## Deprecated in 1.9.0
## Defunct in 2.0.0
## La.eigen <- function(x, symmetric, only.values = FALSE,
##                      method = c("dsyevr", "dsyev")) .Defunct("eigen")
## tetragamma <- function(x) .Defunct("psigamma")
## pentagamma <- function(x) .Defunct("psigamma")
## package.description <- function(pkg, lib.loc = NULL, fields = NULL)
##     .Defunct("packageDescription")
## </entry>

## <entry>
## Deprecated in 2.1.0
## Defunct in 2.2.0
## delay <- function(x, env=.GlobalEnv) .Defunct("delayedAssign")
## loadURL <- function (url, envir = parent.frame(), quiet = TRUE, ...)
##     .Defunct("load(url())")
## </entry>

## Defunct in 2.3.0
## write.table0 <-
## function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
##           eol = "\n", na = "NA", dec = ".", row.names = TRUE,
##           col.names = TRUE, qmethod = c("escape", "double"))
##     .Defunct("write.table")
## format.char <- function(x, width = NULL, flag = "-")
##     .Defunct("format.default")
## </entry>

## <entry>
## Deprecated in 2.3.0
## Defunct in 2.4.0
# La.chol <- function(x) .Defunct("chol")
# La.chol2inv <- function(x, size = ncol(x)) .Defunct("chol2inv")
## </entry>

## <entry>
## Deprecated in 2.4.0
## Defunct in 2.5.0
## symbol.C <- function(name)
## {
##     warning("'symbol.C' is not needed: please remove it", immediate.=TRUE)
##     name
## }
## symbol.For <- function(name)
## {
##     warning("'symbol.For' is not needed: please remove it", immediate.=TRUE)
##     name
## }
## </entry>

## <entry>
## Deprecated in 1999
## Defunct in 2.5.0
# unix <- function(call, intern = FALSE) .Defunct("system")
## </entry>

## <entry>
## Deprecated in 2.7.0
## Defunct in 2.8.0
## gammaCody <- function(x) .Defunct("gamma")
## </entry>

## <entry>
## Deprecated inter alia in 2.8.1
## Defunct in 2.9.0
## manglePackageName <- function (pkgName, pkgVersion) .Defunct()
## </entry>

## <entry>
## Deprecated in 2.12.2 (and only ever experimental)
## Defunct in 2.13.0
## .Import <- function(...)
##     .Defunct(msg = "namespaces should be specified via the 'NAMESPACE' file")
## .ImportFrom <- function(name, ...)
##     .Defunct(msg = "namespaces should be specified via the 'NAMESPACE' file")
## .Export <- function(...)
##     .Defunct(msg = "namespaces should be specified via the 'NAMESPACE' file")
## .S3method <- function(generic, class, method)
##     .Defunct(msg = "namespaces should be specified via the 'NAMESPACE' file")
## </entry>

## <entry>
## Deprecated in 2.14.0
## Defunct in 2.15.0
mem.limits <- function(nsize=NA, vsize=NA) .Defunct("gc")
## </entry>

## <entry>
## Deprecated in 2.13.1
## Defunct in 2.15.0
.readRDS <- function(...) .Defunct("readRDS")
.saveRDS <- function(...) .Defunct("saveRDS")
## </entry>

## <entry>
## Deprecated in 2.5.0
## Removed in 2.15.0
# Sys.putenv <- function(...) .Defunct("Sys.setenv")
## </entry>

## <entry>
## Deprecated in 3.0.0
## Defunct in 3.1.0
.find.package <- function(...).Defunct("find.package")
.path.package <- function(...).Defunct("path.package")
## </entry>
