## <entry>
## Deprecated in 1.9.0
package.contents <- function(pkg, lib.loc=NULL)
{
    .Deprecated(package="utils")
    file <- system.file("CONTENTS", package = pkg, lib.loc = lib.loc)
    if(file == "") {
        warning(paste("Cannot find CONTENTS file of package", pkg))
        return(NA)
    }

    read.dcf(file=file, fields=c("Entry", "Keywords", "Description"))
}
## </entry>
