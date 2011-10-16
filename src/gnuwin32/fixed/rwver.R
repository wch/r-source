# keep in step with tools/GETDISTNAME

if(R.version$status == "Under development (unstable)") {
    res <- "R-devel"
} else {
    res <- paste("R-", R.version$major, ".", R.version$minor,
                 tolower(R.version$status) , sep="")
}
cat(res)
