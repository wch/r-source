# keep in step with tools/GETDISTNAME

status <- switch(R.version$status,
                 "Under development (unstable)" = "dev",
                 tolower(R.version$status))

res <- paste("R-", R.version$major, ".", R.version$minor, status, sep="")
cat(res)
