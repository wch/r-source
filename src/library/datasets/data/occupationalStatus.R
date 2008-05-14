occupationalStatus <-
    matrix(as.integer(c(50, 16, 12, 11,	 2, 12,	 0,  0,
			19, 40, 35, 20,	 8, 28,	 6,  3,
			26, 34, 65, 58, 12,102, 19, 14,
			 8, 18, 66,110, 23,162, 40, 32,
			 7, 11, 35, 40, 25, 90, 21, 15,
			11, 20, 88,183, 46,554,158,126,
			 6,  8, 23, 64, 28,230,143, 91,
			 2,  3, 21, 32, 12,177, 71,106)),
	   8, 8,
	   dimnames =
	   list(origin = as.character(1:8),
		destination= as.character(1:8)))

class(occupationalStatus) <- "table"
