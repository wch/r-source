#  File src/library/base/R/zdatetime.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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
#  http://www.r-project.org/Licenses/

## needs to run after paste()
.leap.seconds <- local({
    .leap.seconds <-
        c("1972-6-30", "1972-12-31", "1973-12-31", "1974-12-31",
          "1975-12-31", "1976-12-31", "1977-12-31", "1978-12-31",
          "1979-12-31", "1981-6-30", "1982-6-30", "1983-6-30",
          "1985-6-30", "1987-12-31", "1989-12-31", "1990-12-31",
          "1992-6-30", "1993-6-30", "1994-6-30","1995-12-31",
          "1997-6-30", "1998-12-31", "2005-12-31", "2008-12-31", 
          "2012-6-30", "2015-6-30")
    .leap.seconds <- strptime(paste(.leap.seconds , "23:59:60"),
                              "%Y-%m-%d %H:%M:%S")
    c(as.POSIXct(.leap.seconds, "GMT")) # lose the timezone
})
