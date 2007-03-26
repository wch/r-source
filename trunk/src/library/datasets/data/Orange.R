### $Id: Orange.R,v 1.1 2003/12/11 07:16:04 ripley Exp $
### Growth of orange trees
### Draper and Smith (1981), Exercise 10.N, p. 524.
"Orange" <-
  structure(list
  (Tree = structure(ordered(c(2, 2, 2, 2, 2, 2, 2,
     4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5,
     3, 3, 3, 3, 3, 3, 3), levels=1:5),
     class = c("ordered", "factor"),
     .Label = c("3", "1", "5", "2", "4")),
   age = c(118, 484, 664,
     1004, 1231, 1372, 1582, 118, 484, 664, 1004, 1231, 1372, 1582,
     118, 484, 664, 1004, 1231, 1372, 1582, 118, 484, 664, 1004, 1231,
     1372, 1582, 118, 484, 664, 1004, 1231, 1372, 1582),
   circumference = c(30,
     58, 87, 115, 120, 142, 145, 33, 69, 111, 156, 172, 203, 203,
     30, 51, 75, 108, 115, 139, 140, 32, 62, 112, 167, 179, 209, 214,
     30, 49, 81, 125, 142, 174, 177)),
row.names = 1:35,
class = c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame"),
formula = circumference ~ age | Tree,
labels = list(x = "Time since December 31, 1968", y = "Trunk circumference"),
units = list(x = "(days)", y = "(mm)"))
environment(attr(Orange, "formula")) <- emptyenv()
