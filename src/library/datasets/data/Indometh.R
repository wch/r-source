### $Id: Indometh.R,v 1.1 2003/12/11 07:16:04 ripley Exp $
### Pharmacokinetic data on the drug Indomethicin
### Kwan, Breault, Umbenhauer, McMahon, and Duggan, (1976) J. of Pharmacokinetics
###  and Biopharmaceutics, 4, 255-280.
### Cited in Davidian and Giltinan (1995), section 2.1
"Indometh" <-
  structure(list(
   Subject = structure(ordered(c(1, 1, 1, 1, 1, 1,
     1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6,
     6, 6, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4,
     4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5), levels=1:6),
     class = c("ordered", "factor"),
     .Label = c("1", "4", "2", "5", "6", "3")),
   time = c(0.25, 0.5, 0.75, 1, 1.25, 2, 3, 4, 5, 6, 8, 0.25, 0.5,
     0.75, 1, 1.25, 2, 3, 4, 5, 6, 8, 0.25, 0.5, 0.75, 1, 1.25, 2, 3,
     4, 5, 6, 8, 0.25, 0.5, 0.75, 1, 1.25, 2, 3, 4, 5, 6, 8, 0.25,
     0.5, 0.75, 1, 1.25, 2, 3, 4, 5, 6, 8, 0.25, 0.5, 0.75, 1, 1.25,
     2, 3, 4, 5, 6, 8),
   conc = c(1.5, 0.94, 0.78, 0.48, 0.37, 0.19, 0.12, 0.11,
     0.08, 0.07, 0.05, 2.03, 1.63, 0.71, 0.7, 0.64, 0.36, 0.32, 0.2,
     0.25, 0.12, 0.08, 2.72, 1.49, 1.16, 0.8, 0.8, 0.39, 0.22, 0.12,
     0.11, 0.08, 0.08, 1.85, 1.39, 1.02, 0.89, 0.59, 0.4, 0.16, 0.11,
     0.1, 0.07, 0.07, 2.05, 1.04, 0.81, 0.39, 0.3, 0.23, 0.13, 0.11,
     0.08, 0.1, 0.06, 2.31, 1.44, 1.03, 0.84, 0.64, 0.42, 0.24, 0.17,
     0.13, 0.1, 0.09)),
row.names = 1:66,
class = c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame"),
formula = conc ~ time | Subject,
labels = list(x = "Time since drug administration",
  y = "Indomethicin concentration"),
units = list(x = "(hr)", y = "(mcg/ml)"))
environment(attr(Indometh, "formula")) <- emptyenv()
