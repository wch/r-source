## [Bug 18654] xyTable fails when both x and y are NA (2024-01-16)
##             https://bugs.r-project.org/show_bug.cgi?id=18654
## Attachment 3292 https://bugs.r-project.org/attachment.cgi?id=3292
## Scenarios authored by Heather Turner in comments #1 and #5

## Case 2: one variable has NA - works fine
## (first combination from Case 1 now has NA)
iris2 <- iris[1:10, 3:4]
iris2[3, 1] <- NA
xyTable(iris2)

## Case 3: both x and y are NA for one case - no good
## (`number` should be the same as for Case 2)
iris3 <- iris[1:10, 3:4]
iris3[3, ] <- NA
xyTable(iris3)


## Case 4: both x and y are NA for >1 case - no good
## (records with both NA are not aggregated)
iris4 <- iris[1:10, 3:4]
iris4[c(3, 5), ] <- NA
xyTable(iris4)

## Case 5: NA in y when x is duplicated
iris5 <- iris[1:10, 3:4]
iris5[4, 2] <- NA
xyTable(iris5)

## Case 6: NA in y when x is duplicated
iris6 <- iris[1:10, 3:4]
iris6[] <- NA
xyTable(iris6)
