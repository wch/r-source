postscript(file = "reg-plot-latin1.ps",
           encoding = "ISOLatin1",
           width = 7, height = 7, paper = "a4")
library(graphics) # to be sure
example(text)     # has examples that need to he plotted in latin-1
q("no")
