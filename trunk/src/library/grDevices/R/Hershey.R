Hershey <-
    list(typeface =
         c("serif", "sans serif", "script",
           "gothic english", "gothic german", "gothic italian",
           "serif symbol", "sans serif symbol"),
         fontindex =
         c("plain", "italic", "bold", "bold italic",
           "cyrillic", "oblique cyrillic", "EUC"),
## List of valid combinations : ../man/Hershey.Rd
## *checking* of allowed combinations is done in
## (via max{#}) in    FixupVFont() ../../../main/plot.c
## The basic "table" really is in  ../../../modules/vfonts/g_fontdb.c

         allowed = rbind(cbind(1, 1:8), cbind(2, 1:5), cbind(3,1:4),
                         cbind(4:6, 1), cbind(7, 1:5), cbind(8,1:3))
         )
