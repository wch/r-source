Filters <-
structure(c("R or S files (*.R,*.q)",
            "Enhanced metafiles (*.emf)",
			"Postscript files (*.ps)",
			"PDF files (*.pdf)",
			"Png files (*.png)",
			"Windows bitmap files (*.bmp)",
			"Jpeg files (*.jpeg,*.jpg)",
			"Text files (*.txt)",
			"R images (*.RData,*.rda)",
			"Zip files (*.zip)",
			"All files (*.*)",
			"*.R;*.q", "*.emf",
			"*.ps", "*.pdf", "*.png",
			"*.bmp", "*.jpeg;*.jpg", "*.txt",
			"*.RData;*.rda", "*.zip", "*.*"),
       .Dim = c(11, 2),
       .Dimnames = list(c("R", "emf",
       					"ps","pdf", "png",
       					"bmp", "jpeg", "txt",
       					"RData", "zip", "All"), NULL))

choose.files <- function(default = '', caption = 'Select files', multi = TRUE,
                         filters=Filters, index = nrow(Filters) ) {
	.Internal(chooseFiles(default, caption, multi, filters, index))
}

choose.dir <- function(default = '', caption = 'Select folder')
    .Internal(chooseDir(default, caption))
