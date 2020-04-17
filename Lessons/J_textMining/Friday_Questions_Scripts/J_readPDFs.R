#' Title: Read PDF
#' Purpose: Read PDFs
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: Jan-16-2020
#'

library(pdftools)

# From library docs; single file
pdf_file <- file.path(R.home("doc"), "NEWS.pdf")
text <- pdf_text(pdf_file)

# Pretend we have three docs
pdfFiles  <- rep(pdf_file, 3)
threeDocs <- vector()
for (i in 1:length(pdfFiles)){
  print(paste('reading pdf', i))
  x <- pdf_text(pdfFiles[i])
  x <- paste(x, collapse = ' ')
  threeDocs[i] <- x
}
threeDocs[1]

finalDocs <- data.frame(doc_id = pdfFiles,
                        text   = threeDocs)
write.csv(finalDocs, 'someName.csv', row.names = F)
# End