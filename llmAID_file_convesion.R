recursiveCopying <- function(source, dest) {
  # Identify items in the folder
  items <- list.files(source, full.names = TRUE)
  
  # Separate folders and files
  dirs <- items[file.info(items)$isdir]
  files <- items[!file.info(items)$isdir]
  
  # Process '.R' files
  r_files <- files[grep("\\.R$", files)]
  sapply(r_files, function(file) {
    # Read contents of the R file
    contents <- readLines(file)
    
    # Substitute '.R' extension with '.txt'
    txt_file <- gsub("\\.R$", ".txt", basename(file))
    
    # Generate a full path for the destination TXT file
    new_dest_file <- file.path(dest, txt_file)
    
    # Write the contents to the TXT file
    writeLines(contents, new_dest_file)
  })
  
  # Process '.pptx' and '.docx' files using LibreOffice
  office_files <- files[grep("\\.(pptx|docx)$", files)]
  sapply(office_files, function(file) {
    # Substitute file extension with '.pdf'
    pdf_file <- sub("\\.pptx$|\\.docx$", ".pdf", basename(file))
    
    # Generate a full path for the destination PDF file
    new_dest_file <- file.path(dest, pdf_file)
    
    # Use LibreOffice to convert to PDF
    system(paste("soffice --headless --convert-to pdf ", 
                 shQuote(file), 
                 "--outdir", 
                 shQuote(dirname(new_dest_file))))
  })
  
  # Process '.pdf' files (copy to the destination directory)
  pdf_files <- files[grep("\\.pdf$", items)]
  sapply(pdf_files, function(file) {
    # Generate a destination file path
    dest_file <- file.path(dest, basename(file))
    
    # Create the destination directory if it does not exist
    dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
    
    # Copy the file
    file.copy(file, dest_file)
  })
  
  # Process '.csv' files (prepare description and save as txt)
  csv_files <- files[grep("\\.csv$", items)]
  sapply(csv_files, function(file) {
    csv_DF <- read.csv(file)
    
    # Get CSV data description
    csv_desc <- sprintf(
      "CSV File Name: %s\nNumber of rows: %d\nNumber of columns: %d\nColumn Names: %s", 
      basename(file),
      nrow(csv_DF),
      ncol(csv_DF),
      paste(colnames(csv_DF), collapse = ", ")
    )
    
    # Generate new destination file path with .txt extension
    new_dest_file <- file.path(dest, paste0(basename(file), "_description.txt"))
    
    # Write data description to TXT file
    writeLines(csv_desc, new_dest_file)
  })
  
  # Process subfolders to maintain the same structure
  sapply(dirs, function(x) {
    # Generate new destination folder
    new_dest <- file.path(dest, basename(x))
    
    # Create new destination folder
    dir.create(new_dest)
    
    # Call function recursively for subfolder
    recursiveCopying(x, new_dest)
  })
}

createFolderStructureAndConvertFiles <- function(source_dir, dest_dir) {
  # Check if destination directory is not existing
  if (!dir.exists(dest_dir)) {
    # Create destination directory
    dir.create(dest_dir)
  }
  
  # Call recursive function
  recursiveCopying(source_dir, dest_dir)
}

# Now you can call the function as
sourcePth <- '~/Desktop/Harvard_DataMining_Business_Student/'
dest <- '~/Desktop/autoFileConversion/'
createFolderStructureAndConvertFiles(sourcePth, dest)
