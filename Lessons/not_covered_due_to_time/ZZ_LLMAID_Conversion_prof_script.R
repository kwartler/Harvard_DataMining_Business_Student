library(readr)
library(stringr)
library(officer)
library(rvg)

#Sys.setenv(OPENAI_KEY = "sk-XXXXXX")

# Function to recursively get all file paths of .r files
get_all_r_Paths <- function(path) {
  r_files <- list.files(path, pattern = "*.R$", full.names = TRUE, recursive = TRUE)
  return(r_files)
}
get_all_r_Files <- function(path) {
  r_files <- list.files(path, pattern = "*.R$", full.names = F, recursive = TRUE)
  return(r_files)
}

# Function to add more information from GPT helping with embeddings
anyPrompt <- function(userPrompt, 
                      apiKey = Sys.getenv("OPENAI_KEY"),
                      temperature=0, 
                      max_tokens=256, 
                      top_p=1,
                      frequency_penalty=0, 
                      presence_penalty=0){
  require(httr)
  require(jsonlite)
  
  # Construct the prompting
  # You could improve this base prompting system and response example significantly to improve your results
  promptList <- list(list(role='system',content = 'You are a helpful assistant.'),
                     list(role='user', content ='Who won the world series in 2020?'),
                     list(role='assistant', content = 'The Los Angeles Dodgers won the World Series in 2020.'),
                     list(role='user',content=userPrompt))
  
  # Put together the API arguments
  args <- list(messages=promptList,
               model = 'gpt-3.5-turbo',
               temperature=temperature, 
               max_tokens=max_tokens, 
               top_p=top_p,
               frequency_penalty=frequency_penalty, 
               presence_penalty=presence_penalty) 
  
  # Post the arguments and data to the openAI service
  req <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = toJSON(args, auto_unbox=TRUE),
    add_headers (
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", apiKey)))
  
  statusChk <- status_code(req)
  if(statusChk==200){
    # Obtain just the information returned
    response <- capture.output(cat(fromJSON(content(req, as = "text", encoding = "UTF-8"))$choices$message$content))
    response <- response[nchar(response)>0]
    
  } else {
    statusResp <- paste0('The POST request failed with status: ',statusChk)
    response <- statusResp
  }
  return(response)
}


# Get all file paths
path <- "~/Desktop/Harvard_DataMining_Business_Student/Lessons/"  # place your directory here
rPaths <- get_all_r_Paths(path)
rFiles <- get_all_r_Files(path)

# Directory to save txt files
new_path <- "~/Desktop/Sept11_LLM" # place your output directory here

# Loop through all .r files
for (i  in 1:length(rFiles)) {
  print(i)
  # Read file
  content <- read_lines(rPaths[i])
  fileName <- rFiles[i]
  
  # Get a description of the script
  filePrep <- c('please summarize this R script from a computer science class to improve embeddings for informational retrieval in a vector database:
                  ```', content,'\n```')
  filePrep <- paste(filePrep, collapse = '\n')
  gptDescription <- anyPrompt(filePrep, max_tokens = 256*2, apiKey = Sys.getenv("OPENAI_KEY"))
  gptDescription <- paste(gptDescription, collapse = '\n')
  
  # Combine original and GPT description
  tmp <- c(gptDescription, paste(content, collapse = '\n'))
  
  # Get new file name
  lesson   <- unlist((strsplit(fileName, '/')))[1]
  fileTmp <- tail(unlist((strsplit(fileName, '/'))),1)
  
  # Append
  metaLesson <- paste('class lesson:', lesson)
  metaFileName <- paste('original file name:', lesson)
  metInfo <- paste(c(metaLesson, metaFileName),collapse = '\n')
  tmp <- paste(c(metInfo,
           tmp), 
           collapse = '\n')
  
  
  # Full output path
  full_file_path <- file.path(new_path, paste0(unlist((strsplit(fileName, '/')))[3], '.txt'))

  
  
  # Write to txt file
  write_lines(tmp, full_file_path)
}

## Now do it for a portion of the CSV files
get_all_CSV_Files <- function(path) {
  r_files <- list.files(path, pattern = "*.csv$", full.names = F, recursive = TRUE)
  return(r_files)
}
# Function to recursively get all file paths of .r files
get_all_CSV_Paths <- function(path) {
  r_files <- list.files(path, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)
  return(r_files)
}
allCSVPaths <- get_all_CSV_Paths(path)
allCSV <- get_all_CSV_Files(path)

for(i in 1:length(allCSV)){
  print(i)
  oneCSV <- read.csv(allCSVPaths[i])
  sumTable <- paste(capture.output(summary(oneCSV)), collapse = '\n')
  idxNROW <- nrow(oneCSV)
  oneCSV <- oneCSV[sample(1:nrow(oneCSV), 6),]
  
  # Organize
  ### Once files are set, later add the URL to this ###
  csvMETA <- c('data set file name:\n', paste(allCSV[i], collapse = '\n'),
    paste('data set number of rows:\n', idxNROW, '\n'),
              paste('data set number of columns:\n', ncol(oneCSV), '\n'),
              paste('data set column names:\n', paste(names(oneCSV), collapse = '\n')),
              paste('data set summary information:\n',sumTable, collapse = '\n'),
              paste('data set sample information:\n',paste(capture.output(oneCSV), collapse = '\n')))
  
  # Collapse it all
  csvMETA <-paste(csvMETA, collapse = '\n\n')
  
  # Chunk size and overlap
  chunkSize <- 4000
  overlap <- 200
  
  # Calculate the number of chunks
  numChunks <- ceiling(nchar(csvMETA) / (chunkSize - overlap))
  
  # Get each chunk
  chunks <- list()
  # Loop through the input string and create chunks
  for (j in 1:numChunks) {
    start <- 1 + (j - 1) * (chunkSize - overlap)
    end <- min(start + chunkSize - 1, nchar(csvMETA))
    chunk <- substr(csvMETA, start, end)
    chunks[[j]] <- chunk
  }
  
  gptDescriptions <- list()
  for(j in 1:length(chunks)){
    print(j)
    oneSection <- chunks[[j]]
    gptMeta <- anyPrompt(userPrompt = paste('Please summarize this information about a CSV file containing data used in a computer science class lesson:',oneSection, collapse = '\n'))
    gptDescriptions[[j]] <- gptMeta
  }
  
  # Organize
  finalInfo <- c(csvMETA, 
                 paste('Data set summary information and description:\n',unlist(gptDescriptions), collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  # Drop values with zero character length
  finalInfo <- finalInfo[nchar(finalInfo) > 0]
  
  # Save
  nam <- tail(unlist(strsplit(allCSVPaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  
  
  # Write to txt file
  write_lines(finalInfo, full_file_path)
}

# Now do it for powerPoints both meta and pdf save
get_all_PPTX_Files <- function(path) {
  r_files <- list.files(path, pattern = "*.pptx$", full.names = F, recursive = TRUE)
  return(r_files)
}
# Function to recursively get all file paths of .r files
get_all_PPTX_Paths <- function(path) {
  r_files <- list.files(path, pattern = "*.pptx$", full.names = TRUE, recursive = TRUE)
  return(r_files)
}
  
allPPTXpaths <- get_all_PPTX_Paths(path)
allPPTX <- get_all_PPTX_Files(path)

for(i in 1:length(allPPTX)){
  print(paste('meta',i))
  doc <- read_pptx(allPPTXpaths[i])
  content <- pptx_summary(doc)
  pptxTXT <- content$text
  pptxTXT <- pptxTXT[complete.cases(pptxTXT)]
  pptxTXT <- paste(pptxTXT, collapse = '\n')
  # Collapse it all
  pptxTXT <- paste('The following text is extracted from a computer science lesson powerpoint slide deck. The slide deck is called:\n',
                  allPPTX[i],'\n', pptxTXT,collapse = '\n\n')
  
  # Chunk size and overlap
  chunkSize <- 4000
  overlap <- 200
  
  # Calculate the number of chunks
  numChunks <- ceiling(nchar(pptxTXT) / (chunkSize - overlap))
  
  # Get each chunk
  chunks <- list()
  # Loop through the input string and create chunks
  for (j in 1:numChunks) {
    start <- 1 + (j - 1) * (chunkSize - overlap)
    end <- min(start + chunkSize - 1, nchar(pptxTXT))
    chunk <- substr(pptxTXT, start, end)
    chunks[[j]] <- chunk
  }
  
  # get descriptions
  gptDescriptions <- list()
  for(j in 1:length(chunks)){
    print(j)
    oneSection <- chunks[[j]]
    gptMeta <- anyPrompt(userPrompt = paste('Please summarize this information from text obtained from a powerpoint presentation slides in a computer science class lesson:',oneSection, collapse = '\n'))
    gptDescriptions[[j]] <- gptMeta
  }
  gptDescriptions <- unlist(gptDescriptions)
  gptDescriptions <- paste(gptDescriptions, collapse = '\n')
  
  # Organize
  finalInfo <- c(paste('Lesson Powerpoint name:\n',allPPTX[i], '\n' ,collapse = '\n'), 
                 paste('Lesson Powerpoint description and summary:\n',gptDescriptions, collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  # Drop values with zero character length
  finalInfo <- finalInfo[nchar(finalInfo) > 0]
  
  # Save
  nam <- tail(unlist(strsplit(allPPTXpaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  # Write to txt file
  write_lines(finalInfo, full_file_path)
  
  # Now save a copy as a pdf using libreoffice
  # Terminal: brew install libreoffice
  # The command to convert
  cleanFullPth <- allPPTXpaths[i]
  cleanNewPth <- gsub('~','/Users/edwardkwartler',new_path)
  #cleanNewPth <-  "/Users/edwardkwartler/Desktop/Harvard_DataMining_Business_Student 2/Lessons/Lessons_TXT/"
  
  #cleanNewPth <- new_path
  sysCommand <- paste0('soffice --headless --convert-to pdf "',
                       cleanFullPth, '" --outdir "',
                       cleanNewPth, '"')
  
  system(sysCommand)
  
}

# Now do the book CSV data sets
#~/Desktop/Harvard_DataMining_Business_Student/BookDataSets
bookCSV <- '~/Desktop/Harvard_DataMining_Business_Student/BookDataSets'
allCSVPaths <- get_all_CSV_Paths(bookCSV)
allCSV <- get_all_CSV_Files(bookCSV)

for(i in 1:length(allCSV)){
  print(allCSVPaths[i])
  oneCSV <- read.csv(allCSVPaths[i])
  sumTable <- paste(capture.output(summary(oneCSV)), collapse = '\n')
  idxNROW <- nrow(oneCSV)
  oneCSV <- oneCSV[sample(1:nrow(oneCSV), 6),]
  
  # Organize
  ### Once files are set, later add the URL to this ###
  csvMETA <- c('data set file name:\n', paste(allCSV[i], collapse = '\n'),
               paste('data set number of rows:\n', idxNROW, '\n'),
               paste('data set number of columns:\n', ncol(oneCSV), '\n'),
               paste('data set column names:\n', paste(names(oneCSV), collapse = '\n')),
               paste('data set summary information:\n',sumTable, collapse = '\n'),
               paste('data set sample information:\n',paste(capture.output(oneCSV), collapse = '\n')))
  
  # Collapse it all
  csvMETA <-paste(csvMETA, collapse = '\n\n')
  
  # Chunk size and overlap
  chunkSize <- 4000
  overlap <- 200
  
  # Calculate the number of chunks
  numChunks <- ceiling(nchar(csvMETA) / (chunkSize - overlap))
  
  # Get each chunk
  chunks <- list()
  # Loop through the input string and create chunks
  for (j in 1:numChunks) {
    start <- 1 + (j - 1) * (chunkSize - overlap)
    end <- min(start + chunkSize - 1, nchar(csvMETA))
    chunk <- substr(csvMETA, start, end)
    chunks[[j]] <- chunk
  }
  
  gptDescriptions <- list()
  for(j in 1:length(chunks)){
    print(j)
    oneSection <- chunks[[j]]
    gptMeta <- anyPrompt(userPrompt = paste('Please summarize this information about a CSV file containing data used in a computer science class lesson:',oneSection, collapse = '\n'))
    gptDescriptions[[j]] <- gptMeta
  }
  
  # Organize
  finalInfo <- c(csvMETA, 
                 paste('Data set summary information and description:\n',unlist(gptDescriptions), collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  # Drop values with zero character length
  finalInfo <- finalInfo[nchar(finalInfo) > 0]
  
  # Save
  nam <- tail(unlist(strsplit(allCSVPaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  
  
  # Write to txt file
  write_lines(finalInfo, full_file_path)
}


# Now do the case information
# ~/Desktop/Harvard_DataMining_Business_Student/Cases
casePath <- '~/Desktop/Harvard_DataMining_Business_Student/Cases'
get_all_DOCX_Files <- function(path) {
  r_files <- list.files(path, pattern = "*.docx$", full.names = F, recursive = TRUE)
  return(r_files)
}
# Function to recursively get all file paths of .r files
get_all_DOCX_Paths <- function(path) {
  r_files <- list.files(path, pattern = "*.docx$", full.names = TRUE, recursive = TRUE)
  return(r_files)
}

allDOCXpaths <- get_all_DOCX_Paths(casePath)
allDOCX <- get_all_DOCX_Files(casePath)

for(i in 1:length(allDOCX)){
  print(paste('meta',i))
  doc <- read_docx(allDOCXpaths[i])
  content <- docx_summary(doc)
  docxTXT <- content$text
  docxTXT <- docxTXT[complete.cases(docxTXT)]
  docxTXT <- paste(docxTXT, collapse = '\n')
  # Collapse it all
  docxTXT <- paste('The following text is extracted from a computer science lesson docx slide deck. The slide deck is called:\n',
                   allDOCX[i],'\n', docxTXT,collapse = '\n\n')
  
  # Chunk size and overlap
  chunkSize <- 4000
  overlap <- 200
  
  # Calculate the number of chunks
  numChunks <- ceiling(nchar(docxTXT) / (chunkSize - overlap))
  
  # Get each chunk
  chunks <- list()
  # Loop through the input string and create chunks
  for (j in 1:numChunks) {
    start <- 1 + (j - 1) * (chunkSize - overlap)
    end <- min(start + chunkSize - 1, nchar(docxTXT))
    chunk <- substr(docxTXT, start, end)
    chunks[[j]] <- chunk
  }
  
  # get descriptions
  gptDescriptions <- list()
  for(j in 1:length(chunks)){
    print(j)
    oneSection <- chunks[[j]]
    gptMeta <- anyPrompt(userPrompt = paste('Please summarize this information from text obtained from a docx presentation slides in a computer science class lesson:',oneSection, collapse = '\n'))
    gptDescriptions[[j]] <- gptMeta
  }
  gptDescriptions <- unlist(gptDescriptions)
  gptDescriptions <- paste(gptDescriptions, collapse = '\n')
  
  # Organize
  finalInfo <- c(paste('Lesson docx name:\n',allPPTX[i], '\n' ,collapse = '\n'), 
                 paste('Lesson docx description and summary:\n',gptDescriptions, collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  # Drop values with zero character length
  finalInfo <- finalInfo[nchar(finalInfo) > 0]
  
  # Save
  nam <- tail(unlist(strsplit(allDOCXpaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  # Write to txt file
  write_lines(finalInfo, full_file_path)
  
  # Now save a copy as a pdf using libreoffice
  # Terminal: brew install libreoffice
  # The command to convert
  cleanFullPth <- allDOCXpaths[i]
  cleanNewPth <- gsub('~','/Users/edwardkwartler',new_path)
  #cleanNewPth <-  "/Users/edwardkwartler/Desktop/Harvard_DataMining_Business_Student 2/Lessons/Lessons_TXT/"
  
  #cleanNewPth <- new_path
  sysCommand <- paste0('soffice --headless --convert-to pdf "',
                       cleanFullPth, '" --outdir "',
                       cleanNewPth, '"')
  
  system(sysCommand)
  
}

# CSV in cases
allCSVPaths <- get_all_CSV_Paths(casePath)
allCSV <- get_all_CSV_Files(casePath)

for(i in 1:length(allCSV)){
  print(i)
  oneCSV <- read.csv(allCSVPaths[i])
  sumTable <- paste(capture.output(summary(oneCSV)), collapse = '\n')
  idxNROW <- nrow(oneCSV)
  oneCSV <- oneCSV[sample(1:nrow(oneCSV), 6),]
  
  # Organize
  ### Once files are set, later add the URL to this ###
  csvMETA <- c('data set file name:\n', paste(allCSV[i], collapse = '\n'),
               paste('data set number of rows:\n', idxNROW, '\n'),
               paste('data set number of columns:\n', ncol(oneCSV), '\n'),
               paste('data set column names:\n', paste(names(oneCSV), collapse = '\n')),
               paste('data set summary information:\n',sumTable, collapse = '\n'),
               paste('data set sample information:\n',paste(capture.output(oneCSV), collapse = '\n')))
  
  # Collapse it all
  csvMETA <-paste(csvMETA, collapse = '\n\n')
  
  # Chunk size and overlap
  chunkSize <- 4000
  overlap <- 200
  
  # Calculate the number of chunks
  numChunks <- ceiling(nchar(csvMETA) / (chunkSize - overlap))
  
  # Get each chunk
  chunks <- list()
  # Loop through the input string and create chunks
  for (j in 1:numChunks) {
    start <- 1 + (j - 1) * (chunkSize - overlap)
    end <- min(start + chunkSize - 1, nchar(csvMETA))
    chunk <- substr(csvMETA, start, end)
    chunks[[j]] <- chunk
  }
  
  gptDescriptions <- list()
  for(j in 1:length(chunks)){
    print(j)
    oneSection <- chunks[[j]]
    gptMeta <- anyPrompt(userPrompt = paste('Please summarize this information about a CSV file containing data used in a computer science class case study assignment:',oneSection, collapse = '\n'))
    gptDescriptions[[j]] <- gptMeta
  }
  
  # Organize
  finalInfo <- c(csvMETA, 
                 paste('Data set summary information and description:\n',unlist(gptDescriptions), collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  # Drop values with zero character length
  finalInfo <- finalInfo[nchar(finalInfo) > 0]
  
  # Save
  nam <- tail(unlist(strsplit(allCSVPaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  
  
  # Write to txt file
  write_lines(finalInfo, full_file_path)
}

# Possible R scripts in the Cases
rFiles <- get_all_r_Files(casePath)
rPaths <- get_all_r_Paths(casePath)

# Loop through all .r files
for (i  in 1:length(rFiles)) {
  print(i)
  # Read file
  content <- read_lines(rPaths[i])
  fileName <- rFiles[i]
  
  # Get a description of the script
  filePrep <- c('please summarize this R script from a computer science case study assignment to improve embeddings for informational retrieval in a vector database:
                  ```', content,'\n```')
  filePrep <- paste(filePrep, collapse = '\n')
  gptDescription <- anyPrompt(filePrep, max_tokens = 256*2, apiKey = Sys.getenv("OPENAI_KEY"))
  gptDescription <- paste(gptDescription, collapse = '\n')
  
  # Combine original and GPT description
  tmp <- c(gptDescription, paste(content, collapse = '\n'))
  
  # Get new file name
  lesson   <- unlist((strsplit(fileName, '/')))[1]
  fileTmp <- tail(unlist((strsplit(fileName, '/'))),1)
  
  # Append
  metaLesson <- paste('case study:', lesson)
  metaFileName <- paste('original file name:', lesson)
  metInfo <- paste(c(metaLesson, metaFileName),collapse = '\n')
  tmp <- paste(c(metInfo,
                 tmp), 
               collapse = '\n')
  
  
  # Full output path
  full_file_path <- file.path(new_path, paste0(tail(unlist((strsplit(fileName, '/'))),1), '.txt'))
  
  
  
  # Write to txt file
  write_lines(tmp, full_file_path)
}

# Now do the ethics articles
# ~/Desktop/Harvard_DataMining_Business_Student/EthicsArticles

# Now do the homework
# ~/Desktop/Harvard_DataMining_Business_Student/HW

# Now do admin
# Syllabus, school information/policy


# End
