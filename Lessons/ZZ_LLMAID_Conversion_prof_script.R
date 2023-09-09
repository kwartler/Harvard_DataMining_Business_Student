library(readr)
library(stringr)

Sys.setenv(OPENAI_KEY = "sk-XXXXXX")

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
path <- "~/Desktop/Harvard_DataMining_Business_Student 2/Lessons/"  # place your directory here
rPaths <- get_all_r_Paths(path)
rFiles <- get_all_r_Files(path)

# Directory to save txt files
new_path <- "~/Desktop/Harvard_DataMining_Business_Student 2/Lessons/Lessons_TXT/" # place your output directory here

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
  fileTmp <- unlist((strsplit(fileName, '/')))[3]
  
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
                 paste(unlist(gptDescriptions), collapse = '\n'))
  finalInfo <- capture.output(cat(finalInfo))
  
  # Save
  nam <- tail(unlist(strsplit(allCSVPaths[i],'/')),1)
  nam <- paste0(nam, '_metaInformation.txt')
  full_file_path <- file.path(new_path, nam)
  
  
  
  # Write to txt file
  write_lines(tmp, full_file_path)
  }
  
  



# End
