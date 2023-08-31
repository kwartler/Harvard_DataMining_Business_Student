library(readr)
library(stringr)

# Function to recursively get all file paths of .r files
get_all_r_Paths <- function(path) {
  r_files <- list.files(path, pattern = "*.R$", full.names = TRUE, recursive = TRUE)
  return(r_files)
}
get_all_r_Files <- function(path) {
  r_files <- list.files(path, pattern = "*.R$", full.names = F, recursive = TRUE)
  return(r_files)
}


# Get all file paths
path <- "~/Desktop/HES llmaid conversion/Lessons/"  # place your directory here
rPaths <- get_all_r_Paths(path)
rFiles <- get_all_r_Files(path)

# Directory to save txt files
new_path <- "~/Desktop/HES llmaid conversion/convertedFiles/" # place your output directory here

# Loop through all .r files
for (i  in 1:length(all_r_files)) {
  # Read file
  content <- read_lines(rPaths[i])
  fileName <- rFiles[i]
  
  # 
  # Create new file structure preserving original folder structure
  relative_path <- str_remove(rPaths[i], paste0("^", path))
  dir_name <- dirname(relative_path)
  
  strsplit(new_path,'/')
  strsplit(path,'/')
  
  
  
  
  
  
  
  # Create new file structure preserving original folder structure
  relative_path <- str_remove(rPaths[i], paste0("^", path))
  dir_name <- dirname(relative_path)
  file_name <- head(unlist(strsplit(relative_path, '[.]')), 1)
  file_name <- paste0(file_name, '.txt')
  
  
  # Full output path
  full_dir_path <- file.path(new_path, dir_name)
  
  # Create directory if not exists
  if (!dir.exists(full_dir_path)) {
    dir.create(full_dir_path, recursive = TRUE)
  }
  
  txt_file_path <- file.path(full_dir_path, file_name)
  
  # Write to txt file
  write_lines(content, txt_file_path)
}






# Loop through all .r files
for (i  in 1:length(all_r_files)) {
  print(i)
  # Read file
  content <- read_lines(all_r_files[i])

  # Create new file path
  file_name <- basename(all_r_files[i])
  file_name <- head(unlist(strsplit(file_name, '[.]')), 1)
  file_name <- paste0(file_name, '.txt')
  txt_file_path <- file.path(new_path, file_name)

  # Write to txt file
  write_lines(content, txt_file_path)
}
