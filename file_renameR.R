typeline <- function(msg = "Enter some text: ") {
  if (interactive() ) {
    txt <- readline(msg)
  } else {
    cat(msg);
    txt <- readLines("",n=1);
  }
  return(txt)
}


# RENAME FILES ------------------------------------------------------------
cat("------ RENAME FILE ------\n\n")
cat("Loading packages..\n")

library(tidyverse)
project_directory <- getwd()

while (TRUE){
  folder_path <- typeline("Insert doc folder path: ")
  setwd(folder_path)
  cat("Setted wd: ", folder_path, "\n")
  
  recoursive  <- typeline("Do you want contents of folders? (TRUE, FLASE)")
  recoursive <- switch(toupper(recoursive),
                       "T" = TRUE,
                       "TRUE" = TRUE,
                       "F" = FALSE,
                       "FALSE" = FALSE,
                       FALSE)
  
  # Creating file list ------------------------------------------------------
  old_file_name <- list.files(".",
                              recursive = recoursive,
                              full.names = FALSE) %>%
    data.frame(old_path = .) %>%
    mutate(id = rownames(.)) %>%
    select(old_path, id)
  write_csv(old_file_name, "_RENAME_FILE.txt")
  
  
  # User interaction --------------------------------------------------------
  print("Please, modify the files names")
  typeline("Press any key to continue..")
  
  
  # Renaming files ----------------------------------------------------------
  new_file_name <- read_csv("_RENAME_FILE.txt") %>%
    mutate(id = as.character(id),
           new_path = old_path) %>%
    select(id, new_path)
  
  
  file_mapping <- old_file_name %>%
    left_join(new_file_name) %>%
    mutate(success = FALSE) %>%
    filter(old_path != new_path)
  
  print("List of file to be modified")
  print(file_mapping)
  typeline("Press any key to continue..")
  
  file_mapping$success <- file.rename(file_mapping$old_path, file_mapping$new_path)
  
  print("Done")

}









