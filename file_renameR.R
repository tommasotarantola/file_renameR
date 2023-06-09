typeline <- function(msg = "Enter some text: ") {
  if (interactive() ) {
    txt <- readline(msg)
  } else {
    cat(msg);
    txt <- readLines("stdin",n=1);
  }
  return(txt)
}

cst_quote <- function(x) {
  ifelse(is.character(x), "\"", "")
}

# RENAME FILES ------------------------------------------------------------
cat("------ RENAME FILE ------\n\n")
cat("Loading packages..\n")

library(tidyverse)
project_directory <- getwd()

while (TRUE){
  
  folder_path <- typeline("\nInsert doc folder path: ")
  cat(">ACK\n")
  setwd(folder_path)
  cat("Setted wd: ", folder_path, "\n")
  
  recoursive  <- typeline("\nDo you want contents of folders? (TRUE, FLASE)\n")
  cat(">ACK\n")
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
    mutate(old_path = paste0("\"", old_path, "\"")) %>%
    select(old_path, id)
  write_csv(old_file_name, "_RENAME_FILE.txt", quote_escape = "double")
  
  
  
  # User interaction --------------------------------------------------------
  cat("\nPlease, modify the files names")
  typeline("\nAfter, press enter to continue..\n")
  cat(">ACK\n")
  
  
  # Renaming files ----------------------------------------------------------
  new_file_name <- read_csv("_RENAME_FILE.txt") %>%
    mutate(id = as.character(id),
           new_path = old_path) %>%
    select(id, new_path)
  
  
  file_mapping <- old_file_name %>%
    left_join(new_file_name) %>%
    mutate(success = FALSE) %>%
    filter(old_path != new_path)
  
  cat("\nList of file to be modified:\n\n")
  print(select(file_mapping, old_path, new_path))
  typeline("\nPress enter to confirm and rename")
  cat(">ACK\n")
  
  file_mapping$success <- file.rename(str_replace_all(file_mapping$old_path, "\"", ""),
                                      str_replace_all(file_mapping$new_path, "\"", ""))
  
  str_replace_all(file_mapping$old_path, "\"", "")
  cat("Done\n\n")
  Sys.sleep(1)
  
}







