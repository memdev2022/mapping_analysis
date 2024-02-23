# This script set the working directory
library(dplyr)
library(stringr)
library(rstudioapi)

if (interactive() ){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  directory = getwd() %>% str_extract("[^litreview_visualization]+") %>% paste0("litreview_visualization/")  
  agree.directory <- readline(sprintf('Folder litreview_visualization is currently in %s (y/n) >> ', directory))
  if (agree.directory == 'n') {
    directory <- readline("Enter directory (D:/litreview_visualization/) >>> ")
    rm(agree.directory)
  }
}