rm(list=ls())
# This script looks for Journals

directory <-"D:/litreview_visualization/"
if (!exists("directory")) {directory <- readline("Enter directory (D:/litreview_visualization/) >>> ")}


{
  data_path = paste0(directory,'00_Data xlsx/')
  funcon_path = paste0(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = paste0(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = paste0(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = paste0(directory,'04_Results/')
  descriptives_path = paste0(directory,'05_Descriptives/')
}

{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(reshape2)
  library(writexl)
  library(stringr)
}

# Set up directory and paths
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
directory = getwd() %>% str_extract("[^litreview_visualization]+") %>% file.path("/litreview_visualization/")  

# This script produces HEB with connections representing the trend of multiple entries
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}
# No direct information about task type
# rearrange task entries in Lit_Review_binarized according to the order in the dfRaw
df <- read_xlsx(sprintf('%sLit_Review_QC.xlsx', data_path)) %>%# choose: Lit_Review_binarized_constrained.xlsx  
  distinct(Article_ID, .keep_all = T)

df$Journal %>% unique() %>% sort()

a <- df %>%
  group_by(Journal)%>%
  summarise(Paper = n())%>%
  ungroup() %>%
  mutate(sumPap = sum(Paper)) %>%
  mutate(Proportion = Paper/ sumPap) %>%
  arrange(desc(Paper), Journal)


write_xlsx(a, paste0(descriptives_path,'JournalDescriptives.xlsx'))
