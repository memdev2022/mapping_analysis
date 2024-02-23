rm(list=ls())

library(rstudioapi)
library(dplyr) 
library(stringr)
library(svDialogs)

directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping_analysis')

{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}



# Load necessary packages ----
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(directory, "03_Best-script", "00_Data-Preprocessing", "00_Script_Packages.R"))

# Step 1. Preprocess and binarize the complete data frame ----
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
         file= file.path(preprocessing_path,"01_Preprocessing.R"))
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(preprocessing_path,"02_Binarize.R"))

{script_path = file.path(directory,'03_Best-script','01_Hierarchical-Edge-Bundling')}
  
# Step 2. Create hierarchy and vertices ----
# Use the complete and binarized data frame
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"02_Hierarchy-Vertices.R"))
       


# Step 3. Create plots of all nodes with hierarchy and vertices ----
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"03_Hierarchy-Vertices-Plot.R"))

# Step 4. Create connections ----
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"04_Connection.R"))

# Step 5. Illustrate ----

# Trend version
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"05_Connection-Group.R"))

# Trend version of some entries (filtered by dimensions)
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"05_Connection-GroupFiltered.R"))

# Filtered version, each entry is color-coded
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"05_Connection-Filtered.R"))
