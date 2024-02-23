rm(list=ls())
# This script binarizes a complex but already preprocessed dataset
  directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping analysis')
  
  {
    data_path = file.path(directory,'00_Data xlsx/')
    funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
    result_path = file.path(directory,'04_Results/')
    descriptives_path = file.path(directory,'05_Descriptives/')
  }
  
  
  # Load packages ----
  {
    library(dplyr) #step1
    library(tidyverse) #step1
    library(readxl) #step1
    library(fastDummies) #step1
    library(reshape2) #step1
    library(stringr) #step1
    library(writexl) #step1
    
    library(ggraph) #step3
    library(igraph) #step3
    library(plotly) #step3
    library(RColorBrewer) #step5
    library(scales)
  }
  
  {source(file.path(funcon_path, "PN_BinarizeDataset.R"))}
  
  # Read data set
  reviewtable <- read_excel(file.path(data_path,"Lit_Review_Preprocessed.xlsx"))
  identifier_var <- c("Entry_ID", "Authors","Title", "Year")
  
  # Select variables of interest, rename
  reviewtable <- reviewtable %>%
    select(all_of(identifier_var),
           starts_with("Age"), starts_with("Design"), starts_with("Eye_tracking"), starts_with("Neuroanatomical_imaging"), starts_with("Neurofunctional_imaging"),  
           starts_with("Task_type"), starts_with("To-be-remembered_information"), starts_with("Encoding_instruction"), starts_with("Repetition_of_encoding_trials_"), starts_with("Retrieval_mode_"), 
           starts_with("Similarity_manipulation_at_encoding_"), starts_with("Similarity_manipulation_at_retrieval_"), 
           starts_with("Cue_manipulation_"), starts_with("Stimulus_"), starts_with("Stimulus_modality_"))
  
  
  #....................................................................................................
  # Variable                              | Data type                                | Transform to    
  # ...................................................................................................
  # Entry_ID                              | character                                |.                
  # Age                                   | numerical, binary                        |binary           
  # Design                                | numerical, non-binary categorical        |binary           
  # Eye tracking                          | numerical, binary                        |binary           
  # Neuroanatomical imaging               | numerical, non-binary categorical        |binary           
  # Neurofunctional imaging               | numerical, non-binary categorical        |binary            
  # Task_type                             | numerical, non-binary categorical        |binary           
  # To-be-remembered-info                 | numerical, non-binary categorical        |binary           
  # Encoding_instruction                  | numerical, non-binary categorical        |binary           
  # Repetition_of_encoding                | numerical, non-binary categorical        |binary           
  # Retrieval_mode                        | numerical, non-binary categorical        |binary           
  # Similiarity_Manipulation_Encoding     | numerical, non-binary categorical        |binary           
  # Similiarity_Manipulation_Retrieval    | numerical, non-binary categorical        |binary           
  # Cue manipulation                      | numerical, non-binary categorical        |binary           
  # Stimuli                               | numerical, non-binary categorical        |binary           
  # Modality                              | numerical, non-binary categorical        |binary           
  #....................................................................................................
  
  df_main <- PN_BinarizeDataset(reviewtable,identifier_var)
  
  # Polish variable , e.g., from Stimuli__1 to Stimuli_1
  non_indentifier <- df_main %>%
    select(-identifier_var)%>%
    names() %>%
    str_replace_all(pattern = "__", replacement = "_") %>%
    str_replace_all(pattern = "_", replacement = " ")
  names(df_main) <- c(identifier_var, non_indentifier)
  
  df_main <- df_main%>%
    select(identifier_var,
           starts_with("Age"),
           starts_with("Design"),
           starts_with('Task'),
           starts_with('To-be'),
           starts_with('Encoding'),
           starts_with('Repetition of encoding trials'),
           starts_with('Retrieval'),
           starts_with('Similarity manipulation'), #at encoding/retrieval
           starts_with('Cue'),
           starts_with('Stimulus'),
           starts_with('Stimulus modality'),
           starts_with('Eye'),
           starts_with('Neuroanatomical'),
           starts_with('Neurofunctional'))
  
  writexl::write_xlsx(df_main, path = file.path(data_path,"Lit_Review_binarized.xlsx"))
  message('Create ', file.path(data_path,"Lit_Review_binarized.xlsx"))
  
  # test distribution
  # test <- melt(df_main, id= identifier_var)
  # distribution <- test %>% group_by(variable, value) %>% summarise(n())
  # table(test$value)

