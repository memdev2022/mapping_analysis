# This script preprocess the complex dataset 

  directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping_analysis')
  
  {
    data_path = file.path(directory,'00_Data xlsx/')
    funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
    result_path = file.path(directory,'04_Results/')
    descriptives_path = file.path(directory,'05_Descriptives/')
  }

# Load packages
{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(fastDummies)
  library(reshape2)
  library(stringr)
  library(writexl)
}

# Read data set
reviewtable <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx", sep = ""))


# GET DATA =====================================================================

# Retrieve dataset

reviewtable <- reviewtable %>%
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = ", "), .after = Task_number) %>%
  select(-c(Article_ID, Task_number))%>%
  # rename some dimensions
  rename(c(Age_18 = Age_18_and_above,
           Eye_tracking_1 = `Eye tracking`,
           `To-be-remembered_information` = `To-be-remembered info`, 
           Cue_manipulation_1 = `Cue manipulation_1`, Cue_manipulation_2 = `Cue manipulation_2`,
           Similarity_manipulation_at_encoding_1 = `Similarity_Manipulation_Encoding_1`,
           Similarity_manipulation_at_encoding_2 = `Similarity_Manipulation_Encoding_2`,
           Similarity_manipulation_at_retrieval_1 = `Similarity_Manipulation_Retrieval_1`,
           Similarity_manipulation_at_retrieval_2 = `Similarity_Manipulation_Retrieval_2`,
           Stimulus_1 = `_stimuli_1`,
           Stimulus_2 = `_stimuli_2`,
           Stimulus_3 = `_stimuli_3`,
           Stimulus_modality_1 = `_modality_1`,
           Stimulus_modality_2 = `_modality_2`,
           Stimulus_modality_3 = `_modality_3`,
           Neuroanatomical_imaging_1 = `Imaging_structure_1`,
           Neuroanatomical_imaging_2 = `Imaging_structure_2`,
           Neurofunctional_imaging_1 = `Imaging_function_1`,
           Neurofunctional_imaging_2 = `Imaging_function_2`))

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
# Stimulus                               | numerical, non-binary categorical        |binary           
# Stimulus_modality                              | numerical, non-binary categorical        |binary           
# Delay                                 | numerical, not binned                    |binary           
#....................................................................................................


# PREPROCESSING ================================================================

# Recode imaging
reviewtable <- reviewtable %>% 
  # manual recoding of value
  mutate(Neuroanatomical_imaging_1 = recode(Neuroanatomical_imaging_1, `0` = -999),
         Neuroanatomical_imaging_2 = recode(Neuroanatomical_imaging_2, `0` = -999),
         Neurofunctional_imaging_1 = recode(Neurofunctional_imaging_1, `0` = -999),
         Neurofunctional_imaging_2 = recode(Neurofunctional_imaging_2, `0` = -999))

# Recode Similarity_manipulation_Encoding/Retrieval and Cue manipulation----
# idea: if manipulation1 is 1, then manipulation2 is -999.
# else, while manipulation1 is >1, if manipulation2 is 1, than manipulation2 is -999:

reviewtable <- reviewtable %>%
  mutate(Similarity_manipulation_at_encoding_2 = case_when(Similarity_manipulation_at_encoding_1 == 1 ~ -999,
                                                        Similarity_manipulation_at_encoding_1 != 1 & Similarity_manipulation_at_encoding_2 == 1 ~ -999,
                                                        Similarity_manipulation_at_encoding_1 != 1 & Similarity_manipulation_at_encoding_2 != 1 ~ Similarity_manipulation_at_encoding_2),
         Similarity_manipulation_at_retrieval_2 = case_when(Similarity_manipulation_at_retrieval_1 == 1 ~ -999,
                                                         Similarity_manipulation_at_retrieval_1 != 1 & Similarity_manipulation_at_retrieval_2 == 1 ~ -999,
                                                         Similarity_manipulation_at_retrieval_1 != 1 & Similarity_manipulation_at_retrieval_2 != 1 ~ Similarity_manipulation_at_retrieval_2),
         Cue_manipulation_2 = case_when(Cue_manipulation_1 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 != 1 ~ Cue_manipulation_2))



# Recode Repetition_of_encoding_trials ----
# idea: repetition == 1 >> one-shot,
# repetition > 1 >> multishot,
# in percentage (e.g., 80% = 0.8) or elsely defined >> criterion (specified),
# unspecified >> -999

RoET.input <- c(reviewtable$Repetition_of_encoding_trials_1,
                reviewtable$Repetition_of_encoding_trials_2,
                reviewtable$Repetition_of_encoding_trials_3,
                reviewtable$Repetition_of_encoding_trials_4) %>% unique()
RoET.999 <- RoET.input[RoET.input %in% c('unspecified','unknown','-999','-999.0')]
RoET.1 <- RoET.input[!is.na(as.numeric(RoET.input)) & as.numeric(RoET.input)==1] # oneshot
RoET.2 <- RoET.input[!is.na(as.numeric(RoET.input)) & as.numeric(RoET.input)>1] # multishot
RoET.3 <- setdiff(RoET.input, c(RoET.1,RoET.2,RoET.999))

reviewtable <- reviewtable %>% mutate_at(vars(starts_with('Repetition_of_encoding')),
                                         funs(case_when(. %in% RoET.999 ~ '-999',
                                                        # 'criterion'
                                                        . %in% RoET.3 ~ '3',
                                                        #'multishot'
                                                        . %in% RoET.2 ~ '2',
                                                        # 'oneshot'
                                                        . %in% RoET.1 ~ '1') %>% as.numeric()))


# Recode delays ----
reviewtable <- reviewtable %>%
  mutate(Delay1min = str_split_fixed(Delay_1, " - ", 2)[,1], Delay1max = str_split_fixed(Delay_1, " - ", 2)[,2],
         Delay2min = str_split_fixed(Delay_2, " - ", 2)[,1], Delay2max = str_split_fixed(Delay_2, " - ", 2)[,2],
         Delay3min = str_split_fixed(Delay_3, " - ", 2)[,1], Delay3max = str_split_fixed(Delay_3, " - ", 2)[,2],
         Delay4min = str_split_fixed(Delay_4, " - ", 2)[,1], Delay4max = str_split_fixed(Delay_4, " - ", 2)[,2],
         Delay5min = str_split_fixed(Delay_5, " - ", 2)[,1], Delay5max = str_split_fixed(Delay_5, " - ", 2)[,2],
         Delay6min = str_split_fixed(Delay_6, " - ", 2)[,1], Delay6max = str_split_fixed(Delay_6, " - ", 2)[,2]) %>%
  mutate(Delay_1 = case_when(Delay1max != '' ~ Delay1max, Delay1max == '' ~ Delay1min) %>% as.numeric(),
         Delay_2 = case_when(Delay2max != '' ~ Delay2max, Delay2max == '' ~ Delay2min) %>% as.numeric(),
         Delay_3 = case_when(Delay3max != '' ~ Delay3max, Delay3max == '' ~ Delay3min) %>% as.numeric(),
         Delay_4 = case_when(Delay4max != '' ~ Delay4max, Delay4max == '' ~ Delay4min) %>% as.numeric(),
         Delay_5 = case_when(Delay5max != '' ~ Delay5max, Delay5max == '' ~ Delay4min) %>% as.numeric(),
         Delay_6 = case_when(Delay6max != '' ~ Delay6max, Delay6max == '' ~ Delay6min))

c(reviewtable$Delay_1,
  reviewtable$Delay_2,
  reviewtable$Delay_3,
  reviewtable$Delay_4,
  reviewtable$Delay_5,
  reviewtable$Delay_6) %>% unique()

reviewtable <- reviewtable%>% select(-c(ends_with('max'), ends_with('min')))

reviewtable <- reviewtable%>%
  mutate_at(vars(starts_with('Delay_')),
            funs(case_when(
              # 'immediate'
              . >=0      & . < 1 ~ 1, 
              # '1-10min'
              . >= 1     & . <= 11 ~ 2, 
              #'11-30min'
              . > 10     & . <= 30 ~ 3, 
              #'31-60min',
              . > 30     & . <= 60 ~ 4, 
              #'61min-12h'
              . > 60     & . <= 720 ~ 5, 
              #'12-24h'
              . > 720    & . <= 1440 ~ 6, 
              #'24-48h'
              . > 1440   & . <= 2880 ~ 7, 
              #'49h-1w'
              . > 2880   & . <= 10080 ~ 8,
              #'1.01wk-1mth'
              . > 10080  & . <= 43800 ~ 9,
              #'1-3mth'
              . > 43800  & . <= 131400 ~ 10, 
              #'3mth-1y'
              . > 131400 & . <= 525600 ~ 11, 
              #'1-2y'
              . > 525600 & . <= 1051200 ~ 12, 
              #'2+y'
              . > 1051200 ~ 13)))



# Define indentifier variables
identifier_var <- c("Entry_ID")

# Define NAs
reviewtable[reviewtable==-999] <- NA



# Quality check----

# 1. stimulus w/o modality and vice versa
dfCheck <- reviewtable %>%
  filter(is.na(Stimulus_1) & is.na(Stimulus_modality_1) == F |
           is.na(Stimulus_2) & is.na(Stimulus_modality_2) == F |
           is.na(Stimulus_3) & is.na(Stimulus_modality_3) == F |
           is.na(Stimulus_1) == F & is.na(Stimulus_modality_1) |
           is.na(Stimulus_2) == F & is.na(Stimulus_modality_2) |
           is.na(Stimulus_3) == F & is.na(Stimulus_modality_3)) %>%
  pull(Entry_ID)


dfCheck1 <- reviewtable %>%
  filter(   Stimulus_1 == 11 |Stimulus_2 == 11 |Stimulus_3 == 11   ) %>%
  pull(Entry_ID)


setdiff(reviewtable$Entry_ID, dfCheck)


# SAVE DATA FILE ----

writexl::write_xlsx(reviewtable, file.path(data_path, "Lit_Review_Preprocessed.xlsx"))
message('Create ', file.path(data_path, "Lit_Review_Preprocessed.xlsx"))
