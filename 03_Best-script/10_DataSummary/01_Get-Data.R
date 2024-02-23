rm(list=ls())

{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(reshape2)
  library(writexl)
  library(rstudioapi)
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


dfBinary <- read_xlsx(file.path(data_path, 'Lit_Review_binarized.xlsx')) 

# Summary for each levels ----
# Summary by Entry
dfSummary.nEntry <- dfBinary %>%
  select(#starts_with("Stimuli"),
         #starts_with("Modality"),
         starts_with("Cue manipulation"),
         contains("Similarity"),
         # starts_with("Retrieval"),
         # starts_with("Repetition"),
         starts_with("Eye tracking"),
         starts_with("Imaging"))%>%
  summarise(across(everything(), sum)) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("Level") %>%
  rename("nEntry" = `V1`) %>%
  mutate(totalEntry = nrow(dfBinary),
         pEntry = nEntry/nrow(dfBinary))



# Summary by article
dfBinaryArticle <- dfBinary %>%
  mutate(Article_ID = str_extract(Entry_ID, "\\w+(?=, Task)")) %>%
  select(Article_ID,
         # starts_with("Stimuli"),
         # starts_with("Modality"),
         starts_with("Cue manipulation"),
         contains("Similarity"),
         # starts_with("Retrieval"),
         # starts_with("Repetition"),
         starts_with("Eye tracking"),
         starts_with("Imaging"))%>%
  group_by(Article_ID) %>%
  summarise(across(everything(), sum)) %>%
  select(-Article_ID) %>%
  mutate_all(funs(case_when( . >= 1 ~ 1,
                             . == 0 ~ 0)))


dfSummary.nArticle <- dfBinaryArticle %>%
  select(starts_with("Stimuli"),
         starts_with("Modality"),
         starts_with("Cue manipulation"),
         contains("Similarity"),
         starts_with("Retrieval"),
         starts_with("Repetition"),
         starts_with("Eye tracking"),
         starts_with("Imaging"))%>%
  summarise(across(everything(), sum)) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("Level") %>%
  rename("nArticle" = `V1`) %>%
  mutate(totalArticle = nrow(dfBinaryArticle),
         pArticle = nArticle/nrow(dfBinaryArticle))


# Merge
df <- merge(dfSummary.nEntry, dfSummary.nArticle, by = "Level")
df <- df %>%
  mutate(Dimension = str_extract(Level, "\\D+"), .after = Level) %>%
  arrange(Dimension,desc(nEntry))

write_xlsx(df, file.path(descriptives_path,'DescriptiveTable.csv'))

# Summary for the presence of a category
dfSummary.Similarity <- dfBinary %>%
  select(starts_with("Similarity Manipulation"),
         starts_with("Cue Manipulation")) %>%
  select(-ends_with('1')) %>%
  transmute(
    nSimManipEncoding = rowSums(select(., contains('Encoding'))),
    nSimManipRetrieval = rowSums(select(., contains('Retrieval'))),
    nSimManipAny = rowSums(select(., contains('Similarity Manipulation'))),
    nCueManipAny = rowSums(select(., contains('Cue Manipulation'))))%>%
  # arrange(nSimManipAny)
  mutate_all(funs(case_when( . >= 1 ~ 1,
                             . == 0 ~ 0))) %>%
  summarise(across(everything(), sum)) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("Level") %>%
  rename("nEntry" = `V1`) %>%
  mutate(totalEntry = nrow(dfBinary),
         pEntry = nEntry/nrow(dfBinary))

write_xlsx(df, file.path(descriptives_path,'DescriptiveTable_Manipulation.csv'))