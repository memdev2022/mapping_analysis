rm(list = ls())



{
  data_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/01_Data xlsx/'
  script_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/'
  descriptives_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/05_Descriptives/'
  
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(RColorBrewer)

  }
########################################################################################################################
# STEP 1: extract new order of entries
# ------------------------

# rearrange task entries (Entry_ID = Article ID + Task number) in the original dfRaw by task type
dfRaw <- read_xlsx(file.choose()) %>% # choose: Lit_Review_QC.xlsx (raw data frame)
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_"), .before = Article_ID) %>%
  arrange(Task_type)
  
Entry_ID <- dfRaw$Entry_ID
Task_type <- dfRaw$Task_type

{Task_type_1 <- which(Task_type == 1)
Task_type_2 <- which(Task_type == 2)
Task_type_3 <- which(Task_type == 3)
Task_type_4 <- which(Task_type == 4)
Task_type_5 <- which(Task_type == 5)}

# No direct information about task type
# rearrange task entries in Lit_Review_binarized according to the order in the dfRaw
dfBinary <- read_xlsx(file.choose()) # choose: Lit_Review_binarized_constrained.xlsx 
order.dfBinary <- match(dfRaw$Entry_ID, dfBinary$Entry_ID) #trick: find out the positions of entries in 


########################################################################################################################
# STEP 2: retrieve similarity/distance matrix in new order
# ------------------------

# Retrieve similarity/distance matrix

sim <- read.csv(file.choose()) # choose: similarity_constrained.csv 
# sim$X <- NULL
simMatrix_arranged <- sim[order.dfBinary, order.dfBinary] %>% as.matrix()

all_entry <- c(1:nrow(simMatrix_arranged))


df.IntertaskSimilarity <- data.frame()

for (current_entry in 1:nrow(simMatrix_arranged)) {
  
  current_Entry_ID <- Entry_ID[current_entry]
  current_Task_type <-  Task_type[current_entry]
  
  if (current_Task_type == 1) {
    current_Ingroup_entries <- setdiff(Task_type_1, current_entry)
    current_Outgroup_entries <- setdiff(all_entry, Task_type_1)
  } else if (current_Task_type == 2) {
    current_Ingroup_entries <- setdiff(Task_type_2, current_entry)
    current_Outgroup_entries <- setdiff(all_entry, Task_type_2)
  } else if (current_Task_type == 3) {
    current_Ingroup_entries <- setdiff(Task_type_3, current_entry)
    current_Outgroup_entries <- setdiff(all_entry, Task_type_3)
  } else if (current_Task_type == 4) {
    current_Ingroup_entries <- setdiff(Task_type_4, current_entry)
    current_Outgroup_entries <- setdiff(all_entry, Task_type_4)
  } else if (current_Task_type == 5) {
    current_Ingroup_entries <- setdiff(Task_type_5, current_entry)
    current_Outgroup_entries <- setdiff(all_entry, Task_type_5)
  }
  
  current_Overall_entries <- setdiff(all_entry, current_entry)
  
  current_IngroupSimilarity = mean(simMatrix_arranged[current_entry, current_Ingroup_entries])
  current_OutgroupSimilarity = mean(simMatrix_arranged[current_entry, current_Outgroup_entries])
  current_OverallSimilarity = mean(simMatrix_arranged[current_entry, current_Overall_entries])
  
  current_df.IntertaskSimilarity <- data.frame(Entry_ID = current_Entry_ID,
                                               Task_type = current_Task_type,
                                               ingroupSimilarity = current_IngroupSimilarity,
                                               outgroupSimilarity = current_OutgroupSimilarity,
                                               overallSimilarity = current_OverallSimilarity)
  
  df.IntertaskSimilarity <- rbind(df.IntertaskSimilarity, current_df.IntertaskSimilarity)
  
  
}

df_top10_IngroupSimilarity <- df.IntertaskSimilarity %>%
  group_by(Task_type) %>% 
  filter(quantile(ingroupSimilarity, 0.9) < ingroupSimilarity) %>%
  arrange(Task_type, desc(ingroupSimilarity)) %>%
  select(Entry_ID, Task_type, ingroupSimilarity)

df_top10_OutgroupSimilarity <- df.IntertaskSimilarity %>%
  group_by(Task_type) %>% 
  filter(quantile(outgroupSimilarity, 0.9) < outgroupSimilarity) %>%
  arrange(Task_type, desc(outgroupSimilarity)) %>%
  select(Entry_ID, Task_type, outgroupSimilarity)

# df_top10_overallSimilarity <- df.IntertaskSimilarity %>%
#   group_by(Task_type) %>% 
#   filter(quantile(overallSimilarity, 0.9) < overallSimilarity) %>%
#   arrange(Task_type, desc(overallSimilarity)) %>%
#   select(Entry_ID, Task_type, overallSimilarity)
# 
# df_top10_WeirdSimilarity <- df.IntertaskSimilarity %>%
#   filter(quantile(ingroupSimilarity, 0.9) < ingroupSimilarity &
#            quantile(outgroupSimilarity, 0.9) < outgroupSimilarity &
#            quantile(overallSimilarity, 0.9) < overallSimilarity)
# 

write.csv(df_top10_IngroupSimilarity, sprintf('%s%s.csv', descriptives_path, deparse(substitute(df_top10_IngroupSimilarity))), row.names = F)
write.csv(df_top10_OutgroupSimilarity, sprintf('%s%s.csv', descriptives_path, deparse(substitute(df_top10_OutgroupSimilarity))), row.names = F)
# write.csv(df_top10_overallSimilarity, sprintf('%s%s.csv', descriptives_path, deparse(substitute(df_top10_overallSimilarity))), row.names = F)
