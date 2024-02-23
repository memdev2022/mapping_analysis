rm(list = ls())
{
  data_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/01_Data xlsx/'  
  script_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/01_Hierarchical-Edge-Bundling/'
  funcon_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/00_FunCon/'
  result_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/04_Results/'
  descriptives_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/05_Descriptives/'
  
}

{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(RColorBrewer)
  library(reshape2)
}


########################################################################################################################
# STEP 1: extract new order of entries
# ------------------------

# rearrange task entries (Entry_ID = Article ID + Task number) in the original dfRaw by task type
dfRaw <- read_xlsx(sprintf('%sLit_Review_QC.xlsx', data_path)) %>% # choose: Lit_Review_QC.xlsx (raw data frame)
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_"), .before = Article_ID) %>%
  arrange(Task_type)
  
Entry_ID <- dfRaw$Entry_ID
Task_type <- dfRaw$Task_type


# No direct information about task type
# rearrange task entries in Lit_Review_binarized according to the order in the dfRaw
dfBinary <- read_xlsx(sprintf('%sLit_Review_binarized.xlsx', data_path)) # choose: Lit_Review_binarized_constrained.xlsx 
order.dfBinary <- match(dfRaw$Entry_ID, dfBinary$Entry_ID) #trick: find out the positions of entries in 


# 04.01. Modification: only include task entries that have a similarity manipulation 
# (doesnt matter which type or whether it's at encoding or retrieval).

dfBinary_SimManip <-  dfBinary %>%
  rowwise() %>%
  mutate(xxxxxxx = sum(`Similarity Manipulation Encoding 2`:`Similarity Manipulation Encoding 6`) + sum(`Similarity Manipulation Retrieval 2`:`Similarity Manipulation Retrieval 6`)) %>%
  filter(xxxxxxx>0)

chosenEntry_ID <- dfBinary_SimManip$Entry_ID

########################################################################################################################
# STEP 2: retrieve similarity/distance matrix in new order
# ------------------------

# Retrieve similarity/distance matrix

sim <- read.csv(sprintf('%ssimilarity_constrained.csv ', data_path))  # choose: similarity_constrained.csv 
# sim$X <- NULL
simMatrix_arranged <- sim[order.dfBinary, order.dfBinary] %>% as.matrix()
simMatrix_arranged[lower.tri(simMatrix_arranged)] <- NA

all_entry <- c(1:nrow(simMatrix_arranged))

df.sim <- simMatrix_arranged %>% as.data.frame()
names(df.sim) <- Entry_ID
df.sim <- df.sim %>%
  mutate(Entry1 = Entry_ID)

df.sim <- melt(df.sim,
               id.vars = "Entry1",
               variable.name = "Entry2",
               value.name = "Similarity")

df.sim$Entry2 <- as.character(df.sim$Entry2) 

df.sim <- df.sim %>%
  filter(Entry1 != Entry2,
         !(is.na(Similarity)))

df.sim1 <- merge(x=df.sim, y=dfRaw %>% select(Entry_ID, Task_type),
                 by.x= "Entry1", by.y= "Entry_ID")
names(df.sim1)[4] <- "Task_type1"
df.sim1 <- merge(x=df.sim1, y=dfRaw %>% select(Entry_ID, Task_type),
                 by.x= "Entry2", by.y= "Entry_ID")
names(df.sim1)[5] <- "Task_type2"

df.sim2 <- df.sim1 %>% 
  filter(Task_type1 == Task_type2 &
           substr(Entry1, 1, nchar(Entry1)-7) != substr(Entry2, 1, nchar(Entry2)-7)) %>%
  filter(Entry1 %in% chosenEntry_ID & Entry2 %in% chosenEntry_ID)



df.SimilarPair <- df.sim2 %>%
  group_by(Task_type1) %>%
  filter(quantile(Similarity, 0.9) < Similarity) %>%
  arrange(Task_type1, desc(Similarity))
 

write.csv(df.SimilarPair, sprintf('%s%s_SimManip.csv', descriptives_path, deparse(substitute(df.SimilarPair))), row.names = F)
