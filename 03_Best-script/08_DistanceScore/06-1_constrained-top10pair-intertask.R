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

# Cosmetics
{source(paste0(funcon_path, "PN_GetPalette.R"))}


########################################################################################################################
# STEP 1: extract new order of entries
# ------------------------

# rearrange task entries (Entry_ID = Article ID + Task number) in the original dfRaw by task type
dfRaw <- read_xlsx(sprintf('%sLit_Review_QC.xlsx', data_path)) %>% # choose: Lit_Review_QC.xlsx (raw data frame)
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
dfBinary <- read_xlsx(sprintf('%sLit_Review_binarized.xlsx', data_path)) # choose: Lit_Review_binarized_constrained.xlsx 
order.dfBinary <- match(dfRaw$Entry_ID, dfBinary$Entry_ID) #trick: find out the positions of entries in 


########################################################################################################################
# STEP 2: retrieve similarity/distance matrix in new order
# ------------------------

# Retrieve similarity/distance matrix

sim <- read.csv(sprintf('%ssimilarity_constrained.csv ', data_path))  # choose: similarity_constrained.csv 

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

df.sim2 <- df.sim1 %>% filter(Task_type1 == Task_type2 &
                                substr(Entry1, 1, nchar(Entry1)-7) != substr(Entry2, 1, nchar(Entry2)-7))


df.SimilarPair <- df.sim2 %>%
  group_by(Task_type1) %>%
  filter(quantile(Similarity, 0.9) < Similarity) %>%
  arrange(Task_type1, desc(Similarity))

df.SimilarPair <- merge(df.SimilarPair, dfRaw %>% select(Entry_ID, Title), by.x = "Entry1", by.y = "Entry_ID")
names(df.SimilarPair)[names(df.SimilarPair) == "Title"] <- "Title1"
df.SimilarPair <- merge(df.SimilarPair, dfRaw %>% select(Entry_ID, Title), by.x = "Entry2", by.y = "Entry_ID")
names(df.SimilarPair)[names(df.SimilarPair) == "Title"] <- "Title2"

df.SimilarPair <- df.SimilarPair %>%
  arrange(Task_type1,desc(Similarity))

write.csv(df.SimilarPair, sprintf('%s%s.csv', descriptives_path, deparse(substitute(df.SimilarPair))), row.names = F)
