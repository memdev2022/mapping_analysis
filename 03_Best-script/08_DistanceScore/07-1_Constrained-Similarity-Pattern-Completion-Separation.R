# This script extract pairwise similarity of selected entries.
# These entries investigated Pattern Separation/Completion
# Similarity scores are retrieved from the constrained version



rm(list=ls())
# Temporary
# This script add PS and PC into the former QC sheet
{
  library(dplyr) #step1
  library(tidyverse) #step1
  library(readxl) #step1
  library(reshape2) #step1
  library(stringr) #step1
  library(writexl)

  data_path = "//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/01_Data xlsx/"
  script_path = "//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/"

  }

# I will use pattern-completion entries as example


# We want to get pairwise similarity of PC entries only
# To do this, we have to get the right Entry_IDs and then find
# their position in the matrix


# STEP 1: extract Entry_ID ----

# Pattern completion was stored in the original QC data frame (dfQC)
# let's retrieve this QC sheet, find those PC entries
dfQC <- read_xlsx(sprintf('%sLit_Review_QC.xlsx', data_path)) %>% # choose: Lit_Review_QC.xlsx
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_"), .before = Article_ID) # created Entry_ID because the QC sheet did not have it


# get Entry_IDs of interest
dfQC.filtered <- dfQC %>% filter(PC == 1)
dfQC.filtered.Entry_ID = dfQC.filtered$Entry_ID

# STEP 2: get position of the PC-entries in the similarity matrix ----


# Retrieve the similarity matrix [matSim]
matSim <- read.csv(sprintf('%ssimilarity_constrained.csv ', data_path))  # choose: similarity_constrained.csv


# Notice how the matrix has no information about Entry_ID.
# The similarity matrix was computed directly from the binary data frame [dfBinary], 
# we can infer Entry_ID of the matrix from this binary dataset

# Retrieve the binary data frame
dfBinary <- read_xlsx(sprintf('%sLit_Review_binarized.xlsx', data_path)) # choose: Lit_Review_binarized.xlsx

# find the position of our filtered Entry_IDs in the binary data frame
dfBinary.filtered <- dfBinary %>% filter(Entry_ID %in% dfQC.filtered.Entry_ID)
dfBinary.filtered.Entry_ID = dfBinary.filtered$Entry_ID
dfBinary.filtered.Entry_ID.position = match(dfBinary.filtered.Entry_ID, dfBinary$Entry_ID)

# the positions in the dfBinary is exactly the positions in the matSim
matSim.filtered.Entry_ID.position = dfBinary.filtered.Entry_ID.position
matSim.filtered.Entry_ID = dfBinary.filtered.Entry_ID


# STEP 3: get a similarity matrix with only PC-entries ----


matSim.filtered <- matSim[matSim.filtered.Entry_ID.position, matSim.filtered.Entry_ID.position]

# delete the lower half of the matrix
matSim.filtered[lower.tri(matSim.filtered)] <- NA

# Prepare for data manipulation
# transform matrix to data frame: its columns and rows are identified by Entry_ID
dfSim.filtered <- matSim.filtered %>% 
  as.data.frame() %>%
  mutate(Entry1 = matSim.filtered.Entry_ID, .before = X1) # first column = Entry_ID
names(dfSim.filtered)[-1] = matSim.filtered.Entry_ID # other columns = Entry_ID

# Melt the dfSim to get a long format
dfSim.filtered <- melt(dfSim.filtered,
                      id.vars = "Entry1",
                      variable.name = "Entry2",
                      value.name = "Similarity")

# We only allow the following similarity scores:
# >>>> similarity score between an entry with another entry other than itself
# >>>> similarity score that is not NA (because we deleted the lower half of the matrix)
dfSim.filtered <- dfSim.filtered %>% 
  filter(Entry1 != Entry2,
         !(is.na(Similarity)))

# Add title of entries
dfSim.filtered <- left_join(dfSim.filtered, dfQC.filtered %>% select(Entry_ID, Title), by = c("Entry1"="Entry_ID"))
names(dfSim.filtered)[names(dfSim.filtered) == "Title"] <- "Title1"
dfSim.filtered <- left_join(dfSim.filtered, dfQC.filtered %>% select(Entry_ID, Title), by = c("Entry2"="Entry_ID"))
names(dfSim.filtered)[names(dfSim.filtered) == "Title"] <- "Title2"


# Last,
# We rank order of highest to lowest pairwise similarity
dfSim.ranked <- dfSim.filtered %>%
  arrange(desc(Similarity))

