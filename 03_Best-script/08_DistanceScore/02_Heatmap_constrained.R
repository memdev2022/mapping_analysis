rm(list = ls())

{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(RColorBrewer)
  library(circlize)
  library(rstudioapi)
}

directory = getwd() %>% str_extract("[^litreview_visualization]+") %>% file.path("/litreview_visualization/")  

# This script produces HEB with connections representing the trend of multiple entries
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# Cosmetics
{source(file.path(funcon_path, "PN_GetPalette.R"))}




# STEP 1: extract new order of entries ------------------------

# rearrange task entries (Entry_ID = Article ID + Task number) in the original dfRaw by task type
dfRaw <- read_xlsx(file.path(data_path,'Lit_Review_QC.xlsx')) %>% # choose: Lit_Review_QC.xlsx (raw data frame)
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = ", "), .before = Article_ID) %>%
  arrange(Task_type)
  
Entry_ID <- dfRaw$Entry_ID
Task_type <- dfRaw$Task_type
nTask_type <- unique(Task_type) %>% length()

# No direct information about task type
# rearrange task entries in Lit_Review_binarized according to the order in the dfRaw
dfBinary <- read_xlsx(file.path(data_path,'Lit_Review_binarized.xlsx')) # choose: Lit_Review_binarized_constrained.xlsx 
order.dfBinary <- match(dfRaw$Entry_ID, dfBinary$Entry_ID) #trick: find out the positions of entries in 



# STEP 2: retrieve similarity/distance matrix in new order------------------------

# Retrieve similarity/distance matrix

sim <- read.csv(file.path(data_path,'similarity_constrained.csv')) # how to create this, go to repository litreview_simcalcpipeline
simMatrix_arranged <- sim[order.dfBinary, order.dfBinary] %>% as.matrix()



# STEP 3: plot matrix------------------------
# # https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/#r-packagesfunctions-for-drawing-heatmaps


# if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)


# add annotation for task type
ha_top <- HeatmapAnnotation(Task_type = Task_type, 
                            annotation_label = "Task type",
                            # col = list(Task_type = c("1" = "#00CD6C", "2" = "#009ADE", "3" = "#AF58BA", "4" = "#FFC61E", "5" = "#F28522"))
                            col = list(Task_type = c("1" = TaskTypelevelPalette[1],
                                                     "2" = TaskTypelevelPalette[2],
                                                     "3" = TaskTypelevelPalette[3],
                                                     "4" = TaskTypelevelPalette[4],
                                                     "5" = TaskTypelevelPalette[5])))
ha_left <- rowAnnotation(Task_type = Task_type,
                         # col = list(Task_type = c("1" = "#00CD6C", "2" = "#009ADE", "3" = "#AF58BA", "4" = "#FFC61E", "5" = "#F28522")),
                         col = list(Task_type = c("1" = TaskTypelevelPalette[1],
                                                  "2" = TaskTypelevelPalette[2],
                                                  "3" = TaskTypelevelPalette[3],
                                                  "4" = TaskTypelevelPalette[4],
                                                  "5" = TaskTypelevelPalette[5])),
                         show_annotation_name = c(bar = FALSE),
                         show_legend = c("bar" = FALSE))

# make plot
# pdf(paste0(result_path,"Heatmap_SimilarityConstrained.pdf"))
Heatmap(simMatrix_arranged, name = "Similarity",
        # col = rev(brewer.pal(n=9, name="Spectral")),
        col = colorRamp2(seq(0,1,.2), rev(heatmatrixPalette)),
        heatmap_legend_param = list(at = seq(0,1,.2)),
        cluster_columns = F, cluster_rows = F,
        show_row_names = FALSE, show_column_names = FALSE,
        top_annotation = ha_top,
        left_annotation = ha_left,
        width = unit(5, "in"), height = unit(5,"in"))
# dev.off()
