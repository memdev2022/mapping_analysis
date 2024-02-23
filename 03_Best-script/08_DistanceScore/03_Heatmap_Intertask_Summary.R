rm(list = ls())

rm(list = ls())
{
  data_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/08_DistanceScore/01_Data xlsx/'  
  script_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/01_Hierarchical-Edge-Bundling/'
  funcon_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/03_Best-script/00_FunCon/'
  result_path = '//mpib-berlin.mpg.de/FB-LIP/RHYME/STUDIES/COMIC/03_Main Study - Measurement/F_Analysis/LitReview visualization/Hierarchical Edge Bundling/04_Results/'
}

{
  # library(dplyr)
  # library(tidyverse)
  # library(readxl)
  # library(RColorBrewer)
}

# Cosmetics
{source(paste0(funcon_path, "PN_GetPalette.R"))}

########################################################################################################################
# STEP 1: extract new order of entries
# ------------------------

# Retrieve similarity/distance matrix
if (interactive() ){ 
  
  # first, identify the working directory. This is different for each machine
  whichModel <- readline('...choose model (1 = constrained, 2 = unconstrained) >>>> ')
  
  if (whichModel == '1'){sim <- read_xlsx(paste0(data_path,"intertask_constrained_sum.xlsx"))[,-1]} # choose: intertask_(un)constrained_sum.csv}
  else if (whichModel == '2'){sim <- read_xlsx(paste0(data_path,"intertask_unconstrained_sum.xlsx"))[,-1]}
    
    

for (row in 1:5){
  for (col in 1:5) {
    
    if (is.na(sim[row, col]) == F) {sim[row, col] = sim[row, col]}
    else {sim[row, col] = sim[col, row]}
    
  }
  
}

sim <- as.matrix(sim)
rownames(sim) <- as.character(c(1:5))
Heatmap(sim)
########################################################################################################################
# STEP 3: plot matrix
# # https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/#r-packagesfunctions-for-drawing-heatmaps
# ------------------------

# if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)


# add annotation for task type
ha_top <- HeatmapAnnotation(Task_type = c(1:5),
                            annotation_label = 'Task type', 
                            show_legend = c("bar" = FALSE),
                            col = list(Task_type = c("1" = "#4F707F", "2" = "#54BAB2", "3" = "#F9EDA8", "4" = "#FCB270", "5" = "#E76466")))

ha_top <- HeatmapAnnotation(Task_type = c(1:5), 
                            show_legend = c("bar" = FALSE),
                            annotation_label = "Task type",
                            # col = list(Task_type = c("1" = "#00CD6C", "2" = "#009ADE", "3" = "#AF58BA", "4" = "#FFC61E", "5" = "#F28522"))
                            col = list(Task_type = c("1" = TaskTypelevelPalette[1],
                                                     "2" = TaskTypelevelPalette[2],
                                                     "3" = TaskTypelevelPalette[3],
                                                     "4" = TaskTypelevelPalette[4],
                                                     "5" = TaskTypelevelPalette[5])))

ha_left <- rowAnnotation(Task_type = c(1:5), 
                         col = list(Task_type = c("1" = "#4F707F", "2" = "#54BAB2", "3" = "#F9EDA8", "4" = "#FCB270", "5" = "#E76466")),
                         show_legend = c("bar" = FALSE), show_annotation_name = c(bar = FALSE))

ha_left <- rowAnnotation(Task_type = c(1:5),
                         # col = list(Task_type = c("1" = "#00CD6C", "2" = "#009ADE", "3" = "#AF58BA", "4" = "#FFC61E", "5" = "#F28522")),
                         col = list(Task_type = c("1" = TaskTypelevelPalette[1],
                                                  "2" = TaskTypelevelPalette[2],
                                                  "3" = TaskTypelevelPalette[3],
                                                  "4" = TaskTypelevelPalette[4],
                                                  "5" = TaskTypelevelPalette[5])),
                         show_annotation_name = c(bar = FALSE),
                         show_legend = c("bar" = FALSE))

# make plot
if (whichModel == '1'){
  pdf(paste0(result_path,"Heatmap_IntertaskConstrained.pdf"))
  Heatmap(as.matrix(sim), name = "Similarity",
          # col = rev(brewer.pal(n=9, name="Spectral")),
          col = colorRamp2(seq(-.2,1,.2), c("#DFDFDF", "#FCFCFC", "#FCFCFC", "#F7DBC8", "#E9A787", "#C96652", "#A62B30")),
          heatmap_legend_param = list(at = seq(0,1,.2)), 
          cluster_columns = F, cluster_rows = F,
          # column_split = 15,
          show_row_names = FALSE, show_column_names = TRUE, column_names_rot = 0,
          column_names_side ='top',
          top_annotation = ha_top,
          left_annotation = ha_left,
          width = unit(5, "in"), height = unit(5, "in"))
  dev.off()
  
  
} 
else if (whichModel == '2'){
  
  pdf(paste0(result_path,"Heatmap_IntertaskUnconstrained.pdf"))
  Heatmap(as.matrix(sim), name = "Similarity",
          # col = rev(brewer.pal(n=9, name="Spectral")),
          col = colorRamp2(seq(-.2,1,.2), c("#DFDFDF", "#FCFCFC", "#FCFCFC", "#F7DBC8", "#E9A787", "#C96652", "#A62B30")),
          heatmap_legend_param = list(at = seq(0,1,.2)), 
          cluster_columns = F, cluster_rows = F,
          # column_split = 15,
          show_row_names = FALSE, show_column_names = TRUE, column_names_rot = 0,
          column_names_side ='top',
          top_annotation = ha_top,
          left_annotation = ha_left,
          width = unit(5, "in"), height = unit(5, "in"))
  dev.off()
}


}