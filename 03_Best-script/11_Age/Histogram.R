# This script illustrates delays from entries

rm(list=ls())
options(scipen = 999)


# Load packages ----
{library(dplyr) 
  library(tidyverse) 
  library(readxl) 
  library(ggplot2)
  library(ggalt)
  library(ggforce)
}
# Set up directory and paths
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
directory = getwd() %>% str_extract(pattern = '^(.*?)mapping analysis')

# This script produces HEB with connections representing the trend of multiple entries
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/09_WorldMap/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}


{
  source(file.path(funcon_path, "PN_GetPalette.R"))
  source(file.path(funcon_path, "PN_DelayDf.R"))
}


# Step 1. Preprocessing ----
reviewtable <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx")) %>%
  select(Entry_ID, starts_with('Age')) 

reviewtable_long <- reviewtable %>%
  pivot_longer(!Entry_ID,
               names_to = 'AgeGroup',
               values_to = 'istested')%>%
  filter(istested==1) %>%
  rowwise()%>%
  mutate(AgeGroupbin=str_extract(AgeGroup,'(?<=Age ).*') %>% as.numeric())

# Step 2. Histogram: The number of task entries that tested each age group 
(p <- ggplot(reviewtable_long) +
  geom_bar(aes(x=AgeGroupbin), fill=nodePalette[1],alpha=.7)+
  labs(x = 'Age', y='Number of entries')+
  guides(fill = guide_legend(override.aes = list(alpha=1,size=8)))+
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 18, by=1), labels = c(as.character(0:17), '18+'))+
  theme(
    aspect.ratio = 1,
    axis.text=element_text(size=15),
    axis.title = element_text(size=18),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
    legend.title=element_text(size=18), 
    legend.text=element_text(size=16)
  )) 



ggsave(filename = file.path(result_path,"BarPlot_Age.pdf"), p, width = 634, height = 634, units = "px", dpi = "screen")
message('Create ', file.path(result_path,"BarPlot_Age.pdf"))
