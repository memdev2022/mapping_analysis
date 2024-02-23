# This script illustrates delays from entries

rm(list=ls())
options(scipen = 999)


# Load packages ----
{library(dplyr) 
library(tidyverse) 
library(readxl) 
library(reshape2)
library(ggplot2)
library(ggalt)
library(ggforce)
}
# Set up directory and paths
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
directory = getwd() %>% str_extract(pattern = "^(.*?)mapping analysis")

# This script produces HEB with connections representing the trend of multiple entries
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/04_Delay/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}


{
  source(file.path(funcon_path, "PN_GetPalette.R"))
  source(file.path(funcon_path, "PN_DelayDf_Stack.R"))
    }
# Step 1. Preprocessing ----
reviewtable <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx")) %>%
  select(Article_ID, Task_number,
         starts_with('Delay_'),
         starts_with('Task_type')) %>%
  mutate(Entry_ID = paste(Article_ID,Task_number,sep=', '),.before = Article_ID) %>%
  select(-c(Task_number, Article_ID)) %>%
  # remove entries without information about delay
  filter(Delay_1 > 0)


# Step 2. Use function to transform data frame
delay1 <- PN_DelayDf_Stack(data=reviewtable, split.interval = ' - ')
nTasktype <- delay1$Task_type %>% unique() %>% length()

# Step 2. Plotting ----

p <- ggplot(data=delay1)+
  geom_segment(aes(x=X.a, xend=X.b, y=Y, yend=Y, group=ID_Delay, color=Task_type))+
  geom_point(aes(x=X.a, y=Y, group=ID_Delay, color=Task_type, text=text))+
  geom_point(aes(x=X.b, y=Y, group=ID_Delay, color=Task_type, text=text))+
  geom_line(aes(x=X.a, y=Y, group=Entry_ID, color=Task_type), linetype='dotted')+
  scale_x_continuous(breaks=c(0,525600,1051200,2102400,3153600),
                     labels = c("0","1y","2y","4y","6y"),
                     minor_breaks=seq(0,1440,by=1), limits = c(0,3153600))+
  ylim(-1, 5040)+
  scale_color_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
  scale_fill_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
  labs(x = 'Delay')+
  guides(fill = guide_legend(override.aes = list(alpha=1,size=8)))+
  theme(
    axis.text.x=element_text(size=15),
    axis.title.x = element_text(size=18),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line.x = element_line(colour = "black"), 
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
    legend.title=element_text(size=18), 
    legend.text=element_text(size=15)) 

p 

p1 <- plotly::ggplotly(p, tooltip = c('text'))
plotly::hide_legend(p1)

# ggsave(filename = file.path(result_path,"Delay.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
# message('Create ', file.path(result_path,"Delay.pdf"))

# writexl::write_xlsx(delay1, file.path(data_path,"delay1.xlsx"))
