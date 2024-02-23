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
directory = getwd()  %>% str_extract(pattern = "^(.*?)mapping_analysis")

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
  source(file.path(funcon_path, "PN_DelayDf.R"))
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
delay1 <- PN_DelayDf(data=reviewtable, split.interval = ' - ')$delay1
MaxDelay <- PN_DelayDf(data=reviewtable, split.interval = ' - ')$MaxDelay


nTasktype <- delay1$Task_type %>% unique() %>% length()

# Step 2. Plotting ----
p <- ggplot(data = delay1)+
  # illustrate delay range:
  geom_polygon(aes(x = X, y = Y, group = ID_Delay, fill = Task_type), alpha = 0.3, show.legend = T)+
  # illustrate single delay:
  geom_segment(aes(x = Xstart, y = Ystart, xend = Xend, yend = Yend, group = ID_Delay, color = Task_type), alpha = 0.09, show.legend = F)+
  # illustrate each task entry, add text to help orient in plotly:
  geom_point(aes(x = Xend, y = Yend, group = ID_Delay, color = Task_type, text = nodeText), size = 0.5, show.legend = F)+
  geom_text(aes(x=MaxDelay+10,y=1.02, label = 'Task entry'))+
  geom_text(aes(x=MaxDelay+10,y=-.02, label = 'Delay'))+
  scale_x_continuous(breaks=c(0,525600,1051200,2102400,3153600),
                     labels = c("0","1y","2y","4y","6y"),
                     minor_breaks=seq(0,1440,by=1))+
  # facet_zoom(xlim = c(0,1440), y == 0 | y == 1, split = T, zoom.size = 1/3)+
  # # original palette
  # scale_color_manual(values = c('#3AE8B0','#19ADF0','#6967CE','#FFB900','#FD636B'))+
  # scale_fill_manual(values = c('#3AE8B0','#19ADF0','#6967CE','#FFB900','#FD636B'))+
  # project palette
  scale_color_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
  scale_fill_manual(values = TaskTypelevelPalette[1:nTasktype], name = 'Task type')+
  labs(x = 'Delay')+
  guides(fill = guide_legend(override.aes = list(alpha=1,size=8)))+
  # guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), size = guide_legend(order = 2, nrow = 1, byrow = T))+
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
    legend.text=element_text(size=15)
    #legend.position = c(.95,.75),
    #legend.background = element_rect(fill='transparent')
  ) 

p 

p1 <- plotly::ggplotly(p, tooltip = c('text'))
plotly::hide_legend(p1)

ggsave(filename = file.path(result_path,"Delay.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
message('Create ', file.path(result_path,"Delay.pdf"))

writexl::write_xlsx(delay1, file.path(data_path,"delay1.xlsx"))
