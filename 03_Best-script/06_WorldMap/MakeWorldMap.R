rm(list=ls())

{library(data.table); library(svDialogs)
  library(readxl)
  library(reshape2)
  library(dplyr)
  library(stringr)
  library(writexl)  
  library(ggplot2)
  library(viridis)
  library(giscoR)
  library(sf)
}

# Set up directory and paths
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
directory = getwd() %>% str_extract(pattern = '^(.*?)mapping_analysis')

# This script produces HEB with connections representing the trend of multiple entries
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/08_WorldMap/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# Get functions
source(file.path(funcon_path, "PN_CountLocation.R"), local = TRUE)
# Read data set
reviewtable <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx"))[1:100,] 
# Make data frame to summarize locations
df <- PN_CountLocation(reviewtable)

if (interactive() ){
  
  myPlotType = dlg_list(title='Make World Map according to?', choices=c('1=Article', '2=Entry'))$res #readline('Make World Map according to (1=Article, 2=Entry) >> ')
  
  if(myPlotType == '1=Article') {
    
    # Make World Map by Article ----
  
    df.location <- df %>%
      rename('sovereignt' = `Location`) %>% 
      mutate(
        Article = case_when(n.Article > 0 & n.Article <= 10 ~ 10,
                            n.Article > 10 & n.Article <= 20 ~ 20,
                            n.Article > 20 & n.Article <= 30 ~ 30,
                            n.Article > 30 & n.Article <= 40 ~ 40,
                            n.Article > 40 ~ 100),
        text.location = paste0(sovereignt, ": ", n.Article)) %>%
      mutate(sovereignt= case_when(sovereignt == 'United States of America'~'United States',
                                   sovereignt != 'United States of America'~sovereignt)) %>%
      rename('NAME_ENGL'='sovereignt')
    
    {library(giscoR)
    library(dplyr)
    library(sf)
    library(ggplot2)}
    
    epsg_code <- 4088
    
    # European countries
    world <- gisco_get_countries() %>%
      st_transform(epsg_code)
    world <- sf::st_cast(world, 'MULTIPOLYGON')
    
    worlddf <- left_join(world, df.location, by='NAME_ENGL')
    worlddf$Article[!is.na(worlddf$Article)] = 1
    worlddf$Article[is.na(worlddf$Article)] = 0
    worlddf$Article = as.factor(worlddf$Article)
    
    
    # Countries centroids
    symbol_pos <- st_centroid(world, of_largest_polygon = TRUE)
    symbol_pos <- merge(symbol_pos,df.location, by='NAME_ENGL') %>% arrange(n.Article)
    
    
    
    p<-ggplot() +
      geom_sf(data=worlddf %>% filter(Article == 1), fill = '#87E1D4', color = '#D6E6E2')+
      geom_sf(data=worlddf %>% filter(Article == 0), fill = '#D6E6E2', color = '#D6E6E2')+
      theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.key = element_rect(fill = "transparent", colour = NA) # get rid of key legend fill, and of the surrounding
            )+
      # Labels position (centroids)
      geom_sf(data = symbol_pos, pch = 21, alpha = .6, aes(size=n.Article, fill = n.Article, color = n.Article))+
      scale_fill_gradientn(colours=magma(10),
                           name="Number of Articles", 
                           trans = "log",
                           breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      scale_color_gradientn(colours=magma(10),
                           name="Number of Articles", 
                           trans = "log",
                           breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      scale_size_continuous(name = "Number of Articles",  range = c(2, 18), breaks = c(1,5,10,40,max(symbol_pos$n.Article)))+
      guides(size = 'none')
    p
    plotly::ggplotly(p)
    
    # Save plot
    ggsave(filename = file.path(result_path,"WorldMapArticle.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
    message('Create ', file.path(result_path,"WorldMapArticle.pdf"))
  
  }
  
  else if (myPlotType == '2=Entry') {
    
    # Make World Map by Entry ----
    
    
    # Read data set
    df.location <- df %>%
      rename('sovereignt' = `Location`) %>% 
      mutate(
        Entry = case_when(n.Entry > 0 & n.Entry <= 10 ~ 10,
                          n.Entry > 10 & n.Entry <= 20 ~ 20,
                          n.Entry > 20 & n.Entry <= 30 ~ 30,
                          n.Entry > 30 & n.Entry <= 40 ~ 40,
                          n.Entry > 40 ~ 100),
        text.location = paste0(sovereignt, ": ", n.Entry)) %>%
      mutate(sovereignt= case_when(sovereignt == 'United States of America'~'United States',
                                   sovereignt != 'United States of America'~sovereignt)) %>%
      rename('NAME_ENGL'='sovereignt')
    
    {library(giscoR)
      library(dplyr)
      library(sf)
      library(ggplot2)}
    
    epsg_code <- 4088
    
    # European countries
    world <- gisco_get_countries() %>%
      st_transform(epsg_code)
    world <- sf::st_cast(world, 'MULTIPOLYGON')
    
    worlddf <- left_join(world, df.location, by='NAME_ENGL')
    worlddf$Entry[!is.na(worlddf$Entry)] = 1
    worlddf$Entry[is.na(worlddf$Entry)] = 0
    worlddf$Entry = as.factor(worlddf$Entry)
    
    
    # Countries centroids
    symbol_pos <- st_centroid(world, of_largest_polygon = TRUE)
    symbol_pos <- merge(symbol_pos,df.location, by='NAME_ENGL') %>% arrange(n.Entry)
    
    
    
    p<-ggplot() +
      geom_sf(data=worlddf %>% filter(Entry == 1), fill = '#87E1D4', color = '#D6E6E2')+
      geom_sf(data=worlddf %>% filter(Entry == 0), fill = '#D6E6E2', color = '#D6E6E2')+
      theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.key = element_rect(fill = "transparent", colour = NA) # get rid of key legend fill, and of the surrounding
      )+
      # Labels position (centroids)
      geom_sf(data = symbol_pos, pch = 21, alpha = .6, aes(size=n.Entry, fill = n.Entry, color = n.Entry, text=text.location))+
      scale_fill_gradientn(colours=magma(10),
                           name="Number of Entries", 
                           trans = "log",
                           breaks = c(1,5,10,40,max(symbol_pos$n.Entry)))+
      scale_color_gradientn(colours=magma(10),
                            name="Number of Entries", 
                            trans = "log",
                            breaks = c(1,5,10,40,max(symbol_pos$n.Entry)))+
      scale_size_continuous(name = "Number of Entries",  range = c(2, 18), breaks = c(1,5,10,40,max(symbol_pos$n.Entry)))+
      guides(size = 'none')
    p
    ggplotly(p, tooltip = 'text')
    
    # Save plot
    ggsave(filename = file.path(result_path,"WorldMapEntry.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
    message('Create ', file.path(result_path,"WorldMapEntry.pdf"))
    
  }
  
  write_xlsx(df, file.path(descriptives_path,"DataCollectionLocation.xlsx"))
  message('Create ', file.path(descriptives_path,"DataCollectionLocation.xlsx"))
    
}
