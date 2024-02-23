rm(list =  ls())

{library(data.table)
  library(readxl)
  library(reshape2)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library("cowplot")
  library(svDialogs)
}

if (interactive() ){
  
  # Set up directory and paths
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  directory = getwd() %>% str_extract(pattern = '^(.*?)mapping_analysis')
  
  
  {
    data_path = file.path(directory,'00_Data xlsx/')
    funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
    preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
    script_path = file.path(directory,'03_Best-script/03_Pie/')
    result_path = file.path(directory,'04_Results/')
    descriptives_path = file.path(directory,'05_Descriptives/')
  }
  
 
  
  # Decide what to draw
  print('available variables: Task_type/Encoding_instruction/Design/To_be_remembered_information')
  whatvar <- dlg_list(title="Which variable to make pie?", 
                       choice=c("Task_type","Encoding_instruction","Design",
                       "To-be-remembered_information"))$res 
  if (length(whatvar) == 0) {msg_box('Warning: No variable was chosen!')}
  
  if (whatvar=='Task_type'){
    pielabels =c('Direct memory','Autobiographical memory','Statistical learning',
                                         'Generalization','Semantic knowledge')
    pietitle = 'Task type'} else
      
    if (whatvar=='Encoding_instruction'){
      pielabels= c('Intentional','Incidental', 'Manipulated: intention and incidental',
                                                      'Unspecified', 'NA, for personal events or semantic knowledge tasks')
      pietitle = 'Encoding instruction'} else
      
        if (whatvar=='Design'){
        pielabels=c('Cross-sectional','Longitudinal')
        pietitle = 'Design'
        } else
        
          if (whatvar=='To-be-remembered_information'){
            pielabels=c('Individual item','Associative co-occurence','Temporal memory',
                                                                 'What-where-when','Story','Event')
            pietitle = 'To-be-remembered information'} 
            
  {source(file.path(funcon_path, "PN_GetPalette.R"))}
  
  # ##########################################################################################################################
  # Read data set
  reviewtable <- read_excel(file.path(data_path,"Lit_Review_Preprocessed.xlsx")) %>%
    mutate(Article_ID = str_extract_all(Entry_ID, "\\w+(?=,)"), .before = Entry_ID)
  
  nArticle <- reviewtable$Article_ID %>% unique() %>% length()
  nEntry <- reviewtable$Entry_ID %>% unique() %>% length()
  
  reviewtable <- reviewtable %>%
    # mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_")) %>%
    select(Title, Entry_ID, whatvar)
  
  
  # ##########################################################################################################################
  # by entry and by article
  # ----------------------------------------------
  filter_string = paste0('`',whatvar,'`', ">0")
  # Entry
  
  dfEntry <- reviewtable %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>% 
    summarise(n = n(),
              freq = n() / nEntry) %>%
    arrange(vars(whatvar)) %>% 
    mutate(Variable = whatvar,
           dflabel = sprintf("%4d %s", n, sprintf("%5s", paste0("(",scales::percent(freq),")")))) %>%
    rename("Levels" = whatvar)
  dfEntry$Levels = factor(dfEntry$Levels, unique(dfEntry$Levels))
  
  
  dfArticle <- reviewtable %>%
    distinct_at(vars("Title",whatvar)) %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>%
    summarise(n = n(),
              freq = n() / nArticle) %>%
    arrange(vars(whatvar)) %>%
    mutate(Variable = whatvar,
           dflabel = sprintf("%4d %s", n, sprintf("%5s", paste0("(",scales::percent(freq),")")))) %>%
    rename("Levels" = whatvar)
  dfArticle$Levels = factor(dfArticle$Levels, unique(dfArticle$Levels))
  
 # Merge 
  df <- merge(dfEntry, dfArticle, by = c("Variable","Levels"), suffix = c(".Entry",".Article")) %>%
    mutate(dflabel = sprintf('%s: %s/ %s', Levels, dflabel.Entry, dflabel.Article) %>% as.factor())
  
  p<- ggplot(df, aes(x = '', y=n.Entry, fill = dflabel))+  
    geom_bar(width = 1, stat="identity")+  
    coord_polar(theta = "y", start = 4*pi/2-.5, direction = -1)+
    theme_void()+ 
    labs(title = pietitle)+
    guides(fill = guide_legend(nrow=nrow(df), byrow=TRUE))+
    scale_fill_manual('',values=projectPalette[1:nrow(df)],
                      labels = pielabels) +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 20,hjust = 0.5,face="bold"),
          strip.text = element_text(size = 15), 
          legend.text=element_text(size=15,face="bold"), legend.title=element_text(size=15),
          legend.position = "bottom")
  ggsave(filename = file.path(result_path, sprintf("Pie_%s.pdf", whatvar)), p, width = 634, height = 634, units = "px", dpi = "screen")
  msg_box(c('Create ', file.path(result_path, sprintf("Pie_%s.pdf", whatvar))))
  
  p

  
  
}

