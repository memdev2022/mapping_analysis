# This function takes the non-binary dataset and a variable of interest and produces pie chart

PN_MakePie <- function(reviewtable=reviewtable, whatvar = 'Task_type/Encoding_instruction/Design/To_be_remembered_information'){
  
  
  # Read data set
  nArticle <- reviewtable$Article_ID %>% unique() %>% length()
  nEntry <- reviewtable$Entry_ID %>% unique() %>% length()
  
  reviewtable <- reviewtable %>%
    # mutate(Entry_ID = paste(Article_ID, Task_number, sep = "_")) %>%
    select(Title, Entry_ID, whatvar)
  
  filter_string = paste0(whatvar, ">0")
  
  # Entry
  dfEntry <- reviewtable %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>% 
    summarise(n = n(), freq = n() / nEntry) %>%
    arrange(vars(whatvar)) %>% 
    mutate(Variable = whatvar) %>%
    rename("Levels" = whatvar)
  dfEntry$Levels = factor(dfEntry$Levels, c(1:6))
  
  
  dfArticle <- reviewtable %>%
    distinct_at(vars("Title",whatvar)) %>%
    filter(!! rlang::parse_expr(filter_string)) %>%
    group_by_at(vars(whatvar)) %>%
    summarise(n = n(),freq = n() / nArticle) %>%
    arrange(vars(whatvar)) %>%
    mutate(Variable = whatvar) %>%
    rename("Levels" = whatvar)
  dfArticle$Levels = factor(dfArticle$Levels, c(1:6))
  
  # Merge 
  df <- merge(dfEntry, dfArticle, by = c("Variable","Levels"), suffix = c(".Entry",".Article"))
  
  # Make pie
  # legend text and pie title:
  if (whatvar=='Task_type'){
    pielabels =c('Direct memory','Autobiographical memory','Statistical learning',
                 'Generalization','Semantic knowledge','')
    pietitle = 'Task type'} else
      
      if (whatvar=='Encoding_instruction'){
        pielabels= c('Intentional','Incidental', 'Manipulated: intention and incidental',
                     'Unspecified', 'NA, for personal events or semantic knowledge tasks')
        pietitle = 'Encoding instruction'} else
          
          if (whatvar=='Design'){
            pielabels=c('Cross-sectional','Longitudinal','','','','')
            pietitle = 'Design'
          } else
            
            if (whatvar=='To_be_remembered_information'){
              pielabels=c('Individual item','Associative co-occurence','Temporal memory',
                          'What-where-when','Story','Event')
              pietitle = 'To-be-remembered information'}
  
  p<- ggplot(df, aes(x = '', y=n.Entry, fill = Levels))+  
    geom_bar(width = 1, stat="identity")+  
    coord_polar(theta = "y", start = 4*pi/2-.5, direction = -1)+
    theme_void()+ 
    labs(title = pietitle)+
    guides(fill = guide_legend(ncol=1, byrow=TRUE))+
    scale_fill_manual('',values=c(projectPalette[1:nrow(df)],rep('#ffffff',6-nrow(df))),drop=F,
                      labels = pielabels) +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 20,hjust = 0.5,face="bold"),
          strip.text = element_text(size = 15), 
          legend.text=element_text(size=15,face="bold"), legend.title=element_text(size=15),
          legend.position = "bottom")
  
  return(p)
  
  
}


PN_MakePie(reviewtable,'Design')


ggdraw()+
  draw_plot(PN_MakePie(reviewtable,'Design'), x = 0, y = 0, width = 1/3, height = 1) +
  draw_plot(PN_MakePie(reviewtable,'Task_type'), x = 1/3, y = 0, width = 1/3, height = 1)+
  draw_plot(PN_MakePie(reviewtable,'To_be_remembered_information'), x = 2/3, y = 0, width = 1/3, height = 1)


plot_grid(PN_MakePie(reviewtable,'Design'), PN_MakePie(reviewtable,'Task_type'),PN_MakePie(reviewtable,'To_be_remembered_information'),ncol=3, align = "v")
