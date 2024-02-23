PN_DelayDf <- function(data = reviewtable, split.interval = ' - '){
  
  
  
  
  # Syntax for period: time - time >> split
  DelaySplit <- data %>%
    transmute(Entry_ID = Entry_ID %>% as.factor(),
              Task_type = Task_type %>% as.factor(),
              Delay1a = str_split_fixed(Delay_1, split.interval, 2)[,1], Delay1b = str_split_fixed(Delay_1, split.interval, 2)[,2],
              Delay2a = str_split_fixed(Delay_2, split.interval, 2)[,1], Delay2b = str_split_fixed(Delay_2, split.interval, 2)[,2],
              Delay3a = str_split_fixed(Delay_3, split.interval, 2)[,1], Delay3b = str_split_fixed(Delay_3, split.interval, 2)[,2],
              Delay4a = str_split_fixed(Delay_4, split.interval, 2)[,1], Delay4b = str_split_fixed(Delay_4, split.interval, 2)[,2],
              Delay5a = str_split_fixed(Delay_5, split.interval, 2)[,1], Delay5b = str_split_fixed(Delay_5, split.interval, 2)[,2],
              Delay6a = str_split_fixed(Delay_6, split.interval, 2)[,1], Delay6b = str_split_fixed(Delay_6, split.interval, 2)[,2])
  
  DelaySplit2 <- DelaySplit %>%
    # '100 - 1000000' will be splitted into '100' and '1000000' >> Delay1a = 100, Delay1b = 1000000
    # '100' will be splitted into '100' and '' >> Delay1a = Delay1b = 100
    mutate(Delay1b = case_when(Delay1b != '' ~ Delay1b, Delay1b == '' ~ Delay1a),
           Delay2b = case_when(Delay2b != '' ~ Delay2b, Delay2b == '' ~ Delay2a),
           Delay3b = case_when(Delay3b != '' ~ Delay3b, Delay3b == '' ~ Delay3a),
           Delay4b = case_when(Delay4b != '' ~ Delay4b, Delay4b == '' ~ Delay4a),
           Delay5b = case_when(Delay5b != '' ~ Delay5b, Delay5b == '' ~ Delay5a),
           Delay6b = case_when(Delay6b != '' ~ Delay6b, Delay6b == '' ~ Delay6a)) %>%
    mutate_if(is.character, as.numeric) %>%
    # after mutation, 'unspecified'-delay will become NA >> drop them:
    tidyr::drop_na(Delay1a, Delay1b)
  
  # Find the maximum delay among all entries
  MaxDelay <- DelaySplit2 %>%
    select(starts_with('Delay'))%>%
    max(na.rm = T)
  
  # Count the total number of entries
  NEntry <- nrow(DelaySplit2)
  
  
  DelaySplit2 <- DelaySplit2 %>%
    arrange(Task_type, Entry_ID) %>%
    # assign position to each entry
    mutate(id = 1:NEntry)%>%
    # calculate the position of each entry on the plot along the x-axis
    mutate(pos1 = (id-1)*(MaxDelay/(NEntry-1)),
           pos2 = (id-1)*(MaxDelay/(NEntry-1)),
           pos3 = (id-1)*(MaxDelay/(NEntry-1)),
           pos4 = (id-1)*(MaxDelay/(NEntry-1)),
           pos5 = (id-1)*(MaxDelay/(NEntry-1)),
           pos6 = (id-1)*(MaxDelay/(NEntry-1))) %>%
    # add text for plotly
    mutate(TextDelay1 = case_when(Delay1a == Delay1b ~ as.character(Delay1a), Delay1a != Delay1b ~ paste0(Delay1a,' - ', Delay1b)),
           TextDelay2 = case_when(Delay2a == Delay2b ~ as.character(Delay2a), Delay2a != Delay2b ~ paste0(Delay2a,' - ', Delay2b)),
           TextDelay3 = case_when(Delay3a == Delay3b ~ as.character(Delay3a), Delay3a != Delay3b ~ paste0(Delay3a,' - ', Delay3b)),
           TextDelay4 = case_when(Delay4a == Delay4b ~ as.character(Delay4a), Delay4a != Delay4b ~ paste0(Delay4a,' - ', Delay4b)),
           TextDelay5 = case_when(Delay5a == Delay5b ~ as.character(Delay5a), Delay5a != Delay5b ~ paste0(Delay5a,' - ', Delay5b)),
           TextDelay6 = case_when(Delay6a == Delay6b ~ as.character(Delay6a), Delay6a != Delay6b ~ paste0(Delay6a,' - ', Delay6b))) %>%
    mutate(nodeText = paste(TextDelay1, TextDelay2, TextDelay3, TextDelay4, TextDelay5, TextDelay6, sep = ', ')) %>% 
    select(-c(TextDelay1, TextDelay2, TextDelay3, TextDelay4, TextDelay5, TextDelay6)) %>%
    mutate(nodeText =  str_split_fixed(nodeText, ", -999",2)[,1]) %>%
    mutate(nodeText = paste0(Entry_ID, '\n Task type: ',Task_type,'\n Delay: ', nodeText))
  
  
  delay <- reshape2::melt(DelaySplit2, id.vars = c('id', 'Entry_ID','Task_type', 'nodeText'),
                          # value.name >> X = position of entries along the X-axis
                          value.name = "X") %>%
    arrange(id) %>%
    # the digit indicates which delay period it is:
    mutate(ID_Delay = paste0(Entry_ID, str_extract(variable, "\\d")))
  
  pos = c('pos1','pos2','pos3', 'pos4','pos5','pos6')
  
  delay0 <- delay %>%
    filter(
      # Some delays were coded as -999, remove these:
      variable == 'Delay6a' & X > 0| variable == 'Delay6b' & X > 0|
        variable == 'Delay5a' & X > 0| variable == 'Delay5b' & X > 0|
        variable == 'Delay4a' & X > 0| variable == 'Delay4b' & X > 0|
        variable == 'Delay3a' & X > 0| variable == 'Delay3b' & X > 0|
        variable == 'Delay2a' & X > 0| variable == 'Delay2b' & X > 0|
        variable == 'Delay1a'| variable == 'Delay1b'| variable %in% pos) %>%
    # calculate the positions in the y-coordinate:
    mutate(Y = case_when(variable %in% pos ~ 1, # ending position of entry >> y=1
                         variable %in% setdiff(variable,pos) ~ 0)) # corresponding delay >> y=0 (on x-axis)
  summarydelay0 <- delay0 %>%
    group_by(ID_Delay) %>%
    summarise(tab = table(ID_Delay))
  
  
  # Include ID_Delay that has 3 information Delaya, Delayb, pos
  myfilter <- summarydelay0$ID_Delay[summarydelay0$tab == 3]
  delay1 <- delay0 %>%
    filter(ID_Delay %in% myfilter)
  
  # label for timestamps
  labels <- delay1 %>%
    filter(variable %in% pos == F) %>%
    pull(X) %>% as.factor() %>% levels()
  
  # All timestamps
  sorted_labels <- paste0(sort(as.numeric(levels(delay1$X %>% as.factor()))))
  # Rescale
  # Customized: only show timestamps from 1h:
  sorted_labels_scale <- sorted_labels
  sorted_labels_scale[sorted_labels_scale %in% labels == F] <- ''
  sorted_labels_scale[sorted_labels_scale %in% c(0.1,0.133,0.25,0.5,0.75,
                                                 1.5,2,3,4,5,6,9,10,15,20,25,30,35,37,50,53,90,
                                                 660,1200,1800,2016,2040,2880,4320,5760,7200,8640,
                                                 11520,12960,24480,25920,30240,
                                                 43800,44199,161280,245280)] <- ''
  sorted_labels_scale[sorted_labels_scale %in% c(60,1440,
                                                 10080,20160,40320,129600,262800,
                                                 525600,1051200,2102400,3153600)] <- c("1h","1d",
                                                                                       "1w","2w","1m","3m","6m",
                                                                                       "1y","2y","4y","6y") 
  delay1 <- delay1 %>%
    group_by(ID_Delay) %>%
    arrange(variable)%>%
    mutate(
      # segment starts from the delay value and ends at the task entry (marked by pos/third row)
      Xstart = X, Ystart = Y,
      Xend = X[3], Yend = Y[3],
      # text for plotly
      text = ifelse(X[1]==X[2],
                    paste0(Entry_ID, '\nTask type: ',Task_type[1],'\n Delay: ',X[1]), # if delay is an amount of time
                    paste0(Entry_ID, '\nTask type: ',Task_type[1],'\n Delay: ',X[1],' - ',X[2]))) # if delay is a range of time
  delay1$X <- delay1$X %>% as.numeric() #%>% as.factor()
  delay1$Xstart <- delay1$Xstart %>% as.numeric() #%>% as.factor()
  delay1$Xend <- delay1$Xend %>% as.numeric() #%>% as.factor()
  # delay1$Y <- delay1$Y %>% as.numeric() %>% as.factor()
  # delay1$Yend <- delay1$Yend %>% as.numeric() %>% as.factor()
  # delay1$Ystart <- delay1$Ystart %>% as.numeric() %>% as.factor()
  delay1 <- delay1 %>%
    arrange(desc(id))
  
  return(list(delay1=delay1, MaxDelay=MaxDelay))
  
}