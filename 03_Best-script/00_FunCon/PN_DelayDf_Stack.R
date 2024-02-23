PN_DelayDf_Stack <- function(data = reviewtable, split.interval = ' - '){
  
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
    drop_na(Delay1a, Delay1b) 
  # Count the total number of entries
  NEntry <- nrow(DelaySplit2)
  
  DelaySplit2 <- DelaySplit2 %>%
    arrange(Task_type, Entry_ID) %>%
    # assign position to each entry
    mutate(id = 1:NEntry)%>%
    # calculate the position of each entry on the plot along the x-axis
    mutate(Y = case_when(max(id) == 1 ~ 2520,
                         max(id) != 1 ~ ((id-1)*(5040/(NEntry-1)))))
    
  
  
  delay0 <- melt(DelaySplit2, id.vars = c('id', 'Entry_ID','Task_type', 'Y'),
                          # value.name >> X = position of entries along the X-axis
                          value.name = "X") %>%
    arrange(id) %>%
    # the digit indicates which delay period it is:
    mutate(ID_Delay = paste0(Entry_ID, str_extract(variable, "\\d"))) %>%
    filter(
      # Some delays were coded as -999, remove these:
      variable == 'Delay6a' & X > 0| variable == 'Delay6b' & X > 0|
        variable == 'Delay5a' & X > 0| variable == 'Delay5b' & X > 0|
        variable == 'Delay4a' & X > 0| variable == 'Delay4b' & X > 0|
        variable == 'Delay3a' & X > 0| variable == 'Delay3b' & X > 0|
        variable == 'Delay2a' & X > 0| variable == 'Delay2b' & X > 0|
        variable == 'Delay1a'| variable == 'Delay1b') %>%
    mutate(variable = str_sub(variable,-1,-1))
  
  delay1 <- reshape(delay0, idvar = c('id','Entry_ID','Task_type','Y','ID_Delay'),
                    timevar = 'variable', direction = 'wide')
  delay1 <- delay1 %>%
    arrange(desc(id)) %>%
    mutate(text = case_when(X.a==X.b ~ paste0(Entry_ID, '\n Task type: ',Task_type,'\n Delay: ', X.a),
                            X.a!=X.b ~ paste0(Entry_ID, '\n Task type: ',Task_type,'\n Delay: ', X.a, '-', X.b)))
  
  return(delay1)
  
}