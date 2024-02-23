# This function detects connections and assign labels 
PN_GetConnection_OverlapOrUniqueLabel <- function(connect) {
 
  # look for overlapping edges
  n_entry <- connect$Entry_ID %>% unique() %>% length()
  
  connect_Count <- connect %>%
    group_by(from, to) %>%
    summarise(Entry = n_distinct(Entry_ID)) %>%
    ungroup() %>%
    # calculate the proportion of entries of each edge: Proportion
    mutate(Proportion = (Entry/n_entry)*100) %>%
    #arrange in ascending order, the weakest edge will be drawn first, the most prevalent edge is drawn on top
    arrange(Proportion) 
  
  
  df_connect <- inner_join(connect, connect_Count, by = c("from", "to")) %>%
    mutate(Entry_ID = case_when(Entry > 1 ~ "overlap",
                                Entry <= 1 ~ Entry_ID)) %>%
    arrange(Entry_ID, from, to)
  
  mylevels = c(setdiff(df_connect$Entry_ID %>% unique(), "overlap"), "overlap")
  
  connect_OverlapOrUnique <- df_connect %>%
    distinct() %>%
    arrange(match(Entry_ID, mylevels))
  
  
  connect_OverlapOrUnique$Entry_ID <- factor(connect_OverlapOrUnique$Entry_ID, levels=mylevels)
    
  
  return(connect_OverlapOrUnique)
  
  
}


