# This function produces a data frame for all data collection location
PN_CountLocation <- function(data = data) {
  
  # Read data set
  data <- data %>%
    transmute(Article_ID,
              Entry_ID = paste0(Article_ID,", ", Task_number),
              Locations = str_split(`Data collection location`, "; ")) %>%
    rowwise()%>%
    mutate(l1 = unlist(Locations)[1],
           l2 = unlist(Locations)[2],
           l3 = unlist(Locations)[3],
           l4 = unlist(Locations)[4],
           l5 = unlist(Locations)[5],
           l6 = unlist(Locations)[6]) %>%
    select(-Locations)
  
  nEntry = data$Entry_ID %>% unique() %>% length()
  nArticle = data$Article_ID %>% unique() %>% length()
  
  data.long <- melt(data, id.vars = c('Article_ID','Entry_ID'), value.name = 'Location') %>%
    mutate(Location = recode(Location, 
                             `UK` = "United Kingdom", `US` = "United States of America"))
  
  data.long$Location[data.long$Location %in% c('na','-999.0')] = NA
  data.long$Location %>% unique()
  data.long <- data.long %>% tidyr::drop_na(Location)
  
  
  # Summarize country stats by entry and by paper
  dfEntry <- data.long %>%
    distinct(Entry_ID, Location) %>%
    group_by(Location) %>%
    summarise(n = n(), freq = n()/nEntry)%>%
    mutate(Total = nEntry) %>%
    arrange(desc(freq))
  
  
  dfArticle <- data.long %>%
    distinct(Article_ID, Location) %>%
    group_by(Location) %>%
    summarise(n = n(), freq = n()/nArticle)%>%
    mutate(Total = nArticle) %>%
    arrange(desc(freq))
  
  df <- merge(dfEntry, dfArticle, by = "Location", suffixes = c('.Entry','.Article'))%>%
    arrange(desc(n.Article))
  
  return(df)

}