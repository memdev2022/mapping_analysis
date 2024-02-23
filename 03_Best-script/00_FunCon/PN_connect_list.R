PN_connect_list <- function(connecttable,limitrange = c(0,20,40,60,80,100)){
  
  # prepare a list of lists
  # each sublist contains pairs of start-nodes (from, vec1) and end-nodes (to,vec2) that produce proportion values 
  # in the range of 
  # [0-20], [20-40], [40-60], [60-80], [80-100]
  connect.list = list()
  
  for (i in 1:(length(limitrange)-1)){
    
    current.connecttable = connecttable %>% 
      filter(Proportion>limitrange[i] & Proportion<=limitrange[i+1])
    current.from = current.connecttable$from.match.vertice
    current.to = current.connecttable$to.match.vertice
    if (length(current.from)==0){
      current.from=NA
      current.to=NA
    }
    
    connect.list[[i]] = list(from=current.from,to=current.to)
    
  }
  return(connect.list)
}

PN_connect_list_2 <- function(connecttable,entry_ids){
  
  # prepare a list of lists
  # each sublist contains pairs of start-nodes (from, vec1) and end-nodes (to,vec2) in different color based on
  # one of the 3 types of entry_id
  # e.g. paper 1, paper 2, overlap
  connect.list = list()
  
  for (i in 1:3){
    
    current.connecttable = connecttable %>% 
      filter(Entry_ID==entry_ids[i])
    current.from = current.connecttable$from.match.vertice
    current.to = current.connecttable$to.match.vertice
    if (length(current.from)==0){
      current.from=NA
      current.to=NA
    }
    
    connect.list[[i]] = list(from=current.from,to=current.to)
    
  }
  connect.list[[4]] = unique(connecttable$Entry_ID)
  return(connect.list)
}
