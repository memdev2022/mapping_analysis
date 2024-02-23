# This function detects connections
PN_GetConnection <- function(data, identifier_var, identifier_row = "Entry_ID") {
  
  # Row-wise operation: bind cells = 1 with each other, unique connections (if a >> b exists, no b >> a)
  # data = data frame in binarized format
  # identifier_var = vars that are not used to build connections
  
  connect <- data.frame()
  
  for (i in 1:nrow(data)){
    row.df <- data[i,]
    row.leaves <- names(row.df)[row.df>0] #select only nodes that are available
    row.leaves <- setdiff(row.leaves, identifier_var) #drop identifier vars
    
    row.Entry_ID <- row.df %>% select(all_of(identifier_row)) %>% pull()
    
    
    if (length(row.leaves)>1) {
      # if more than 1 leaf is activated, the bundle is connected between nodes
      row.combi <- row.leaves%>%
        combn(2, simplify = T) %>%
        t() %>%
        as.data.frame()
      
      row.connect <- data.frame(Entry_ID = row.Entry_ID, from = row.combi$V1, to = row.combi$V2)
    }
    
    else {
      # if no or only one leaf is activated, then bundle is a horizontal line.
      # this step checks whether there is any such article
      # Connections from a node to itself does not enter the dataframe "connect"
      row.connect.single <- data.frame(from = row.leaves, to = row.leaves) 
    }
    
    connect <- rbind(connect, row.connect)
    
  }
  
  return(connect)
 
  
}


