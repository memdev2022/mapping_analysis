# This function binarize a complex data frame
PN_BinarizeDataset <- function(data, identifier_var) {
  
  
  # Define variable types ----
  # .................................................................
  # character var                  numerical
  #                         /-----------------------\
  #                     categorical             continuos
  #                  /--------------\             (?)
  #               binary          nonbinary
  #           (e.g., age)             (?)
  
  
  # Define variable types that could exist in a df
  character_var <- NULL #done
  numerical_var <- NULL #done
  categorical_var <- NULL #done
  binary_var <- NULL #done
  nonbinary_var <- NULL #done
  continuous_var <- NULL #done
  
  
  ## character var ----
  #Find the character data type
  character_var <- data %>% 
    select(where(is.character)) %>% 
    names()
  
  ## numerical var ----
  #Find the numerical data type: numerical_var
  df.numerical <- data %>% select(where(is.numeric))
  df.numerical <- lapply(df.numerical, factor) %>%
    as_tibble() %>%
    as.data.frame()
  numerical_var <- names(data %>% select(where(is.numeric)))
  
  ## categorical var ----
  #Find the categorical cols: categorical_var
  nlevel.var <- sapply(df.numerical[,sapply(df.numerical, is.factor)], nlevels)
  nlevel.var %>% table() #to see the distribution of nlevels in the df
  is.categorical <- function(variable) {
    levels.variable <- unique(variable)
    length(levels.variable) - sum(is.na(levels.variable)) <= 20L #categorical vars are defined as having max. 20 levels
    }
  categorical_var <- names(df.numerical %>% select(where(is.categorical)))
    
  ### binary var ----
  #Find columns that are binary variables: binary_var
  is.binary <- function(variable) {
    levels.variable <- unique(variable)
    length(levels.variable) - sum(is.na(levels.variable)) == 2L && all(levels.variable %in% c(1,0,NA))
    }
  binary_var <- names(data %>% select(where(is.binary)))
  
  ### non-binary var ----
  #Find the non-binary, categorical cols (to be binarized): nonbinary_var
  nonbinary_var <- setdiff(categorical_var, binary_var)
  
  rm(df.numerical, nlevel.var)
  
  ## continuous var ----
  #Find columns that are continuous variables: continuous_var
  continuous_var <- setdiff(numerical_var, categorical_var)
  
  # Make data frame with indentifier columns and columns of binary vars ----
  ### Step 1 ----   
  #### Create one main "healthy" data frame and one separate data frame for the problematic variable
  #### healthy df includes identifier var, some continuous var (e.g., authors' name), and binary vars
  df_main <- data %>% #healthy df
    select(identifier_var, continuous_var, binary_var) 
  
  #### the problematic df includes nonbinary vars which we want to binarize 
  nonbinary_group <- nonbinary_var %>% # nonbinary groups are the supercategory (e.g., "Modality" is the supercategory of its subcategory Modality1 visual, Modality2 auditory) 
    str_extract("\\D+")%>%
    # str_extract("[A-aZ-z]+") %>%
    unique()
  
  for (i in 1:length(nonbinary_group)){
    nonbinary.group = nonbinary_group[i]
    nonbinary.var = nonbinary_var[startsWith(nonbinary_var, nonbinary.group)]
    df <- data%>% #problematic df
      select(identifier_var, nonbinary.var)
    
    ### Step 2 ----
    #### Create new columns that contain all possible levels
    df1 <- melt(df,id= identifier_var, value.name = nonbinary.group)
    
    df2 <- df1 %>%
      dummy_cols(select_columns = nonbinary.group, remove_selected_columns = T, ignore_na = T)
    
    ### Step 3 ----
    #### Reduce rows to complete binarization
    df3 <- df2%>%
      select(-variable)%>%
      group_by(.dots = identifier_var)%>%
      # summarise_each(funs(sum(., na.rm = TRUE)))
      summarise(across(everything(), ~sum(., na.rm = T)))
    
    ### Step 4 ----
    #### Merge the data frame of the newly binarized variable with the main data frame
    df_main <- merge(df_main,df3, by = identifier_var)
    
    
  }
  
  df_main[df_main >=1 & df_main <= 1000] <- 1 # if a node is repeated >> transform to binary data by recode into 1 
  return(df_main)
  
}
