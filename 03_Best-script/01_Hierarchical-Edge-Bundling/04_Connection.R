# This script gets connections and detect overlapping connections
# Note: This script consider only the complete dataset
rm(list=ls())

directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping analysis')
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# Step 0. Load necessary functions ====
source(file.path(funcon_path, "PN_GetConnection.R"))
source(file.path(funcon_path, "PN_GetConnection_OverlapOrUniqueLabel.R"))

# Get dataset (must be binarized)
df <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx"))

identifier_var <- c("Entry_ID", "Authors","Title", "Year")


# Step 5. Create connections ====

# get connections
connect <- PN_GetConnection(data=df, identifier_var = identifier_var)
# look for overlapping edges
connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(connect)

# Step 6. Check which connections are not in the data
nodenames = setdiff(names(df),identifier_var)
allconnect = expand.grid(to=nodenames,from=nodenames)%>%
  filter(from!=to)%>%
  arrange(from,to) %>% select(from,to)
checkallconnect=left_join(allconnect,connect_OverlapOrUnique,by=c('from','to'))%>%
  mutate(Entry_ID=case_when(is.na(Entry_ID) ~ 'non-existent',
                            !is.na(Entry_ID) ~ Entry_ID),
         Proportion=case_when(is.na(Proportion) ~ 0,
                            !is.na(Proportion) ~ Proportion))

{
  write_xlsx(connect, file.path(data_path, "Lit_Review_Connect.xlsx")) 
  message('Create ', file.path(data_path, "Lit_Review_Connect.xlsx"))
  
  write_xlsx(connect, file.path(descriptives_path, "Lit_Review_Connect.xlsx")) 
  message('Create ', file.path(descriptives_path, "Lit_Review_Connect.xlsx"))
  
  write_xlsx(connect_OverlapOrUnique, file.path(data_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  message('Create ', file.path(data_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  
  write_xlsx(connect_OverlapOrUnique, file.path(descriptives_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  message('Create ', file.path(descriptives_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  
  

}

