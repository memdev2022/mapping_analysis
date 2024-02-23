# Run script 03-04 before this!
# This script produces HEB with connections of individual selected entries 
rm(list=ls())

directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping_analysis')
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}


{
  {source(file.path(funcon_path, "PN_FilterEntry.R"))}
  {source(file.path(funcon_path, "PN_GetPalette.R"))}
  {source(file.path(funcon_path, "PN_GetConnection.R"))}
  {source(file.path(funcon_path, "PN_GetConnection_OverlapOrUniqueLabel.R"))}
  
  {source(file.path(funcon_path, "PN_connect_list.R"))}

  {source(file.path(funcon_path, "PN_PlotNode.R"))}
  {source(file.path(funcon_path, "PN_PlotEdge_Filter.R"))}
  {source(file.path(funcon_path, "PN_PlotEdge_Group.R"))}
}


# Step 3. Create plots of all nodes with hierarchy and vertices ====
# Extras: Customize labels for all nodes
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(script_path,"03_Hierarchy-Vertices-Plot.R"))


#Step 4. Filter rows by some criteria ====
df.filtered = read_excel(file.path(data_path,"Lit_Review_binarized.xlsx")) %>% PN_FilterEntry(version='compare')

identifier_var <- c("Entry_ID", "Authors","Title", "Year")

value.df <- data.frame(
  # drop identifier vars
  name = setdiff(names(df.filtered), identifier_var),
  # nEntry=the number of Entrys (sum of 1s) that light up each node
  nEntry = as.numeric(colSums(df.filtered %>% select(-identifier_var)))) 

vertices <- left_join(vertices, value.df, by = "name")


# Step 5. Create connections ====

# get connections
connect <- PN_GetConnection(data=df.filtered, identifier_var = identifier_var)

# look for overlapping edges
connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(connect)%>%
  mutate(from.match.vertice=match(from,vertices$name),
         to.match.vertice=match(to,vertices$name))

# v1
from <- match(connect_OverlapOrUnique$from, vertices$name)
to <- match(connect_OverlapOrUnique$to, vertices$name)
mylevel = c(unique(connect$Entry_ID), 'overlap')
Entry_ID <- connect_OverlapOrUnique$Entry_ID %>% factor(levels = mylevel)

mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)

size.breaks = seq.int(0, max(vertices$nEntry, na.rm = T), length.out=5) %>% round()
p <- PN_PlotNode(mygraph=mygraph)

(p1 <- PN_PlotEdge_Filter(p, edge.from = from, edge.to = to, edge.color = Entry_ID))

entry <- df.filtered$Entry_ID %>% paste(collapse = "-")

# v2: 091123 - Adapt for manuscript and change in R version
if (length(unique(connect$Entry_ID))>1) {entry_ids=c(unique(connect$Entry_ID), 'overlap')} else 
  if (length(unique(connect$Entry_ID))==1){entry_ids=unique(connect$Entry_ID)}

connect_OverlapOrUnique_list = PN_connect_list_2(connecttable = connect_OverlapOrUnique,entry_ids=entry_ids)
(p1 <- PN_PlotEdge_Filter_2(p, connectlist=connect_OverlapOrUnique_list,edge.from = from, edge.to = to, 
                            # edge.color = c(unique(connect$Entry_ID), 'overlap'),
                            edge.color = entry_ids
                            )
  
  )

# Save plot
ggsave(filename = file.path(result_path, sprintf("HEB_%s.pdf", entry)), p1, width = 1228, height = 634, units = "px", dpi = "screen")
msg_box(c('Create ', file.path(result_path, sprintf("HEB_%s.pdf", entry)))) #message('Create ', file.path(result_path, sprintf("HEB_%s.pdf", entry)))
write_xlsx(connect, file.path(descriptives_path, sprintf("Lit_Review_Connect_%s.xlsx", entry)))
msg_box(c('Create ', file.path(result_path, sprintf("Lit_Review_Connect_%s.xlsx", entry)))) # message('Create ', file.path(descriptives_path, sprintf("Lit_Review_Connect_%s.xlsx", entry)))


