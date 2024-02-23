rm(list=ls())
# This script produces HEB with connections representing the trend of all entries in the QC sheet
directory = dirname(getSourceEditorContext()$path)%>% str_extract(pattern = '^(.*?)mapping analysis')

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
  
  {source(file.path(funcon_path, "PN_GetConnection.R"))}
  {source(file.path(funcon_path, "PN_GetConnection_OverlapOrUniqueLabel.R"))}
  
  {source(file.path(funcon_path, "PN_connect_list.R"))}
  
  {source(file.path(funcon_path, "PN_GetPalette.R"))}
  {source(file.path(funcon_path, "PN_PlotNode.R"))}
  {source(file.path(funcon_path, "PN_PlotEdge_Group.R"))}
}
# Work flow ====
# step 00. load necessary packages
# step 1, 2. ignore if these files are available: Lit_Review_binarized.xlsx, Hierarchy.slsx, Vertices.xlsx
# step 3. Create plots of all nodes with hierarchy and vertices
# step 4. Filter data frame (ignore if not necessary)
# step 5. Create connections (aka. edges)
# step 5. Plot connection


# Load necessary packages ----
source(local = FALSE,echo = TRUE,print.eval = TRUE,verbose = FALSE,
       file= file.path(directory, "03_Best-script", "00_Data-Preprocessing", "00_Script_Packages.R"))

# Step 3. Create Hierarchical Edge plot ====
# Create plots of all nodes with hierarchy and vertices 
# Extras: Customize labels for all nodes

# determine identifier variables
identifier_var <- c("Entry_ID", "Authors","Title", "Year")

# retrieve hierarchy and edge
hierarchy <- read_excel(file.path(data_path,"Hierarchy.xlsx"))
vertices <- read_excel(file.path(data_path,"Vertices.xlsx"))
vertices$group <- factor(vertices$group, levels = vertices$group %>% unique())


if (interactive() ) {
  
  
  # Step 4. Get dataframe ====
  # Get data frame
  df <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx")) %>% PN_FilterEntry(version='trend')
  
  
  # Step 5. Make connections (or get connection dfs directly from data folder)
  
  # get connections
  connect <- PN_GetConnection(data=df, identifier_var = identifier_var)
  # look for overlapping edges
  connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(connect)

  # Check: How many edges that appeared in only one entry (appeared only once in the whole literature)?===
  how_many_edge <- connect_OverlapOrUnique %>% 
    group_by(Entry) %>%
    summarise(count = n())
  how_many_edge[how_many_edge$Entry == 1,] #e.g., 408 edges that appeared in only one entry
  
  # Check the distribution: 
  n_entry <- connect$Entry_ID %>% unique() %>% length()
  ggplot(connect_OverlapOrUnique)+
    geom_histogram(aes(x = Entry), binwidth = 1, fill = "darkblue")+
    # ylim(0, 400)+
    labs(y = "Number of edges", x = "Number of entries/edge", subtitle = paste("In total:", n_entry,"entries"))
  
  
  
  # Step 6. Plot ====
  
  # 6.1. Draw nodes ---- 
  # Nodes vary in size and transparency according to the entry size
  # Idea: the size of a node depends on how many papers light up that node
  
  
  # In the data frame "vertices", create the column "Entry" that represents the size of each node
  value.df <- data.frame(
    # drop identifier vars
    name = setdiff(names(df), identifier_var),
    # Entry=the number of Entrys (sum of 1s) that light up each node
    nEntry = as.numeric(colSums(df %>% select(-identifier_var)))) 
  
  vertices <- left_join(vertices, value.df, by = "name")
  
  # draw plot
  mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)
  size.breaks = seq.int(0, max(vertices$nEntry, na.rm = T), length.out=5) %>% round()
  p <- PN_PlotNode(mygraph=mygraph, size.breaks = size.breaks)
  p
  
  # 6.2. Draw edges ----
  
  # Drawing 3000+ edges is unproductive 
  # idea 1 >> we draw only, for example, 500 most trendy edges
  connect.clean <- connect_OverlapOrUnique %>%
    arrange(desc(Proportion)) %>%
    head(500)%>%
    arrange(Proportion)
  
  #idea 2 >> we draw only edges, each of which has at least 20% of the entries
  # v1
  connect.clean <- connect_OverlapOrUnique %>%
    filter(Proportion >= 20) %>%
    arrange(Proportion)
  
  from.clean <- match(connect.clean$from, vertices$name)
  to.clean <- match(connect.clean$to, vertices$name)
  my_alpha.clean <- connect.clean$Proportion
  
  # version 2 (09112023): adapted for manuscript
  connect.clean <- connect_OverlapOrUnique %>%
    filter(Proportion >= 20) %>%
    mutate(from.match.vertice=match(from,vertices$name),
           to.match.vertice=match(to,vertices$name))
  connect.clean.list=PN_connect_list(connect.clean)
  
  # p4: edges displayed as heatmap
  # Define category breaks
  (p4 <- PN_PlotEdge_Group(p, edge.from = from.clean, edge.to = to.clean, edge.color = my_alpha.clean))
  (p4 <- PN_PlotEdge_Group_2(p, connectlist=connect.clean.list))
  p4
  
  
  
  
  
  {
    # All Edges
    # version 1 
    connect.all <- connect_OverlapOrUnique %>%
      arrange(Proportion)
    
    from.all <- match(connect.all$from, vertices$name)
    to.all <- match(connect.all$to, vertices$name)
    my_alpha.all <- connect.all$Proportion
    
    # version 2 (09112023): adapted for manuscript
    connect.all <- connect_OverlapOrUnique %>%
      arrange(Proportion)%>%
      mutate(from.match.vertice=match(from,vertices$name),
             to.match.vertice=match(to,vertices$name))
    connect.all.list=PN_connect_list(connect.all)
    
    #p5: edges displayed as heatmap
    (p5 <- PN_PlotEdge_Group(p, edge.from = from.all, edge.to = to.all, edge.color = my_alpha.all))
    (p5 <- PN_PlotEdge_Group_2(p, connectlist=connect.all.list))
  }
  
  # SAVE DATA ====
  
  # plotNames = readline('How to name the plots >>> ')
  plotNames = dlg_input("How to name the plots \n\n Number1", default='pattern_separation_entries')$res
  
  
  {
    write_xlsx(connect_OverlapOrUnique, file.path(descriptives_path, sprintf("Lit_Review_connect_OverlapOrUnique_%s.xlsx", plotNames)))
    message('Create ', file.path(descriptives_path, sprintf("Lit_Review_connect_OverlapOrUnique_%s.xlsx", plotNames)))
    
  }
  
  
  # Save plot
  ggsave(filename = file.path(result_path, sprintf("HEB_AllEdges_%s.pdf", plotNames)), p5, width = 1228, height = 634, units = "px", dpi = "screen")
  msg_box(c('Create ', file.path(result_path, sprintf("HEB_AllEdges_%s.pdf", plotNames)))) #message('Create ', file.path(result_path, sprintf("HEB_AllEdges_%s.pdf", plotNames)))
  
  
  ggsave(filename = file.path(result_path, sprintf("HEB_Top20Edges_%s.pdf", plotNames)), p4, width = 1228, height = 634, units = "px", dpi = "screen")
  msg_box(c('Create ', file.path(result_path, sprintf("HEB_Top20Edges_%s.pdf", plotNames)))) #message('Create ', file.path(result_path, sprintf("HEB_Top20Edges_%s.pdf", plotNames)))
  
  
  
  
  }