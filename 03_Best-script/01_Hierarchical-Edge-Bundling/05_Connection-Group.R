rm(list=ls())
# This script produces HEB with connections representing the trend of all entries in the QC sheet
directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping analysis')
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}
{source(file.path(funcon_path, "PN_GetPalette.R"))}
{source(file.path(funcon_path, "PN_connect_list.R"))}
{source(file.path(funcon_path, "PN_PlotNode.R"))}
{source(file.path(funcon_path, "PN_PlotEdge_Group.R"))}

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
vertices$betweengroupid <- factor(vertices$betweengroupid, levels = vertices$betweengroupid %>% unique())


# Step 4. Get dataframe ====
# Get data frame
df <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx"))


# Step 5. Make connections (or get connection dfs directly from data folder)

# retrieve edges from data file
connect <- read_xlsx(file.path(data_path,"Lit_Review_Connect.xlsx"))
connect_OverlapOrUnique <- read_xlsx(file.path(data_path,"Lit_Review_connect_OverlapOrUnique.xlsx"))

# Check: How many edges that appeared in only one entry (appeared only once in the whole literature)?
how_many_edge <- connect_OverlapOrUnique %>% 
  group_by(Entry) %>%
  summarise(count = n())
how_many_edge[how_many_edge$Entry == 1,] #e.g., 409 edges that appeared in only one entry

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

#vertices <- left_join(vertices, value.df, by = "name", all = T)
# 231103
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


#idea 2 >> we draw the top 20% of the densest internode connections
connect.clean <- connect_OverlapOrUnique %>%
  filter(Proportion >= quantile(connect_OverlapOrUnique$Proportion, 0.8))%>%
  arrange(Proportion)

# idea 2* >> we draw the bottom 20% of the densest internode connections
connect.bottom20 <- connect_OverlapOrUnique %>%
  filter(Proportion <= quantile(connect_OverlapOrUnique$Proportion, 0.2))%>%
  arrange(Proportion)

#idea 3 >> we draw only edges, each of which has at least 20% of the entries. 

# version 1 
minpercentage=20
connect.clean <- connect_OverlapOrUnique %>%
  filter(Proportion >= minpercentage) %>%
  arrange(Proportion)
from.clean <- match(connect.clean$from, vertices$name)
to.clean <- match(connect.clean$to, vertices$name)
my_alpha.clean <- connect.clean$Proportion

# version 2 (09112023): adapted for manuscript
connect.clean <- connect_OverlapOrUnique %>%
  filter(Proportion >= minpercentage) %>%
  mutate(from.match.vertice=match(from,vertices$name),
         to.match.vertice=match(to,vertices$name))
connect.clean.list=PN_connect_list(connect.clean)

# 
# # version 3 (12112) Explore the lower 20%
# connect.clean <- connect_OverlapOrUnique %>%
#   filter(Proportion < minpercentage) %>%
#   arrange(Proportion)%>%
#   mutate(from.match.vertice=match(from,vertices$name),
#          to.match.vertice=match(to,vertices$name))
# connect.clean.list_temp=PN_connect_list(connect.clean, limitrange = seq(0,20,length.out=6))
# connect.clean.list = list(connect.clean.list_temp[[5]],
#                           connect.clean.list_temp[[4]],
#                           connect.clean.list_temp[[3]],
#                           connect.clean.list_temp[[2]],
#                           connect.clean.list_temp[[1]])
# PN_PlotEdge_Group_2(p, connectlist=connect.clean.list, limitrange = seq(0,20,length.out=6))

# p4: edges displayed as heatmap
# Define category breaks
(p4 <- PN_PlotEdge_Group(p, edge.from = from.clean, edge.to = to.clean, edge.color = connect.clean$Proportion))
(p4 <- PN_PlotEdge_Group_2(p, connectlist=connect.clean.list))


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
# generate a timestamp to prevent overwriting on old files
i=Sys.time() %>% str_replace_all(pattern = "\\D", replacement = "")

ggsave(filename = file.path(result_path, "HEB_Nodes.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
msg_box(c('Create ', file.path(result_path, "HEB_Nodes.pdf"))) # message('Create ', file.path(result_path, "HEB_Nodes.pdf"));

ggsave(filename = file.path(result_path, "HEB_AllEdges.pdf"), p5, width = 1228, height = 634, units = "px", dpi = "screen")
msg_box(c('Create ', file.path(result_path, "HEB_AllEdges.pdf"))) # message('Create ', file.path(result_path, "HEB_AllEdges.pdf"))

ggsave(filename = file.path(result_path, paste0("HEB_Top",minpercentage,"Edges.pdf")), p4, width = 1228, height = 634, units = "px", dpi = "screen")
msg_box(c('Create ', file.path(result_path, paste0("HEB_Top",minpercentage,"Edges.pdf")))) #message('Create ', file.path(result_path, paste0("HEB_Top",minpercentage,"Edges.pdf")))

