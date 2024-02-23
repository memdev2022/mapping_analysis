directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping analysis')

{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# Step 3. Create Hierarchical Edge plot ====

# determine identifier variables
identifier_var <- c("Entry_ID", "Authors","Title", "Year")
{source(file.path(funcon_path, "PN_GetPalette.R"))}
{source(file.path(funcon_path, "PN_PlotNode.R"))}

# retrieve hierarchy and edge
hierarchy <- read_excel(file.path(data_path,"Hierarchy.xlsx"))
vertices <- read_excel(file.path(data_path,"Vertices.xlsx"))

vertices$group <- factor(vertices$group, levels = vertices$group %>% unique())
ngroup <- vertices %>% filter(group == "Origin") %>% nrow()

# Step 3'. Customize node labels on the plot(www.r-graph-gallery.com)
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)

p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)+
  geom_node_point(aes(x = x*1.02, y = y*1.02, filter = leaf, color = group),
                  alpha = 0.7, size = 6, show.legend = T)+
  # adapt the order of the legend items (color-coded)
  scale_color_manual(name = 'Variable', values = nodePalette, 
                     breaks = setdiff(unique(vertices$group), c(NA,"Origin")))+
  # add label for levels
  geom_node_text(aes(x = x*1.09, y = y*1.09, filter = leaf, label = groupid), hjust = 0.5, vjust = 0.5, size = 4, alpha = 1)+
  # adjust themes and guides
  # scale_y_reverse()+
  # scale_x_reverse()+
  # coord_flip()+
  theme(aspect.ratio = 1,
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
        legend.title=element_text(size=18), 
        legend.text=element_text(size=16))+
  guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), size = guide_legend(order = 2, nrow = 1, byrow = T))  
p


legend_p=get_legend(p)


ggsave(filename = file.path(result_path, "HEB_Node.pdf"), p, width = 1228, height = 634, units = "px", dpi = "screen")
message('Create ', file.path(result_path, "HEB_Node.pdf"))
ggsave(filename = file.path(result_path, "HEB_NodeLegend.pdf"), legend_p, width = 560, height = 634, units = "px", dpi = "screen")
message('Create ', file.path(result_path, "HEB_NodeLegend.pdf"))
