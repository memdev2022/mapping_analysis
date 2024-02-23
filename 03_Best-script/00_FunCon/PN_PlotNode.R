PN_PlotNode <- function(mygraph, 
                        size.breaks = seq.int(0, 2, length.out=3)){
             
  
  p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)+
    geom_node_point(aes(x = x*1.02, y = y*1.02, filter = leaf,color = group, size = nEntry),alpha = 0.7, show.legend = T)+
    # adapt the order of the legend items (color-coded)
    scale_color_manual(name = 'Variable', values = nodePalette, breaks = setdiff(unique(vertices$group), c(NA,"Origin")))+
    # adapt the size of nodes
    # scale_size_continuous(name = "Number of entries", range = c(3, 13), breaks = seq.int(0, max(vertices$nEntry, na.rm = T), length.out=guide.size.length.out) %>% round())+
    scale_size_continuous(name = "Number of entries (Node size)", range = c(3, 13), breaks = size.breaks)+
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
  
  
  return(p)
  
}
