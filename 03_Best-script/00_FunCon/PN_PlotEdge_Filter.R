PN_PlotEdge_Filter <- function(p, edge.from, edge.to, edge.color){
  
  # edge.from/to/color
  
  pEdge <- p +
    geom_conn_bundle(data = get_con(from = edge.from, to = edge.to, col = edge.color),
                     aes(colour = col),
                     width = 0.6, tension = 0.6, alpha = .7) +
    scale_edge_colour_manual(values = rbMixPalette2,name = "Entry_ID",drop=F)+
    guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), 
           size = guide_legend(order = 2, nrow = 1, byrow = T),
           edge_color = guide_legend(order = 3, override.aes = list(edge_alpha = 1, edge_width = 3)))+
   theme(
      aspect.ratio = 1,
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
      legend.title=element_text(size=14), 
      legend.text=element_text(size=12)
    )  
  
  
  return(pEdge)
  
}

# version 2: 09112023 manual adaptation for manuscript due to updated R version
PN_PlotEdge_Filter_2 <- function(p, connectlist, edge.from, edge.to, edge.color, rbMixPalette2=c("#A2D1E7","#EB828D", "#6A59A1")){
  
  # edge.from/to/color
  
  pEdge <- p+
     geom_conn_bundle(data = get_con(from = edge.from, to = edge.to, col = edge.color),
                      aes(colour = col),
                      width = 0, tension = 0.6, alpha = 0) +
     scale_edge_colour_manual(values = rbMixPalette2,labels=edge.color,name = "Entry_ID",drop=F)+
     
    
    geom_conn_bundle(data = get_con(from = connectlist[[1]]$from, to = connectlist[[1]]$to),
                     color=rbMixPalette2[1],width=.7,tension = 0.6,alpha=.7)+
    
    geom_conn_bundle(data = get_con(from = connectlist[[2]]$from, to = connectlist[[2]]$to),
                     color=rbMixPalette2[2],width=.7,tension = 0.6, alpha=.7)+
    
    geom_conn_bundle(data = get_con(from = connectlist[[3]]$from, to = connectlist[[3]]$to),
                     color=rbMixPalette2[3],width=.7,tension = 0.6,alpha=.9)+
    guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), 
           size = guide_legend(order = 2, nrow = 1, byrow = T),
           edge_color = guide_legend(order = 3, override.aes = list(edge_alpha = 1, edge_width = 3)))+
    
    theme(
      aspect.ratio = 1,
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
      legend.spacing.x = unit(0.07, 'cm'),
      legend.spacing.y = unit(.6, 'cm'),
      legend.title=element_text(size=18), 
      legend.text=element_text(size=16)
    )  
  
  
  return(pEdge)
  
}
