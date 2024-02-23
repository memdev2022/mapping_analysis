PN_PlotEdge_Group <- function(p, edge.from, edge.to, edge.color){
  
  # p = plot with frame (nodes)
  # edge.from/to/color
  
  pEdge <- p +
    geom_conn_bundle(data = get_con(from = edge.from, to = edge.to, col = edge.color),
                     aes(colour = col),
                     width = 0.8, tension = 0.6, alpha = .7) +
    scale_edge_colour_gradientn(colours = heatPalette, breaks = seq(0,100,length.out=6),
                                limits = c(0,100), name = "Proportion of entries (%)")+
    guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), 
           size = guide_legend(order = 2, nrow = 1, byrow = T)
           # no need to specify edge_colour
           )+
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



PN_PlotEdge_Group_2 <- function(p, connectlist, alphavalue=c(.3,5,.7,.9,1)){
  
  # 09112023: New version adapted for the manuscript 
  # p = plot with frame (nodes)
  # edge.from/to/color
  
  pEdge <- p +
    geom_conn_bundle(data = get_con(from = NA, to = NA, value=NA),
                     aes(color=value,alpha=value),width=.7,tension = 0.6) +
    
    geom_conn_bundle(data = get_con(from = connectlist[[1]]$from, to = connectlist[[1]]$to),
                     color=heatPalette[1],width=.7,tension = 0.6,alpha=alphavalue[1])+
    
    geom_conn_bundle(data = get_con(from = connectlist[[2]]$from, to = connectlist[[2]]$to),
                     color=heatPalette[2],width=.7,tension = 0.6,alpha=alphavalue[2])+
    
    geom_conn_bundle(data = get_con(from = connectlist[[3]]$from, to = connectlist[[3]]$to),
                     color=heatPalette[3],width=.7,tension = 0.6,alpha=alphavalue[3])+
    
    geom_conn_bundle(data = get_con(from = connectlist[[4]]$from, to = connectlist[[4]]$to),
                     color=heatPalette[4],width=.7,tension = 0.6,alpha=alphavalue[4])+
    
    geom_conn_bundle(data = get_con(from = connectlist[[5]]$from, to = connectlist[[5]]$to),
                     color=heatPalette[5],width=.7,tension = 0.6,alpha=alphavalue[5])+
    
    scale_edge_colour_gradientn(colours = heatPalette, breaks = seq(0,100,length.out=6),
                                limits = c(0,100), name = "Percentage of entries (%)")+
    guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), 
           size = guide_legend(order = 2, nrow = 1, byrow = T),
           # no need to specify edge_colour
           edge_colour = guide_edge_colorbar(title.position='top',direction='horizontal', barwidth=unit(8,'cm'))
    )+
    theme(
      aspect.ratio = 1,
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
      legend.spacing.x = unit(0.07, 'cm'),
      legend.spacing.y = unit(.6, 'cm'),
      # legend.
      legend.title=element_text(size=18), 
      legend.text=element_text(size=16)
    ) 
  
  
  return(pEdge)
  
}