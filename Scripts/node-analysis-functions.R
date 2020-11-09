get_nodes = function(graph, top = T, FUN = calc.node.deg) {
  ndf = FUN(graph)
  comp.set = NULL
  if(top) {
    comp.set = as.character(ndf[order(-ndf$metric),]$node[1:5])
  } else {
    comp.set = as.character(ndf[order(ndf$metric),]$node[1:5])
  }
  return(comp.set)
}

jaccard_similarity_df = function(graphlist, numgraphslist, top = T, FUN = calc.node.deg, o_name) {
  df = data.frame(num.graphs = integer(), 
                  sim = integer())
  onodes = get_nodes(graphlist[[1]], top = top, FUN = FUN)
  
  for(i in 2:length(graphlist)) {
    df[nrow(df) + 1, ] = c(
      numgraphslist[[i]], 
      jaccard(onodes, get_nodes(graphlist[[i]], top = top, FUN = FUN))
    )
  }
  df$original = c(replicate(nrow(df), o_name))
  return(df)
}

plot_jaccard_similarity = function(df) {
  p = ggplot(df, aes(x = num.graphs, y = sim, group = original, color = as.factor(original))) +
    geom_smooth(se = F) +
    geom_jitter(width = 0.1, height = 0.1) +
    labs(x = "number of networks", y = "jaccard similarity") +
    guides(color = FALSE) +
    theme_minimal()
  return(p)
}

#need to work on this function to create plot grids (use structure for one graph from Prignano node analysis)
plot_grid_jaccard = function(list.gl, list.ngl) {
  for(f in list(calc.btwn, calc.node.deg, calc.eigen)) {
    df = jaccard_similarity_df()
  }
}