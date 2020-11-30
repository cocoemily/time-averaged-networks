library(ggplot2)

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

plot_jaccard_similarity = function(df, title) {
  p = ggplot(df) +
    geom_jitter(aes(x = num.graphs, y = sim, group = original, color = as.factor(original)), width = 0.5, height = 0.01, size = 0.4, alpha = 0.75) +
    geom_smooth(aes(x = num.graphs, y = sim, group = original, color = as.factor(original)), se = F, size = 0.5) +
    labs(x = "number of networks", y = "jaccard similarity", title = title) +
    guides(color = FALSE) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10), 
          axis.title = element_text(size = 7)) +
    geom_smooth(aes(x = num.graphs, y = sim), se = F, size = 1, color = "black")
  return(p)
}
