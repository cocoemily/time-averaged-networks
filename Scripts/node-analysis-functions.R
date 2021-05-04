library(ggplot2)
library(OmicsMarkeR)

#'
#'Function for getting the nodes with the highest or lowest values based on the provided function
#'@param graph network to be analyzed
#'@param top boolean to determine if returning highest values (true) or lowest values (false)
#'@param FUN function for calculating node-based metric
#'@return dataframe containing the node IDs of the nodes with the extreme values
#'
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

#'
#'Function for calculating the jaccard similarity between the node set with highest/lowest metric
#'values of an original networks and its associated time-averaged networks
#'@param graphlist list of networks where the first is the original and the subsequent are time-averaged
#'@param numgraphslist list of value numerating how many networks are time-average at each index of the graphlist
#'@param top boolean to determine if looking at highest values (true) or lowest values (false)
#'@param FUN function for calculating node-based metric
#'@param o_name string with name for original networks 
#'@return dataframe with jaccard similarity values by number of networks time-averaged
#'
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

#'
#'Creates single plot for jaccard similarity scores by number of networks based on one node metric
#'@param df jaccard similarity dataframe from above function
#'@param title string with title of plot
#'@return plot with overall trend of similarity as time-averaging increases (black line) as well as
#'trend lines for similarity based on each individual original network 
#'
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

#'
#'Plots degree distribution histograms for each original network
#'@param original_graphs a list of the original graph from the data
#'@param name.list a list of graph names in chronological order
#'@return a plot with histograms of degree distributions
#'
#'
plot_original_degree_distributions = function(original_graphs, name.list) {
  ddist = data.frame()
  if(vcount(graphs[[1]]) != 0) {
    ddist = calc.deg.dist(original_graphs[[1]])
    ddist$network = name.list[[1]]
  }
  for(i in 2:length(original_graphs)) {
    if(vcount(graphs[[i]]) != 0) {
      temp = calc.deg.dist(original_graphs[[i]])
      temp$network = name.list[[i]]
      ddist = rbind(ddist, temp)
    }
  }
  ddist$network = factor(ddist$network, levels = name.list)
  
  ddplot = ggplot(ddist, aes(x = degree, y = freq)) +
    geom_col() +
    facet_wrap(~ network, scale = "free_x")
  
  return(ddplot)
}

#'
#'Plots density function of a given node metric for each original network
#'@param original_graphs a list of the original graph from the data
#'@param name.list a list of graph names in chronological order
#'@return a plot with density functions facetted by original network name
#'
#'
plot_original_value_density_plot = function(original_graphs, name.list, FUN = calc.node.deg) {
  val = data.frame()
  if(vcount(graphs[[1]]) != 0) {
    val = FUN(original_graphs[[1]])
    val$network = name.list[[1]]
  }
  for(i in 2:length(original_graphs)) {
    if(vcount(graphs[[i]]) != 0) {
      temp = FUN(original_graphs[[i]])
      temp$network = name.list[[i]]
      val = rbind(val, temp)
    }
  }
  val$network = factor(val$network, levels = name.list)
  
  p = ggplot(val, aes(metric, fill = network)) + 
    geom_density() + facet_wrap(~ network, scale = "free_x") +
    theme(legend.position = "none")
  
  return(p)
}
