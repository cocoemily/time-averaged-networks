source("scripts/Chaco-time-average.R")
source("scripts/node_metrics.R")
source("scripts/node-analysis-functions.R")
library(OmicsMarkeR)
library(ggthemes)
library(rlist)
library(cowplot)

####Original graph analysis####
##what to do here?


####TA analyses####
get_ta_graphs = function(original, index, graphs) {
  gl = list(original)
  ngl = list(1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      gl = list.append(gl, time_average(graphs, index, index+i))
      ngl = list.append(ngl, i+1)
    }
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      gl = list.append(gl, time_average(graphs, i, index))
      ngl = list.append(ngl, i+1)
    }
  } else { 
    for(i in 1:(length(graphs)-index)) {
      gl = list.append(gl, time_average(graphs, index, index+i))
      ngl = list.append(ngl, i+1)
    }
    for(i in 1:(index-1)) {
      gl = list.append(gl, time_average(graphs, i, index))
      ngl = list.append(ngl, i+1)
    }
  }
  return(list(gl, ngl))
  #return(gl)
}


name.list = c(
  "chaco800", "chaco825", "chaco850", "chaco875", 
  "chaco900", "chaco925", "chaco950", "chaco975",
  "chaco1000", "chaco1025", "chaco1050", "chaco1075",
  "chaco1100", "chaco1125", "chaco1150", "chaco1175",
  "chaco1200", "chaco1225", "chaco1250", "chaco1275"
)
tagraphs = get_ta_graphs(graphs[[1]], i, graphs)
ds.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.node.deg, name.list[[1]])
es.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.eigen, name.list[[1]])
bs.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.btwn, name.list[[1]])
ds.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.node.deg, name.list[[1]])
es.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.eigen, name.list[[1]])
bs.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.btwn, name.list[[1]])
for(i in 2:length(graphs)) {
  tagraphs = get_ta_graphs(graphs[[i]], i, graphs)
  ds.top = rbind(ds.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.node.deg, name.list[[i]]))
  es.top = rbind(es.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.eigen, name.list[[i]]))
  bs.top = rbind(bs.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.btwn, name.list[[i]]))
  ds.bot = rbind(ds.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.node.deg, name.list[[i]]))
  es.bot = rbind(es.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.eigen, name.list[[i]]))
  bs.bot = rbind(bs.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.btwn, name.list[[i]]))
}

ggsave("figures/node-centrality/Chaco-centrality-similarity.pdf",
  plot_grid(plot_jaccard_similarity(ds.top, "Highest degree centrality"), 
  plot_jaccard_similarity(es.top, "Highest eigenvector centrality"), 
  plot_jaccard_similarity(bs.top, "Highest betweenness centrality"), 
  plot_jaccard_similarity(ds.bot, "Lowest degree centrality"), 
  plot_jaccard_similarity(es.bot, "Lowest eigenvector centrality"), 
  plot_jaccard_similarity(bs.bot, "Lowest betweenness centrality")), 
  width = 8, height = 5
)
