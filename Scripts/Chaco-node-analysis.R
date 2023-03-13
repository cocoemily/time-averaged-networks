source("Scripts/time-average-functions.R")
#source("Scripts/Chaco-time-average.R")
source("Scripts/node_metrics.R")
source("Scripts/node-analysis-functions.R")

load("Data/Chaco_original_graph_objects.RData")
graphs = Chaco_original_graphs
for(i in 1:8) {
  load(paste0("Data/Chaco_ta_graph_objects_", i,".RData"))
}
chaco_tagraphs = c(c1, c2, c3, c4, c5, c6, c7, c8)

library(cowplot)

theme_set(theme_minimal())

name.list = c(
  "chaco800", "chaco825", "chaco850", "chaco875", 
  "chaco900", "chaco925", "chaco950", "chaco975",
  "chaco1000", "chaco1025", "chaco1050", "chaco1075",
  "chaco1100", "chaco1125", "chaco1150", "chaco1175",
  "chaco1200", "chaco1225", "chaco1250", "chaco1275"
)

####Original graph analysis####
##plot original degree distributions
ddplot = plot_original_degree_distributions(graphs, name.list)
ggsave("figures/metrics/Chaco/original-degree-dist.pdf", ddplot, 
       width = 10, height = 7)

btwn = plot_original_value_density_plot(graphs, name.list, FUN = calc.btwn)
ggsave("figures/metrics/Chaco/original-betweenness-density.pdf", btwn, 
       width = 10, height = 7)

deg = plot_original_value_density_plot(graphs, name.list, FUN = calc.node.deg)
ggsave("figures/metrics/Chaco/original-degree-density.pdf", deg, 
       width = 10, height = 7)

eigen = plot_original_value_density_plot(graphs, name.list, FUN = calc.eigen)
ggsave("figures/metrics/Chaco/original-eigenvector-density.pdf", eigen, 
       width = 10, height = 7)



####TA analyses####
tagraphs = chaco_tagraphs[[1]]
ds.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.node.deg, name.list[[1]])
es.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.eigen, name.list[[1]])
bs.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.btwn, name.list[[1]])
ds.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.node.deg, name.list[[1]])
es.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.eigen, name.list[[1]])
bs.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.btwn, name.list[[1]])
for(i in 2:length(graphs)) {
  tagraphs = chaco_tagraphs[[i]]
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
