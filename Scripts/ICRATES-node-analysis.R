source("Scripts/time-average-functions.R")
source("Scripts/ICRATES-time-average.R")
#load("Data/ICRATES/workspace_image.RData")
source("Scripts/node_metrics.R")
source("Scripts/node-analysis-functions.R")

library(cowplot)

load("Data/ICRATES/timeslice_levels.RData")
name.list = lvls

####Original graph analysis####
ddplot = plot_original_degree_distributions(graphs, name.list)
ggsave("figures/metrics/ICRATES/original-degree-dist.pdf", ddplot,
       width = 10, height = 7)

btwn = plot_original_value_density_plot(graphs, name.list, FUN = calc.btwn)
ggsave("figures/metrics/ICRATES/original-betweenness-density.pdf", btwn, 
       width = 10, height = 7)

deg = plot_original_value_density_plot(graphs, name.list, FUN = calc.node.deg)
ggsave("figures/metrics/ICRATES/original-degree-density.pdf", deg, 
       width = 10, height = 7)

eigen = plot_original_value_density_plot(graphs, name.list, FUN = calc.eigen)
ggsave("figures/metrics/ICRATES/original-eigenvector-density.pdf", eigen, 
       width = 10, height = 7)


####TA analyses####
tagraphs = icrates_tagraphs[[1]]
ds.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.node.deg, name.list[[1]])
es.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.eigen, name.list[[1]])
bs.top = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.btwn, name.list[[1]])
ds.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.node.deg, name.list[[1]])
es.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.eigen, name.list[[1]])
bs.bot = jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.btwn, name.list[[1]])
for(i in 2:length(graphs)) {
  tagraphs = icrates_tagraphs[[i]]
  ds.top = rbind(ds.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.node.deg, name.list[[i]]))
  es.top = rbind(es.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.eigen, name.list[[i]]))
  bs.top = rbind(bs.top, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = T, FUN = calc.btwn, name.list[[i]]))
  ds.bot = rbind(ds.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.node.deg, name.list[[i]]))
  es.bot = rbind(es.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.eigen, name.list[[i]]))
  bs.bot = rbind(bs.bot, jaccard_similarity_df(tagraphs[[1]], tagraphs[[2]], top = F, FUN = calc.btwn, name.list[[i]]))
}

ggsave("figures/node-centrality/ICRATES-centrality-similarity.pdf",
       plot_grid(plot_jaccard_similarity(ds.top, "Highest degree centrality"), 
                 plot_jaccard_similarity(es.top, "Highest eigenvector centrality"), 
                 plot_jaccard_similarity(bs.top, "Highest betweenness centrality"), 
                 plot_jaccard_similarity(ds.bot, "Lowest degree centrality"), 
                 plot_jaccard_similarity(es.bot, "Lowest eigenvector centrality"), 
                 plot_jaccard_similarity(bs.bot, "Lowest betweenness centrality")), 
       width = 8, height = 5
)
