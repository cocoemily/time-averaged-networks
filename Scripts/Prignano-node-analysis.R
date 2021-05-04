
source("Scripts/time-average-functions.R")
source("Scripts/Prignano-time-average.R")
source("Scripts/node_metrics.R")
source("Scripts/node-analysis-functions.R")

library(cowplot)

theme_set(theme_minimal())

####Original graph analysis####

##plot original degree distributions
ddplot = plot_original_degree_distributions(original_graphs, name.list)
ggsave("figures/metrics/Prignano/original-degree-dist.pdf", ddplot, 
       width = 10, height = 7)

btwn = plot_original_value_density_plot(original_graphs, name.list, FUN = calc.btwn)
ggsave("figures/metrics/Prignano/original-betweenness-density.pdf", btwn, 
       width = 10, height = 7)

deg = plot_original_value_density_plot(original_graphs, name.list, FUN = calc.node.deg)
ggsave("figures/metrics/Prignano/original-degree-density.pdf", deg, 
       width = 10, height = 7)

eigen = plot_original_value_density_plot(original_graphs, name.list, FUN = calc.eigen)
ggsave("figures/metrics/Prignano/original-eigenvector-density.pdf", eigen, 
       width = 10, height = 7)


####Analysis####
btwn.top = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = T, FUN = calc.btwn, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = T, FUN = calc.btwn, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = T, FUN = calc.btwn, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = T, FUN = calc.btwn, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = T, FUN = calc.btwn, o_name = "AA")
  ), "Highest betweenness centrality"
)
btwn.bottom = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = F, FUN = calc.btwn, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = F, FUN = calc.btwn, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = F, FUN = calc.btwn, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = F, FUN = calc.btwn, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = F, FUN = calc.btwn, o_name = "AA")
  ), "Lowest betweenness centrality"
)
deg.top = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = T, FUN = calc.node.deg, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = T, FUN = calc.node.deg, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = T, FUN = calc.node.deg, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = T, FUN = calc.node.deg, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = T, FUN = calc.node.deg, o_name = "AA")
  ), "Highest degree centrality"
)
deg.bottom = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = F, FUN = calc.node.deg, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = F, FUN = calc.node.deg, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = F, FUN = calc.node.deg, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = F, FUN = calc.node.deg, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = F, FUN = calc.node.deg, o_name = "AA")
  ), "Lowest degree centrality"
)
eigen.top = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = T, FUN = calc.eigen, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = T, FUN = calc.eigen, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = T, FUN = calc.eigen, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = T, FUN = calc.eigen, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = T, FUN = calc.eigen, o_name = "AA")
  ), "Highest eigenvector centrality"
)
eigen.bottom = plot_jaccard_similarity(
  rbind(
    jaccard_similarity_df(gl1e, gl1e.ngl, top = F, FUN = calc.eigen, o_name = "EIA1E"),
    jaccard_similarity_df(gl1l, gl1l.ngl, top = F, FUN = calc.eigen, o_name = "EIA1L"),
    jaccard_similarity_df(gl2, gl2.ngl, top = F, FUN = calc.eigen, o_name = "EIA2"),
    jaccard_similarity_df(glo, glo.ngl, top = F, FUN = calc.eigen, o_name = "OA"),
    jaccard_similarity_df(gla, gla.ngl, top = F, FUN = calc.eigen, o_name = "AA")
  ), "Lowest eigenvector centrality"
)


ggsave("figures/node-centrality/Prignano-centrality-similarity.pdf",
       plot_grid(deg.top, eigen.top, btwn.top, 
                 deg.bottom, eigen.bottom, btwn.bottom), 
       width = 8, height = 5
)

####Node Metric Analysis####

# calculate_mean_sd = function(gl1e, gl1l, gl2, glo, gla, label, FUN = calc.node.deg) {
#   sf1 = c(".2", ".3", ".4", ".5")
#   sf2 = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
#   sf3 = c(".2.1", ".2.2", ".3.1", ".3.2", ".3.3", ".4.1", ".4.2", ".5")
#   df1e = get_comparison_dataframe(gl1e, sf1, "EIA1E", FUN = FUN) 
#   colnames(df1e) <- c("1", "node", "2", "3", "4", "5", "original")
#   df1e = df1e %>% gather(key = "network", value = "value", c(1, 3:6))
#   df1l = get_comparison_dataframe(gl1l, sf2, "EIA1L", FUN = FUN)
#   colnames(df1l) <- c("1", "node", "2", "2 ", "3", "3 ", "4", "4 ", "5", "original")
#   df1l = df1l %>% gather(key = "network", value = "value", c(1, 3:9))
#   df2 = get_comparison_dataframe(gl2, sf3, "EIA2", FUN = FUN)
#   colnames(df2) <- c("1", "node", "2", "2 ", "3", "3 ", "3  ", "4", "4 ", "5", "original")
#   df2 = df2 %>% gather(key = "network", value = "value", c(1, 3:10))
#   dfo = get_comparison_dataframe(glo, sf2, "OA", FUN = FUN)
#   colnames(dfo) <- c("1", "node", "2", "2 ", "3", "3 ", "4", "4 ", "5", "original")
#   dfo = dfo %>% gather(key = "network", value = "value", c(1, 3:9))
#   dfa = get_comparison_dataframe(gla, sf1, "AA", FUN = FUN)
#   colnames(dfa) <- c("1", "node", "2", "3", "4", "5", "original")
#   dfa = dfa %>% gather(key = "network", value = "value", c(1, 3:6))
#   
#   allval = rbind(df1e, df1l, df2, dfo, dfa) %>%
#     mutate(network = str_trim(network)) %>% 
#     filter(!is.na(value)) %>%
#     group_by(original, network) %>%
#     summarize(mean = mean(value), 
#               stdv = sd(value))
#   allval$label = label
#   return(allval)
# }
# 
# degree.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "degree", FUN = calc.node.deg)
# btwn.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "btwn", FUN = calc.btwn)
# eigen.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "eigen", FUN = calc.eigen)
# all.val = rbind(degree.val, btwn.val, eigen.val)
# all.val$label = ordered(all.val$label, levels = c("degree", "btwn", "eigen"))
# all.val$original = ordered(all.val$original, level = c("EIA1E", "EIA1L", "EIA2", "OA", "AA"))
# metric.labels = c("degree", "betweenness centrality", "eigenvector centrality")
# names(metric.labels) = c("degree", "btwn", "eigen")
# 
# p = ggplot(all.val, aes(x = network, y = stdv, group = original, color = original)) +
#   geom_smooth(se = F, size = 0.5) +
#   theme_minimal() +
#   scale_color_colorblind() +
#   facet_wrap(~label, ncol = 1, scales = "free_y", labeller = labeller(label = metric.labels)) +
#   labs(x = "number of networks", y = "standard deviation", color = "")
# 
# ggsave(filename = "figures/metrics/stdv.pdf", p, width = 4.5, height = 6)
