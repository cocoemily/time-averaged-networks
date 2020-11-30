#Prignano dataset node-based analysis
library(OmicsMarkeR)
library(ggthemes)
source("scripts/Prignano-time-average.R")
source("scripts/node_metrics.R")
source("scripts/node-analysis-functions.R")

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)

#EIA1E analysis
gl1e = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6])), 
               average_two(eia1e.edge, eia1l.edge, groups), 
               average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
               average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
               average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl1e.ngl = list(1, 2, 3, 4, 5)

#EIA1L analysis
gl1l = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6])), 
            average_two(eia1e.edge, eia1l.edge, groups), 
            average_two(eia1l.edge, eia2.edge, groups), 
            average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
            average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
            average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl1l.ngl = list(1, 2, 2, 3, 3, 4, 4, 5)

#EIA2 analysis
gl2 = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia2.edge, groups)[,5:6])), 
                 average_two(eia1l.edge, eia2.edge, groups), 
                 average_two(eia2.edge, oa.edge, groups), 
                 average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
                 average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_three(eia2.edge, oa.edge, aa.edge, groups), 
                 average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl2.ngl = list(1, 2, 2, 3, 3, 3, 4, 4, 5)

#OA analysis
glo = list(graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups), 
                 average_two(eia2.edge, oa.edge, groups), 
                 average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_three(eia2.edge, oa.edge, aa.edge, groups), 
                 average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
glo.ngl = list(1, 2, 2, 3, 3, 4, 4, 5)

#AA analysis
gla = list(graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups),
                 average_three(eia2.edge, oa.edge, aa.edge, groups),
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gla.ngl = list(1, 2, 3, 4, 5)

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
