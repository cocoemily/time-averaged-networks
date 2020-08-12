#Prignano dataset node-based analysis
library(OmicsMarkeR)
library(ggthemes)
source("scripts/Prignano-time-average.R")
source("scripts/node_metrics.R")

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)
calc_jaccard_similarity_high = function(compdf, num.graphs) { #calculate similarity between 
  comp.set = as.character(compdf[order(-compdf$metric),]$node[1:5])
  df = data.frame(num.graphs = integer(), 
                  sim = integer())
  
  for(i in 3:ncol(compdf)) {
    df[nrow(df) + 1, ] = c(num.graphs[i-2], jaccard(comp.set, as.character(compdf[order(-compdf[,i]),]$node[1:5])))
  }
  #df$original = label
  return(df)
}

calc_jaccard_similarity_low = function(compdf, num.graphs) { 
  comp.set = as.character(compdf[order(compdf$metric),]$node[1:5])
  df = data.frame(num.graphs = integer(), 
                  sim = integer())
  
  for(i in 3:ncol(compdf)) {
    df[nrow(df) + 1, ] = c(num.graphs[i-2], jaccard(comp.set, as.character(compdf[order(compdf[,i]),]$node[1:5])))
  }
  #df$original = label
  return(df)
}

get_comparison_dataframe = function(graphlist, suffixes, label, FUN = calc.btwn) {
  compdf = FUN(graphlist[[1]])
  for(i in 2:length(graphlist)) {
    compdf = compdf %>% full_join(FUN(graphlist[[i]]), by = "node", suffix = c("", suffixes[i-1]))
  }
  compdf$original = label
  return(compdf)
}


#EIA1E analysis
gl1e = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6])), 
               average_two(eia1e.edge, eia1l.edge, groups), 
               average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
               average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
               average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))

#EIA1L analysis
gl1l = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6])), 
            average_two(eia1e.edge, eia1l.edge, groups), 
            average_two(eia1l.edge, eia2.edge, groups), 
            average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
            average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
            average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))

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

#OA analysis
glo = list(graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups), 
                 average_two(eia2.edge, oa.edge, groups), 
                 average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_three(eia2.edge, oa.edge, aa.edge, groups), 
                 average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))

#AA analysis
gla = list(graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups),
                 average_three(eia2.edge, oa.edge, aa.edge, groups),
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))

all_similarity_dataframe_high = function(gl1e, gl1l, gl2, glo, gla, FUN = calc.node.deg) {
  sf1 = c(".2", ".3", ".4", ".5")
  sf2 = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
  sf3 = c(".2.1", ".2.2", ".3.1", ".3.2", ".3.3", ".4.1", ".4.2", ".5")
  allsim = rbind(
    calc_jaccard_similarity_high(get_comparison_dataframe(gl1e, sf1, "EIA1E", FUN = FUN), c(2, 3, 4, 5)), 
    calc_jaccard_similarity_high(get_comparison_dataframe(gl1l, sf2, "EIA1L", FUN = FUN), c(2, 2, 3, 3, 4, 4, 5)), 
    calc_jaccard_similarity_high(get_comparison_dataframe(gl2, sf3, "EIA2", FUN = FUN), c(2, 2, 3, 3, 3, 4, 4, 5)), 
    calc_jaccard_similarity_high(get_comparison_dataframe(glo, sf2, "OA", FUN = FUN), c(2, 2, 3, 3, 4, 4, 5)),
    calc_jaccard_similarity_high(get_comparison_dataframe(gla, sf1, "AA", FUN = FUN), c(2, 3, 4, 5))
  )
  allsim$original = ordered(allsim$original, levels = c("EIA1E", "EIA1L", "EIA2", "OA", "AA"))
  return(allsim)
}

all_similarity_dataframe_low = function(gl1e, gl1l, gl2, glo, gla, FUN = calc.node.deg) {
  sf1 = c(".2", ".3", ".4", ".5")
  sf2 = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
  sf3 = c(".2.1", ".2.2", ".3.1", ".3.2", ".3.3", ".4.1", ".4.2", ".5")
  allsim = rbind(
    calc_jaccard_similarity_low(get_comparison_dataframe(gl1e, sf1, FUN = FUN), c(2, 3, 4, 5), "EIA1E"), 
    calc_jaccard_similarity_low(get_comparison_dataframe(gl1l, sf2, FUN = FUN), c(2, 2, 3, 3, 4, 4, 5), "EIA1L"), 
    calc_jaccard_similarity_low(get_comparison_dataframe(gl2, sf3, FUN = FUN), c(2, 2, 3, 3, 3, 4, 4, 5), "EIA2"), 
    calc_jaccard_similarity_low(get_comparison_dataframe(glo, sf2, FUN = FUN), c(2, 2, 3, 3, 4, 4, 5), "OA"),
    calc_jaccard_similarity_low(get_comparison_dataframe(gla, sf1, FUN = FUN), c(2, 3, 4, 5), "AA")
  )
  allsim$original = ordered(allsim$original, levels = c("EIA1E", "EIA1L", "EIA2", "OA", "AA"))
  return(allsim)
}

deg.sim.high = all_similarity_dataframe_high(gl1e, gl1l, gl2, glo, gla, FUN = calc.node.deg)
eigen.sim.high = all_similarity_dataframe_high(gl1e, gl1l, gl2, glo, gla, FUN = calc.eigen)
btwn.sim.high = all_similarity_dataframe_high(gl1e, gl1l, gl2, glo, gla, FUN = calc.btwn)

deg.sim.low = all_similarity_dataframe_low(gl1e, gl1l, gl2, glo, gla, FUN = calc.node.deg)
eigen.sim.low = all_similarity_dataframe_low(gl1e, gl1l, gl2, glo, gla, FUN = calc.eigen)
btwn.sim.low = all_similarity_dataframe_low(gl1e, gl1l, gl2, glo, gla, FUN = calc.btwn)


plot_jaccard_similarity = function(df, name) {
  p = ggplot(df, aes(x = num.graphs, y = sim, group = original, color = as.factor(original))) +
    geom_smooth(se = F) +
    geom_jitter(width = 0.1, height = 0.1) +
    scale_color_colorblind() +
    labs(x = "number of networks", y = "jaccard similarity", color = "original graph")
  ggsave(filename = paste0("figures/node-centrality/", name, ".pdf"), p, width = 5, height = 3)
}

plot_jaccard_similarity(deg.sim.high, "deg-high")
plot_jaccard_similarity(eigen.sim.high, "eigen-high")
plot_jaccard_similarity(btwn.sim.high, "btwn-high")
plot_jaccard_similarity(deg.sim.low, "deg-low")
plot_jaccard_similarity(eigen.sim.low, "eigen-low")
plot_jaccard_similarity(btwn.sim.low, "btwn-low")

####Node Metric Analysis####

calculate_mean_sd = function(gl1e, gl1l, gl2, glo, gla, label, FUN = calc.node.deg) {
  sf1 = c(".2", ".3", ".4", ".5")
  sf2 = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
  sf3 = c(".2.1", ".2.2", ".3.1", ".3.2", ".3.3", ".4.1", ".4.2", ".5")
  df1e = get_comparison_dataframe(gl1e, sf1, "EIA1E", FUN = FUN) 
  colnames(df1e) <- c("1", "node", "2", "3", "4", "5", "original")
  df1e = df1e %>% gather(key = "network", value = "value", c(1, 3:6))
  df1l = get_comparison_dataframe(gl1l, sf2, "EIA1L", FUN = FUN)
  colnames(df1l) <- c("1", "node", "2", "2 ", "3", "3 ", "4", "4 ", "5", "original")
  df1l = df1l %>% gather(key = "network", value = "value", c(1, 3:9))
  df2 = get_comparison_dataframe(gl2, sf3, "EIA2", FUN = FUN)
  colnames(df2) <- c("1", "node", "2", "2 ", "3", "3 ", "3  ", "4", "4 ", "5", "original")
  df2 = df2 %>% gather(key = "network", value = "value", c(1, 3:10))
  dfo = get_comparison_dataframe(glo, sf2, "OA", FUN = FUN)
  colnames(dfo) <- c("1", "node", "2", "2 ", "3", "3 ", "4", "4 ", "5", "original")
  dfo = dfo %>% gather(key = "network", value = "value", c(1, 3:9))
  dfa = get_comparison_dataframe(gla, sf1, "AA", FUN = FUN)
  colnames(dfa) <- c("1", "node", "2", "3", "4", "5", "original")
  dfa = dfa %>% gather(key = "network", value = "value", c(1, 3:6))
  
  allval = rbind(df1e, df1l, df2, dfo, dfa) %>%
    mutate(network = str_trim(network)) %>% 
    filter(!is.na(value)) %>%
    group_by(original, network) %>%
    summarize(mean = mean(value), 
              stdv = sd(value))
  allval$label = label
  return(allval)
}

degree.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "degree", FUN = calc.node.deg)
btwn.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "btwn", FUN = calc.btwn)
eigen.val = calculate_mean_sd(gl1e, gl1l, gl2, glo, gla, "eigen", FUN = calc.eigen)
all.val = rbind(degree.val, btwn.val, eigen.val)
all.val$label = ordered(all.val$label, levels = c("degree", "btwn", "eigen"))
all.val$original = ordered(all.val$original, level = c("EIA1E", "EIA1L", "EIA2", "OA", "AA"))
metric.labels = c("degree", "betweenness centrality", "eigenvector centrality")
names(metric.labels) = c("degree", "btwn", "eigen")

p = ggplot(all.val, aes(x = network, y = stdv, group = original, color = original)) +
  geom_smooth(se = F, size = 0.5) +
  theme_minimal() +
  scale_color_colorblind() +
  facet_wrap(~label, ncol = 1, scales = "free_y", labeller = labeller(label = metric.labels)) +
  labs(x = "number of networks", y = "standard deviation", color = "")

ggsave(filename = "figures/metrics/stdv.pdf", p, width = 4.5, height = 6)
