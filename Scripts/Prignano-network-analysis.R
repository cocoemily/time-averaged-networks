#Prignano dataset network-level analysis
source("scripts/Prignano-time-average.R")
theme_set(theme_minimal())
library(ggthemes)
library(lme4)
library(dplyr)
library(factoextra)

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)
ta5 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups = groups)

ta2_1 = ta_compare(eia1e.edge, eia1l.edge, groups = groups)
ta2_2 = ta_compare(eia1l.edge, eia2.edge, groups = groups)
ta2_3 = ta_compare(eia2.edge, oa.edge, groups = groups)
ta2_4 = ta_compare(oa.edge, aa.edge, groups = groups)

ta3_1 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, groups = groups)
ta3_2 = ta_compare(eia1l.edge, eia2.edge, oa.edge, groups = groups)
ta3_3 = ta_compare(eia2.edge, oa.edge, aa.edge, groups = groups)

ta4_1 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups = groups)
ta4_2 = ta_compare(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups = groups)

eia1eta = rbind(ta2_1 %>% filter(names != "eia1l"), 
                ta3_1 %>% filter(names == "ta3"),
                ta4_1 %>% filter(names == "ta4"),
                ta5 %>% filter(names == "ta5"))
eia1eta$num.graphs = c(1,2,3,4,5)
eia1eta$network = "EIA1E"
eia1lta = rbind(ta2_1 %>% filter(names != "eia1e"), ta2_2 %>% filter(names == "ta2"), 
                ta3_1 %>% filter(names == "ta3"), ta3_2 %>% filter(names == "ta3"), 
                ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
                ta5 %>% filter(names == "ta5"))
eia1lta$num.graphs = c(1,2,2,3,3,4,4,5)
eia1lta$network = "EIA1L"
eia2ta = rbind(ta2_2 %>% filter(names != "eia1l"), ta2_3 %>% filter(names == "ta2"), 
               ta3_1 %>% filter(names == "ta3"), ta3_2 %>% filter(names == "ta3"), ta3_3 %>% filter(names == "ta3"), 
               ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
               ta5 %>% filter(names == "ta5"))
eia2ta$num.graphs = c(1,2,2,3,3,3,4,4,5)
eia2ta$network = "EIA2"
oata = rbind(ta2_3 %>% filter(names != "eia2"), ta2_4 %>% filter(names == "ta2"), 
             ta3_2 %>% filter(names == "ta3"), ta3_3 %>% filter(names == "ta3"), 
             ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
             ta5 %>% filter(names == "ta5"))
oata$num.graphs = c(1,2,2,3,3,4,4,5)
oata$network = "OA"
aata = rbind(ta2_4 %>% filter(names != "oa"), 
             ta3_3 %>% filter(names == "ta3"), 
             ta4_2 %>% filter(names == "ta4"), 
             ta5 %>% filter(names == "ta5"))
aata$num.graphs = c(1,2,3,4,5)
aata$network = "AA"

alldata = rbind(eia1eta, eia1lta, eia2ta, oata, aata)
alldata$network = factor(alldata$network, levels = c("EIA1E", "EIA1L", 
                                                     "EIA2", "OA", "AA"))

####PCA Analysis####
avg = alldata %>% filter(num.graphs != 1)
orig = alldata %>% filter(num.graphs == 1)

pca_avg = prcomp(avg[,c(1, 3:6, 8:9)], scale = T)
avgdf = data.frame(pca_avg$x)
avgdf$network = avg$network
hulls = avgdf %>% group_by(network) %>% dplyr::slice(chull(PC1, PC2))
ggplot(avgdf, aes(x = PC1, y = PC2, fill = network, color = network)) +
  geom_point() +
  coord_equal() +
  geom_polygon(data = hulls, alpha = 0.25)
print(pca_avg)
biplot(pca_avg)

orig.coord = as.data.frame(predict(pca_avg, orig[,c(1, 3:6, 8:9)]))
rownames(orig.coord) = as.character(orig$network)

p.pca = fviz_pca_biplot(pca_avg, repel = T, pointsize = 1, pointshape = 20, col.var = "grey30", col.ind = avg$network,
                addEllipses = T, ellipse.type = "confidence", palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"), 
                label = "var")
p.pca = fviz_add(p.pca, orig.coord[1,], shape = 15, color = "#E69F00")
p.pca = fviz_add(p.pca, orig.coord[2,], shape = 15, color = "#56B4E9")
p.pca = fviz_add(p.pca, orig.coord[3,], shape = 15, color = "#009E73")
p.pca = fviz_add(p.pca, orig.coord[4,], shape = 15, color = "#F0E442")
p.pca = fviz_add(p.pca, orig.coord[5,], shape = 15, color = "#0072B2")
plot(p.pca)
ggsave("figures/pca/pca-biplot.pdf", p.pca)

  

####Null Model Analysis####
get_null_model_values = function(graph, FUN = calc.diam) {
  values = c()
  for(i in 1:1000) {
    ngraph = rewire(graph, each_edge(p = 1))
    values = c(values, FUN(ngraph))
  }
  return(values)
}

####Model Error Comparison####
calculate_model_error = function(graph, df) {
  null.btwn = get_null_model_values(graph, FUN = calc.mean.between)
  df$btwn_me = ifelse(median(null.btwn) > df$btwn, 
                   (median(null.btwn) - df$btwn)/(quantile(null.btwn, 0.975) - median(null.btwn)), 
                   (median(null.btwn) - df$btwn)/(median(null.btwn) - quantile(null.btwn, 0.025))) 
  
  null.eigen = get_null_model_values(graph, FUN = calc.mean.eigen)
  df$eigen_me = ifelse(median(null.eigen) > df$eigen, 
                       (median(null.eigen) - df$eigen)/(quantile(null.eigen, 0.975) - median(null.eigen)), 
                       (median(null.eigen) - df$eigen)/(median(null.eigen) - quantile(null.eigen, 0.025)))

  null.pl = get_null_model_values(graph, FUN = calc.mean.path.length)
  df$pl_me = ifelse(median(null.pl) > df$path.length, 
                    (median(null.pl) - df$path.length)/(quantile(null.pl, 0.975) - median(null.pl)), 
                    (median(null.pl) - df$path.length)/(median(null.pl) - quantile(null.pl, 0.025)))

  null.diam = get_null_model_values(graph, FUN = calc.diam)
  df$diam_me = ifelse(median(null.diam) > df$diam, 
                      (median(null.diam) - df$diam)/(quantile(null.diam, 0.975) - median(null.diam)), 
                      (median(null.diam) - df$diam)/(median(null.diam) - quantile(null.diam, 0.025)))

  null.cc = get_null_model_values(graph, FUN = calc.cc)
  df$cc_me = ifelse(median(null.cc) > df$cc, 
                    (median(null.cc) - df$cc)/(quantile(null.cc, 0.975) - median(null.cc)), 
                    (median(null.cc) - df$cc)/(median(null.cc) - quantile(null.cc, 0.025)))

  null.mod = get_null_model_values(graph, FUN = calc.mod)
  df$mod_me = ifelse(median(null.mod) > df$mod, 
                     (median(null.mod) - df$mod)/(quantile(null.mod, 0.975) - median(null.mod)), 
                     (median(null.mod) - df$mod)/(median(null.mod) - quantile(null.mod, 0.025)))
  
  # null.ed = get_null_model_values(graph, FUN = calc.edge.dens)
  # df$ed_me = ifelse(median(null.ed) > df$edge.dens, 
  #                    (median(null.ed) - df$edge.dens)/(quantile(null.ed, 0.975) - median(null.ed)), 
  #                    (median(null.ed) - df$edge.dens)/(median(null.ed) - quantile(null.ed, 0.025)))
  # 
  # null.deg = get_null_model_values(graph, FUN = calc.mean.deg)
  # df$deg_me = ifelse(median(null.deg) > df$mean.deg, 
  #                   (median(null.deg) - df$mean.deg)/(quantile(null.deg, 0.975) - median(null.deg)), 
  #                   (median(null.deg) - df$mean.deg)/(median(null.deg) - quantile(null.deg, 0.025)))
  
  
  return(df)
}

modelerrors = rbind(
  calculate_model_error(graph_from_edgelist(as.matrix(eia1e.edge[,1:2])), eia1eta), 
  calculate_model_error(graph_from_edgelist(as.matrix(eia1l.edge[,1:2])), eia1lta), 
  calculate_model_error(graph_from_edgelist(as.matrix(eia2.edge[,1:2])), eia2ta), 
  calculate_model_error(graph_from_edgelist(as.matrix(oa.edge[,1:2])), oata), 
  calculate_model_error(graph_from_edgelist(as.matrix(aa.edge[,1:2])), aata)
)

plot_me = function(modelerrors) {
  me = modelerrors %>% gather(key = "modelerror", value = "value", c(15:20))
  me$network = factor(me$network, levels = c("EIA1E", "EIA1L", "EIA2", "OA", "AA"))
  metric.labs = c("betweenness centrality", "clustering coefficient", "diameter", 
                  "eigenvector centrality", "modularity", "path length", 
                  "edge density", "degree")
  names(metric.labs) = c("btwn_me", "cc_me", "diam_me", "eigen_me", "mod_me", "pl_me", "ed_me", "deg_me")
  meplot = ggplot(me, aes(x = num.graphs, y = value, group = network, color = network)) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, alpha = 0.05, color = NA, fill = "grey80") +
    geom_hline(yintercept = 0) +
    geom_smooth(se = F) +
    facet_wrap(~ modelerror, scales = "free_y", labeller = labeller(modelerror = metric.labs)) +
    labs(x = "number of graphs") +
    theme(axis.title.y = element_blank()) +
    scale_color_colorblind()
  return(meplot)
}
ggsave("figures/null-models/me_ta-to-orig.pdf", plot_me(modelerrors), height = 4, width = 7)

me_eia1eta = rbind(
  calculate_model_error(average_two(eia1e.edge, eia1l.edge, groups), (eia1eta %>% filter(names == "ta2"))), 
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), (eia1eta %>% filter(names == "ta3"))), 
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), (eia1eta %>% filter(names == "ta4"))), 
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), (eia1eta %>% filter(names == "ta5")))
)
ggsave("figures/null-models/me_eia1e.pdf", plot_me(me_eia1eta))

me_eia1lta = rbind(
  calculate_model_error(average_two(eia1e.edge, eia1l.edge, groups), eia1lta[2,]),
  calculate_model_error(average_two(eia1l.edge, eia2.edge, groups), eia1lta[3,]),
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), eia1lta[4,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), eia1lta[5,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), eia1lta[6,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia1lta[7,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia1lta[8,])
)
ggsave("figures/null-models/me_eia1l.pdf", plot_me(me_eia1lta))

me_eia2ta = rbind(
  calculate_model_error(average_two(eia1l.edge, eia2.edge, groups), eia2ta[2,]),
  calculate_model_error(average_two(eia2.edge, oa.edge, groups), eia2ta[3,]),
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), eia2ta[4,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), eia2ta[5,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), eia2ta[6,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), eia2ta[7,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia2ta[8,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia2ta[9,])
)
ggsave("figures/null-models/me_eia2.pdf", plot_me(me_eia2ta))

me_oata = rbind(
  calculate_model_error(average_two(eia2.edge, oa.edge, groups), oata[2,]),
  calculate_model_error(average_two(oa.edge, aa.edge, groups), oata[3,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), oata[4,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), oata[5,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), oata[6,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), oata[7,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), oata[8,])
)
ggsave("figures/null-models/me_oa.pdf", plot_me(me_oata))

me_aata = rbind(
  calculate_model_error(average_two(oa.edge, aa.edge, groups), aata[2,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), aata[3,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), aata[4,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), aata[5,])
)
ggsave("figures/null-models/me_aa.pdf", plot_me(me_aata))

all_me_ta = rbind(me_eia1eta, me_eia1lta, me_eia2ta, me_oata, me_aata)
ggsave("figures/null-models/all_ta_me.pdf", plot_me(all_me_ta), height = 4, width = 7)
