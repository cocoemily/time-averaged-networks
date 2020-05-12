#Prignano dataset network-level analysis
source("scripts/Prignano-time-average.R")
theme_set(theme_minimal())
library(ggthemes)
library(lme4)

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

fit1 = lm(mod ~ num.graphs + network, data = alldata)
summary(fit1)
fit2 = lm(mod ~ num.graphs, data = alldata)
fit3 = lmer(mod ~ num.graphs + (1 | network), data = alldata)
summary(fit3)
AICtab(fit2, fit3, base = T, weights = T)

talldata = alldata %>% gather(key = "measure", value = "value", c(1:11))
ggplot(talldata, aes(x = num.graphs, y = value, group = network, color = network)) +
  geom_smooth(se = F) + facet_wrap(. ~ measure, scales = "free_y")

pca_all = prcomp(alldata[,1:11])
pcadf = data.frame(pca_all$x)
pcadf$network = alldata$network
hulls = pcadf %>% group_by(network) %>% dplyr::slice(chull(PC1, PC2))
ggplot(pcadf, aes(x = PC1, y = PC2, fill = network, color = network)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.25)

avg = alldata %>% filter(num.graphs != 1)
orig = alldata %>% filter(num.graphs == 1)
pca_avg = prcomp(avg[,1:11])
avgdf = data.frame(pca_avg$x)
avgdf$network = avg$network
hulls = avgdf %>% group_by(network) %>% dplyr::slice(chull(PC1, PC2))
ggplot(avgdf, aes(x = PC1, y = PC2, fill = network, color = network)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.25)
  

####Null Model Analysis####
get_null_model_values = function(graph, FUN = calc.diam) {
  values = c()
  for(i in 1:1000) {
    ngraph = rewire(graph, each_edge(p = 1))
    values = c(values, FUN(ngraph))
  }
  return(values)
}

#create graphs
create_null_model_graph = function(graph, avg.data, orig.name) {
  p1 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.mean.between)), aes(x = metric)) +
    geom_histogram(binwidth = 0.001) +
    geom_vline(data = avg.data, aes(xintercept = btwn, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "betweeness", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  p2 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.mean.eigen)), aes(x = metric)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(data = avg.data, aes(xintercept = eigen, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "eigencentrality", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  p3 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.mean.path.length)), aes(x = metric)) +
    geom_histogram(binwidth = 0.005) +
    geom_vline(data = avg.data, aes(xintercept = path.length, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "path length", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  p4 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.diam)), aes(x = metric)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(data = avg.data, aes(xintercept = diam, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "diameter", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  p5 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.cc)), aes(x = metric)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(data = avg.data, aes(xintercept = cc, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "clustering coefficient", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  p6 = ggplot(data.frame(metric = get_null_model_values(graph, FUN = calc.mod)), aes(x = metric)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(data = avg.data, aes(xintercept = mod, color = as.factor(num.graphs))) +
    scale_color_colorblind() +
    labs(x = "modularity", color = "Networks Averaged", title = paste0("Original Graph: ", orig.name)) +
    theme(axis.title = element_text(size = 7), 
          title = element_text(size = 8), 
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 6))
  
  return(cowplot::plot_grid(p1, p2, p3, p4, p5, p6))
}

eia1e.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6]))
pall = create_null_model_graph(eia1e.graph, eia1eta, "EIA1E")
ggsave("figures/null-models/eia1e.png", pall, dpi = 300, width = 8.5, height = 5)

eia1l.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6]))
pall = create_null_model_graph(eia1l.graph, eia1lta, "EIA1L")
ggsave("figures/null-models/eia1l.png", pall, dpi = 300, width = 8.5, height = 5)

eia2.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia2.edge, groups)[,5:6]))
pall = create_null_model_graph(eia2.graph, eia2ta, "EIA2")
ggsave("figures/null-models/eia2.png", pall, dpi = 300, width = 8.5, height = 5)

oa.graph = graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6]))
pall = create_null_model_graph(oa.graph, oata, "OA")
ggsave("figures/null-models/oa.png", pall, dpi = 300, width = 8.5, height = 5)

aa.graph = graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6]))
pall = create_null_model_graph(aa.graph, aata, "AA")
ggsave("figures/null-models/aa.png", pall, dpi = 300, width = 8.5, height = 5)