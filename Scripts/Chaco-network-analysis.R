source("scripts/Chaco-time-average.R")
theme_set(theme_minimal())
library(ggthemes)
library(lme4)
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyverse)

c800 = Chaco_ta_compare(graphs[[1]], 1, graphs, "chaco800")
c825 = Chaco_ta_compare(graphs[[2]], 2, graphs, "chaco825")
c850 = Chaco_ta_compare(graphs[[3]], 3, graphs, "chaco850")
c875 = Chaco_ta_compare(graphs[[4]], 4, graphs, "chaco875")
c900 = Chaco_ta_compare(graphs[[5]], 5, graphs, "chaco900")
c925 = Chaco_ta_compare(graphs[[6]], 6, graphs, "chaco925")
c950 = Chaco_ta_compare(graphs[[7]], 7, graphs, "chaco950")
c975 = Chaco_ta_compare(graphs[[8]], 8, graphs, "chaco975")
c1000 = Chaco_ta_compare(graphs[[9]], 9, graphs, "chaco1000")
c1025 = Chaco_ta_compare(graphs[[10]], 10, graphs, "chaco1025")
c1050 = Chaco_ta_compare(graphs[[11]], 11, graphs, "chaco1050")
c1075 = Chaco_ta_compare(graphs[[12]], 12, graphs, "chaco1075")
c1100 = Chaco_ta_compare(graphs[[13]], 13, graphs, "chaco1100")
c1125 = Chaco_ta_compare(graphs[[14]], 14, graphs, "chaco1125")
c1150 = Chaco_ta_compare(graphs[[15]], 15, graphs, "chaco1150")
c1175 = Chaco_ta_compare(graphs[[16]], 16, graphs, "chaco1175")
c1200 = Chaco_ta_compare(graphs[[17]], 17, graphs, "chaco1200")
c1225 = Chaco_ta_compare(graphs[[18]], 18, graphs, "chaco1225")
c1250 = Chaco_ta_compare(graphs[[19]], 19, graphs, "chaco1250")
#c1275 = Chaco_ta_compare(graphs[[20]], 20, graphs, "chaco1275")

alldata = rbind(c800, c825, c850, c875, 
                  c900, c925, c950, c975,
                  c1000, c1025, c1050, c1075,
                  c1100, c1125, c1150, c1175,
                  c1200, c1225, c1250)

datalist = list(c800, c825, c850, c875, 
                c900, c925, c950, c975,
                c1000, c1025, c1050, c1075,
                c1100, c1125, c1150, c1175,
                c1200, c1225, c1250)

####PCA Analysis####
avg = alldata %>% filter(num_graphs != 1)
orig = alldata %>% filter(num_graphs == 1)

pca_avg = prcomp(avg[,c(1, 3:6, 8:9)], scale = T)
avgdf = data.frame(pca_avg$x)
avgdf$network = avg$name
hulls = avgdf %>% group_by(network) %>% dplyr::slice(chull(PC1, PC2))
ggplot(avgdf, aes(x = PC1, y = PC2, fill = network, color = network)) +
  geom_point() +
  coord_equal() +
  geom_polygon(data = hulls, alpha = 0.25)
print(pca_avg)
biplot(pca_avg)

orig.coord = as.data.frame(predict(pca_avg, orig[,c(1, 3:6, 8:9)]))
rownames(orig.coord) = as.character(orig$name)

p.pca = fviz_pca_biplot(pca_avg, repel = T, pointsize = 1, pointshape = 20, col.var = "grey30", col.ind = avg$name,
                        label = "var")
p.pca = fviz_add(p.pca, orig.coord, shape = 15, labelsize = 0)
#p.pca = fviz_add(p.pca, orig.coord[2,], shape = 15, color = "#56B4E9")
#p.pca = fviz_add(p.pca, orig.coord[3,], shape = 15, color = "#009E73")
#p.pca = fviz_add(p.pca, orig.coord[4,], shape = 15, color = "#F0E442")
#p.pca = fviz_add(p.pca, orig.coord[5,], shape = 15, color = "#0072B2")
plot(p.pca)
ggsave("figures/pca/Chaco/pca-biplot.pdf", p.pca)

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
  
  # null.pl = get_null_model_values(graph, FUN = calc.mean.path.length)
  # df$pl_me = ifelse(median(null.pl) > df$path.length, 
  #                   (median(null.pl) - df$path.length)/(quantile(null.pl, 0.975) - median(null.pl)), 
  #                   (median(null.pl) - df$path.length)/(median(null.pl) - quantile(null.pl, 0.025)))
  # 
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
  return(df)
}

##comparison to original graph
modelerrors = calculate_model_error(graphs[[1]], c800)
for(g in 2:(length(graphs)-1)) {
  modelerrors = rbind(modelerrors, calculate_model_error(graphs[[g]], datalist[[g]]))
}

plot_model_errors(modelerrors) {
  me = modelerrors %>% gather(key = "modelerror", value = "value", c(14:18))
  #me$name = factor(me$name, levels = c("chaco800", ....)) ##need to update levels
  metric.labs = c("betweenness centrality", "clustering coefficient", "diameter", 
                  "eigenvector centrality", "modularity", "path length", 
                  "edge density", "degree")
  names(metric.labs) = c("btwn_me", "cc_me", "diam_me", "eigen_me", "mod_me", "pl_me", "ed_me", "deg_me")
  meplot = ggplot(me, aes(x = num_graphs, y = value, group = name, color = name)) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, alpha = 0.05, color = NA, fill = "grey80") +
    geom_hline(yintercept = 0) +
    geom_smooth(se = F) +
    facet_wrap(~ modelerror, scales = "free_y", labeller = labeller(modelerror = metric.labs)) +
    labs(x = "number of graphs") +
    theme(axis.title.y = element_blank()) +
    scale_color_colorblind()
  return(meplot)
}

##need to implement comparison to time-averaged graphs -- look at Prignano script
