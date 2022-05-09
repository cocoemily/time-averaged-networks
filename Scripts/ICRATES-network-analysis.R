source("Scripts/time-average-functions.R")
source("Scripts/ICRATES-time-average.R")
source("Scripts/network-analysis-functions.R")

library(ggthemes)
library(dplyr)
library(ggplot2)
library(tidyverse)

theme_set(theme_minimal())

load("Data/ICRATES/timeslice_levels.RData")
lvls_names = paste0(c(letters, paste0("z", letters[1:24])), ":", lvls)
df_lvls = cbind.data.frame("lvls" = lvls, "network" = lvls_names)

##all graphs
comp.dfs = list()
for(g in 1:length(graphs)) {
  comp.dfs[[g]] = ICRATES_ta_compare(graphs[[g]], g, graphs, names[g])
}

i_alldata = comp.dfs[[1]]
for(i in 2:length(comp.dfs)) {
  i_alldata = rbind(i_alldata, comp.dfs[[i]])
}
write.csv(i_alldata, file = "output/ICRATES/ta-network-metrics.csv")

#find graphs that are not fully connected
ncgraphs = list()
ncnames = list()
index = 1
for(i in 1:length(graphs)) {
  if(!is.null(graphs[[i]])) {
  if(clique_num(graphs[[i]]) != vcount(graphs[[i]])) {
    ncgraphs[[index]] = graphs[[i]]
    ncnames[[index]] = names[[i]]
    index = index + 1
  }
  }
}
# 
# comp.dfs2 = list()
# for(g in 1:length(ncgraphs)) {
#   comp.dfs2[[g]] = ICRATES_ta_compare(ncgraphs[[g]], g, ncgraphs, ncnames[g])
# }
# 
# i_alldata2 = comp.dfs2[[1]]
# for(i in 2:length(comp.dfs2)) {
#   i_alldata2 = rbind(i_alldata, comp.dfs2[[i]])
# }

####Original graphs analysis####
metric.list = c("size", "diam", "mean.deg", "mean.in", "mean.out", "edge.dens", "path.length", "cc", "mod", "btwn", "eigen")
iod = i_alldata %>% filter(num.graphs == 1) %>% gather(key = "metric", value = "value", c(1:11))
iod$network = factor(iod$network, levels = lvls)
iod$metric = factor(iod$metric, levels = metric.list)
ioplot = ggplot(iod, aes(y = value, x = network, color = network)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))
ggsave("figures/metrics/ICRATES/original-network-metrics.pdf", ioplot, width = 8, height = 5)

# iod2 = i_alldata2 %>% filter(num.graphs == 1) %>% gather(key = "metric", value = "value", c(1:11))
# iod2$network = factor(iod2$network, levels = lvls)
# iod2$metric = factor(iod2$metric, levels = metric.list)
# ioplot2 = ggplot(iod2, aes(y = value, x = network, color = network)) +
#   geom_point() +
#   facet_wrap(~metric, scales = "free_y") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank())
# ggsave("figures/metrics/ICRATES/noncomplete_original-network-metrics.pdf", ioplot2, width = 8, height = 5)

####PCA Analysis####
i_alldata = na.omit(i_alldata)
colnames(i_alldata)[which(colnames(i_alldata) == "network")] = "lvls"
i_alldata = merge(i_alldata, df_lvls, by = "lvls")
head(i_alldata)
write.csv(get_pca_variable_contribs(i_alldata, c("btwn", "eigen", "mean.deg", "cc", "mod", "path.length")), file = "output/ICRATES/pca-dim-contribs.csv")
ipca = pca_biplot(i_alldata, c("btwn", "eigen", "mean.deg", "cc", "mod", "path.length")) +
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))
ggsave("figures/pca/ICRATES/pca-biplot.pdf", ipca)



####Model Errors####
##comparison to original graph

#starting at graph 2 because graph 1 is empty
modelerrors = calculate_model_error(graphs[[1]], comp.dfs[[1]])
for(g in 2:(length(graphs))) {
  print(g) #to see which graph is breaking it
  modelerrors = rbind(modelerrors, calculate_model_error(graphs[[g]], comp.dfs[[g]]))
}
write.csv(modelerrors, file = "output/ICRATES/model-errors_ta-to-orig.csv")
ggsave("figures/null-models/ICRATES/me_ta-to-orig.pdf", plot_model_errors(modelerrors, c("btwn_me", "eigen_me", "cc_me", "mod_me", "diam_me")), height = 4, width = 7)
ggsave("figures/null-models/ICRATES/me_perc_ta-to-orig.pdf", plot_model_errors_bars(modelerrors, c("btwn_me", "eigen_me", "cc_me", "mod_me", "diam_me"), labsize = 0), height = 4, width = 7)

# ncmodelerrors = calculate_model_error(ncgraphs[[1]], comp.dfs2[[1]])
# for(g in 2:(length(ncgraphs)-1)) {
#   ncmodelerrors = rbind(ncmodelerrors, calculate_model_error(ncgraphs[[g]], comp.dfs2[[g]]))
# }
# ggsave("figures/null-models/ICRATES/noncomplete_me_ta-to-orig.pdf", plot_model_errors(ncmodelerrors, c("btwn_me", "eigen_me", "cc_me", "mod_me", "diam_me")), height = 4, width = 7)

#this analysis no longer included in paper
# modelerrors2 = calculate_model_error(graphs[[1]], comp.dfs[[1]])
# for(index in 1:length(graphs)) {
#   if(index == 1) { 
#     for(i in 2:(length(graphs)-index)) {
#       modelerrors2 = rbind(modelerrors2, calculate_model_error(time_average(graphs, index, index+i), comp.dfs[[index]]))
#     }
#   } else if(index == length(graphs)) {
#     modelerrors2 = rbind(modelerrors2, calculate_model_error(graphs[[index]], comp.dfs[[index]]))
#     for(i in 1:(length(graphs)-1)) {
#       modelerrors2 = rbind(modelerrors2, calculate_model_error(time_average(graphs, i, index), comp.dfs[[index]]))
#     }
#   } else { 
#     modelerrors2 = rbind(modelerrors2, calculate_model_error(graphs[[index]], comp.dfs[[index]]))
#     for(i in 1:(length(graphs)-index)) {
#       modelerrors2 = rbind(modelerrors2, calculate_model_error(time_average(graphs, index, index+i), comp.dfs[[index]]))
#     }
#     for(i in 1:(index-1)) {
#       modelerrors2 = rbind(modelerrors2, calculate_model_error(time_average(graphs, i, index), comp.dfs[[index]]))
#     }
#   }
# }
# me1 = modelerrors2[1:276238,]
# me2 = modelerrors2[276239:552476,]
# me3 = modelerrors2[552477:828715,]
# me4 = modelerrors2[828716:nrow(modelerrors2),]
# write.csv(me1, file = "output/ICRATES/model-errors_ta-to-ta_1.csv")
# write.csv(me2, file = "output/ICRATES/model-errors_ta-to-ta_2.csv")
# write.csv(me3, file = "output/ICRATES/model-errors_ta-to-ta_3.csv")
# write.csv(me4, file = "output/ICRATES/model-errors_ta-to-ta_4.csv")
# ggsave("figures/null-models/ICRATES/all_ta_me.pdf", plot_model_errors(modelerrors2, c("btwn_me", "eigen_me", "cc_me", "mod_me", "diam_me")), height = 4, width = 7)


