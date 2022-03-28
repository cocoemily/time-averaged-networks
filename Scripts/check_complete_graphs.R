library(dplyr)
library(igraph)
library(utils)

names = as.data.frame(list.files("Data/ICRATES/time-slices", "*.csv"))
colnames(names) = "file"
names = names %>% tidyr::separate(file, into = c("i", "s", "n", "r", "dates"), sep = "_") 
names$dates = substr(names$dates, 1, nchar(names$dates)-4)
names = as.vector(names$dates)

load("Data/ICRATES_original_graph_objects.RData")


checkdf = data.frame(graph = character(), 
                     num_nodes = numeric(), 
                     num_exp_edges = numeric(), 
                     num_obs_edges = numeric(), 
                     complete = logical())
check_complete = function(graph, name) {
  complete = FALSE
  nodes = gorder(graph)
  exp_edges = nodes*(nodes-1)/2
  obs_edges = gsize(graph)
  if(exp_edges == obs_edges) {
    complete = TRUE
  }
  return( c(name, nodes, exp_edges, obs_edges, complete))
}

for(i in 1:length(ICRATES_original_graphs)){
  #plot(ICRATES_original_graphs[[i]])
  checkdf[nrow(checkdf) + 1,] = check_complete(ICRATES_original_graphs[[i]], names[i])
}

checkdf$per_edges = as.numeric(checkdf$num_obs_edges)/as.numeric(checkdf$num_exp_edges)

