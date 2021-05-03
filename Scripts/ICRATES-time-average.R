library(dplyr)
library(igraph)
library(reshape2)
library(utils)

source("scripts/time-average-functions.R")

##read in all data slices and make list of them
files = list.files("Data/ICRATES/time-slices", "*.csv", full.names = TRUE)
timeslices = list()
for(f in 1:length(files)) {
  newslice = read.table(files[f], header = TRUE, row.names = 1, 
                        sep = ",", check.names = FALSE)
  timeslices[[f]] = newslice
}


create_edge_list = function(slice) {
  df_incidence = slice
  # rownames(df_incidence)
  # colnames(df_incidence)
  df_incidence = ifelse(df_incidence > 0, TRUE, FALSE)
  
  ##filter out columns where everything is false
  df_incidence = as.data.frame(df_incidence[, as.vector(which(colSums(df_incidence == FALSE) != nrow(df_incidence)))])
  
  df_links = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_links) = c("source", "target", "weight")
  
  for (i in 1:(dim(df_incidence)[1]-1)) {
    df1 = df_incidence[i,]
    for (j in (i+1):dim(df_incidence)[1]) {
      df2 = df_incidence[j,]
      df_i = cbind.data.frame(rownames(df_incidence)[i], rownames(df_incidence)[j], sum(df1 & df2))
      colnames(df_i) = c("source", "target", "weight")
      df_links = rbind.data.frame(df_links, df_i)
    }
  }
  df_links = df_links[df_links$weight > 0,]
  df_links$source = as.character(df_links$source)
  df_links$target = as.character(df_links$target)
  
  return(df_links)
}

graphs = list()
for(s in 1:10) {
  el = create_edge_list(timeslices[[s]])
  graphs[[s]] = igraph::simplify(graph_from_edgelist(as.matrix(el[,1:2]), directed=FALSE))
}
for(s in 11:20) {
  el = create_edge_list(timeslices[[s]])
  graphs[[s]] = igraph::simplify(graph_from_edgelist(as.matrix(el[,1:2]), directed=FALSE))
}
for(s in 21:30) {
  el = create_edge_list(timeslices[[s]])
  graphs[[s]] = igraph::simplify(graph_from_edgelist(as.matrix(el[,1:2]), directed=FALSE))
}
for(s in 31:40) {
  el = create_edge_list(timeslices[[s]])
  graphs[[s]] = igraph::simplify(graph_from_edgelist(as.matrix(el[,1:2]), directed=FALSE))
}
for(s in 41:length(timeslices)) {
  el = create_edge_list(timeslices[[s]])
  graphs[[s]] = igraph::simplify(graph_from_edgelist(as.matrix(el[,1:2]), directed=FALSE))
}


names = as.data.frame(list.files("Data/ICRATES/time-slices", "*.csv"))
colnames(names) = "file"
names = names %>% tidyr::separate(file, into = c("i", "s", "n", "r", "dates"), sep = "_") 
names$dates = substr(names$dates, 1, nchar(names$dates)-4)
names = as.vector(names$dates)


#ICRATES time-averaged graphs
tagraphs = get_ta_graphs(graphs[[1]], 1, graphs)
for(i in 2:length(graphs)) {
  tagraphs = get_ta_graphs(graphs[[i]], i, graphs)
}

