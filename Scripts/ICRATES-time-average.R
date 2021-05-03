library(dplyr)
library(igraph)
library(reshape2)
library(utils)
source("scripts/network_metrics.R")
source("scripts/node_metrics.R")
source("scripts/network-analysis-functions.R")

##read in all data slices and make list of them
# slice1 = read.table("Data/ICRATES/time-slices/ICRATES_slice_1_range_-300to-280.csv", header = TRUE, row.names = 1, 
#                     sep = ",", check.names = FALSE)
# timeslices = list(slice1)

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

#'Helper function for creating the time-averaged graphs
time_average = function(graphs, start, end) {
  el = as_edgelist(graphs[[start]])
  for(i in (start+1):end) {
    el = rbind(el, as_edgelist(graphs[[i]]))
  }
  return(igraph::simplify(graph_from_edgelist(el, directed = F)))
}

#' Function for calculating network metrics for original and time-averaged graphs
#' Can only do time-averaging starting at the original graph and working forward through the list of all graphs
#' 
#' @param original starting network that all time-averaged networks will be compared to
#' @param index index in the graphs list of the original network
#' @param graphs list of all graphs in order to time-average among them
#' @param o_name string for identifying the original graph within the produced dataframe
#' 
#' @return df containing network metrics for original graph and all time-averaged graphs including the original graph
#' 
ICRATES_ta_compare = function(original, index, graphs, o_name) { 
  df = data.frame(cc = integer(), 
                  diam = integer(), 
                  edge.dens = integer(), 
                  btwn = integer(), 
                  eigen = integer(), 
                  path.length = integer(), 
                  size = integer(), 
                  mod = integer(), 
                  mean.deg = integer(), 
                  mean.in = integer(), 
                  mean.out = integer(), 
                  num.graphs = integer())
  df[nrow(df) + 1, ] = c(get_row(original), 1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      #print(i)
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, index, index+i)), i+1)
    }
    df$num.graphs = c(seq(1, nrow(df), by = 1))
    df$network = c(replicate(nrow(df), o_name))
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, index)), i+1)
    }
    df$num.graphs = c(1, seq(nrow(df), 2 , by = -1))
    df$network = c(replicate(nrow(df), o_name))
  } else { #TODO multiple direction time-averaging
    for(i in 1:(length(graphs)-index)) {
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, index, index+i)), i+1)
    }
    for(i in 1:(index-1)) {
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, index)), i+1)
    }
    df$network = c(replicate(nrow(df), o_name))
  }
  return(df)
}

names = as.data.frame(list.files("Data/ICRATES/time-slices", "*.csv"))
colnames(names) = "file"
names = names %>% tidyr::separate(file, into = c("i", "s", "n", "r", "dates"), sep = "_") 
names$dates = substr(names$dates, 1, nchar(names$dates)-4)
names = as.vector(names$dates)


