library(dplyr)
library(igraph)
source("scripts/network_metrics.R")
source("scripts/node_metrics.R")

chaco = read.csv("Data/Chaco/AllCeramics.csv")
chaco_id = chaco %>% group_by(SWSN_ID) %>%
  summarize(Site = first(Site))

ad800 = read.csv("Data/Chaco/AD800cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad850 = read.csv("Data/Chaco/AD850cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad900 = read.csv("Data/Chaco/AD900cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad950 = read.csv("Data/Chaco/AD950cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1000 = read.csv("Data/Chaco/AD1000cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1050 = read.csv("Data/Chaco/AD1050cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1100 = read.csv("Data/Chaco/AD1100cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1150 = read.csv("Data/Chaco/AD1150cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1200 = read.csv("Data/Chaco/AD1200cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))
ad1250 = read.csv("Data/Chaco/AD1250cer.csv") %>% left_join(chaco_id, by = c("X" = "Site"))

#make networks -- 25 years
create_network = function(chaco_tp) {
  wares = as.character(unique(chaco_tp$SWSN_Ware))
  sites = as.numeric(unique(chaco_tp
                            $SWSN_ID))
  mat = matrix(0, nrow = length(sites), ncol = length(sites))
  rownames(mat) = sites
  colnames(mat) = sites
  for(w in wares) {
    nodes = chaco_tp%>% filter(SWSN_Ware == w)
    for(i in 1:nrow(nodes)){
      for(j in 1:nrow(nodes)) {
        if(i != j) {
          mat[as.character(nodes$SWSN_ID[i]), as.character(nodes$SWSN_ID[j])] =
            as.numeric(mat[as.character(nodes$SWSN_ID[i]), as.character(nodes$SWSN_ID[j])] + 1)
        }
      }
    }
  }
  return(graph_from_adjacency_matrix(mat, mode = "undirected", weighted = T))
}

chaco800 = create_network(chaco %>% filter(P800 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P800))
chaco825 = create_network(chaco %>% filter(P825 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P825))
chaco850 = create_network(chaco %>% filter(P850 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P850))
chaco875 = create_network(chaco %>% filter(P875 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P875))
chaco900 = create_network(chaco %>% filter(P900 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P900))
chaco925 = create_network(chaco %>% filter(P925 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P925))
chaco950 = create_network(chaco %>% filter(P950 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P950))
chaco975 = create_network(chaco %>% filter(P975 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P975))
chaco1000 = create_network(chaco %>% filter(P1000 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1000))
chaco1025 = create_network(chaco %>% filter(P1025 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1025))
chaco1050 = create_network(chaco %>% filter(P1050 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1050))
chaco1075 = create_network(chaco %>% filter(P1075 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1075))
chaco1100 = create_network(chaco %>% filter(P1100 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1100))
chaco1125 = create_network(chaco %>% filter(P1125 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1125))
chaco1150 = create_network(chaco %>% filter(P1150 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1150))
chaco1175 = create_network(chaco %>% filter(P1175 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1175))
chaco1200 = create_network(chaco %>% filter(P1200 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1200))
chaco1225 = create_network(chaco %>% filter(P925 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1225))
chaco1250 = create_network(chaco %>% filter(P950 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1250))
chaco1275 = create_network(chaco %>% filter(P975 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1275))

graphs = list(chaco800, chaco825, chaco850, chaco875, 
              chaco900, chaco925, chaco950, chaco975,
              chaco1000, chaco1025, chaco1050, chaco1075,
              chaco1100, chaco1125, chaco1150, chaco1175,
              chaco1200, chaco1225, chaco1250, chaco1275)

#'Helper function for creating the time-averaged graphs
time_average = function(graphs, start, end) {
  el = as_edgelist(graphs[[start]])
  for(i in (start+1):end) {
    el = rbind(el, as_edgelist(graphs[[i]]))
  }
  return(simplify(graph_from_edgelist(el, directed = F)))
}

#'Helper function for calculating all relevant network indices
get_row = function(graph) {
  return(c(calc.cc(graph), calc.diam(graph), calc.edge.dens(graph), 
           calc.mean.between(graph),
           calc.mean.eigen(graph), calc.mean.path.length(graph), 
           calc.S(graph), calc.mod(graph), calc.mean.deg(graph), 
           calc.mean.in(graph), calc.mean.out(graph)))
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
Chaco_ta_compare = function(original, index, graphs, o_name, backward = FALSE) { 
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
                  mean.out = integer())
  df[nrow(df) + 1, ] = get_row(original)
  if(!backward) { 
    for(i in 1:(length(graphs)-index)) {
      #print(i)
      df[nrow(df) + 1, ] = get_row(time_average(graphs, index, index+i))
    }
    df$num.graphs = c(seq(1, nrow(df), by = 1))
    df$network = c(replicate(nrow(df), o_name))
  }
  # else { #TO DO: time-average in opposite direction
  #   for(i in length(graphs):index)) {
  #     df[nrow(df) + 1, ] = get_row(time_average(graphs, ))
  #   }
  # }
  return(df)
}

#test = Chaco_ta_compare(graphs[[1]], 1, graphs, "chaco800")
