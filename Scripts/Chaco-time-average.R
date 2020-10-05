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

chaco800 = create_network(chaco %>% filter(P800 != 0) %>% select(SWSN_ID, SWSN_Ware, P800))
chaco825 = create_network(chaco %>% filter(P825 != 0) %>% select(SWSN_ID, SWSN_Ware, P825))
chaco850 = create_network(chaco %>% filter(P850 != 0) %>% select(SWSN_ID, SWSN_Ware, P850))
chaco875 = create_network(chaco %>% filter(P875 != 0) %>% select(SWSN_ID, SWSN_Ware, P875))
chaco900 = create_network(chaco %>% filter(P900 != 0) %>% select(SWSN_ID, SWSN_Ware, P900))
chaco925 = create_network(chaco %>% filter(P925 != 0) %>% select(SWSN_ID, SWSN_Ware, P925))
chaco950 = create_network(chaco %>% filter(P950 != 0) %>% select(SWSN_ID, SWSN_Ware, P950))
chaco975 = create_network(chaco %>% filter(P975 != 0) %>% select(SWSN_ID, SWSN_Ware, P975))
chaco1000 = create_network(chaco %>% filter(P1000 != 0) %>% select(SWSN_ID, SWSN_Ware, P1000))
chaco1025 = create_network(chaco %>% filter(P1025 != 0) %>% select(SWSN_ID, SWSN_Ware, P1025))
chaco1050 = create_network(chaco %>% filter(P1050 != 0) %>% select(SWSN_ID, SWSN_Ware, P1050))
chaco1075 = create_network(chaco %>% filter(P1075 != 0) %>% select(SWSN_ID, SWSN_Ware, P1075))
chaco1100 = create_network(chaco %>% filter(P1100 != 0) %>% select(SWSN_ID, SWSN_Ware, P1100))
chaco1125 = create_network(chaco %>% filter(P1125 != 0) %>% select(SWSN_ID, SWSN_Ware, P1125))
chaco1150 = create_network(chaco %>% filter(P1150 != 0) %>% select(SWSN_ID, SWSN_Ware, P1150))
chaco1175 = create_network(chaco %>% filter(P1175 != 0) %>% select(SWSN_ID, SWSN_Ware, P1175))
chaco1200 = create_network(chaco %>% filter(P1200 != 0) %>% select(SWSN_ID, SWSN_Ware, P1200))
chaco1225 = create_network(chaco %>% filter(P925 != 0) %>% select(SWSN_ID, SWSN_Ware, P1225))
chaco1250 = create_network(chaco %>% filter(P950 != 0) %>% select(SWSN_ID, SWSN_Ware, P1250))
chaco1275 = create_network(chaco %>% filter(P975 != 0) %>% select(SWSN_ID, SWSN_Ware, P1275))

graphs = list(chaco800, chaco825, chaco850, chaco875, 
              chaco900, chaco925, chaco950, chaco975,
              chaco1000, chaco1025, chaco1050, chaco1075,
              chaco1100, chaco1125, chaco1150, chaco1175,
              chaco1200, chaco1225, chaco1250, chaco1275)
#up to 25 graphs to be combined together
time_average = function(graphs, start, end) {
  el = as_edgelist(graphs[[start]])
  for(i in (start+1):end) {
    el = rbind(el, as_edgelist(graphs[[i]]))
  }
  return(simplify(graph_from_edgelist(el, directed = F)))
}
#test = time_average(graphs, 1, 2)

get_row = function(graph) {
  return(c(calc.cc(graph), calc.diam(graph), calc.edge.dens(graph), 
           calc.mean.between(graph),
           calc.mean.eigen(graph), calc.mean.path.length(graph), 
           calc.S(graph), calc.mod(graph), calc.mean.deg(graph), 
           calc.mean.in(graph), calc.mean.out(graph)))
}

#can do time averaging starting at given graph and moving forward
Chaco_ta_compare = function(original, index, graphs, o_name) { 
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
  for(i in 1:(length(graphs)-index)) {
    print(i)
    df[nrow(df) + 1, ] = get_row(time_average(graphs, index, index+i))
  }
  df$name = c(o_name, seq(index+1, nrow(df), by = 1))
}
