source("Scripts/time-average-functions.R")

library(dplyr)
library(igraph)

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
create_network_OLD = function(chaco_tp) {
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

create_network = function(chaco_tp) {
  wares = as.character(unique(chaco_tp$SWSN_Ware))
  sites = as.numeric(unique(chaco_tp
                            $SWSN_ID))
  mat = matrix(0, nrow = length(sites), ncol = length(sites))
  rownames(mat) = sites
  colnames(mat) = sites
  
  z <- reshape::cast(chaco_tp, SWSN_ID~SWSN_Ware)
  row.names(z) <- z[,1]
  z <- z[,-1]
  z[is.na(z)] <- 0
  z_p <- prop.table(as.matrix(z), margin = 1)*100
  row.names(z_p) = row.names(z)
  
  mat <- ((200 - as.matrix(vegan::vegdist(z_p, method = "manhattan"))) / 200)
  diag(mat) <- 0
  #mat = pmax(mat, 0)
  mat = round(mat, 3)
  
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
chaco1225 = create_network(chaco %>% filter(P1225 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1225))
chaco1250 = create_network(chaco %>% filter(P1250 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1250))
chaco1275 = create_network(chaco %>% filter(P1275 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1275))

graphs = list(chaco800, chaco825, chaco850, chaco875, 
              chaco900, chaco925, chaco950, chaco975,
              chaco1000, chaco1025, chaco1050, chaco1075,
              chaco1100, chaco1125, chaco1150, chaco1175,
              chaco1200, chaco1225, chaco1250, chaco1275)

CHACO_datasets = list(
  chaco %>% filter(P800 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P800),
  chaco %>% filter(P825 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P825),
  chaco %>% filter(P850 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P850),
  chaco %>% filter(P875 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P875),
  chaco %>% filter(P900 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P900),
  chaco %>% filter(P925 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P925),
  chaco %>% filter(P950 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P950),
  chaco %>% filter(P975 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P975), 
  chaco %>% filter(P1000 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1000),
  chaco %>% filter(P1025 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1025),
  chaco %>% filter(P1050 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1050),
  chaco %>% filter(P1075 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1075),
  chaco %>% filter(P1100 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1100),
  chaco %>% filter(P1125 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1125),
  chaco %>% filter(P1150 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1150),
  chaco %>% filter(P1175 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1175),
  chaco %>% filter(P1200 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1200),
  chaco %>% filter(P1225 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1225),
  chaco %>% filter(P1250 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1250),
  chaco %>% filter(P1275 != 0) %>% dplyr::select(SWSN_ID, SWSN_Ware, P1275)
)

time_average_CHACO = function(datasets, start, end) {
  newnet = datasets[[1]]
  colnames(newnet) = c(colnames(datasets[[1]])[1], colnames(datasets[[1]])[2], "period")
  for(i in 2:end) {
    newcols = datasets[[i]]
    colnames(newcols) = c(colnames(datasets[[i]])[1], colnames(datasets[[i]])[2], "period")
    newnet = rbind(newnet, newcols)
  }
  return(create_network(newnet))
}

#time-averaged graphs
chaco_tagraphs = list(get_ta_graphs(graphs[[1]], 1, graphs, Chaco = TRUE))
for(i in 2:length(graphs)) {
  chaco_tagraphs[[i]] = get_ta_graphs(graphs[[i]], i, graphs, Chaco = TRUE)
}
