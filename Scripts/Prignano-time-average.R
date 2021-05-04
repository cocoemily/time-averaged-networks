source("Scripts/time-average-functions.R")

library(dplyr)
library(igraph)

#read in data and clean
aa = read.csv("Data/Prignano/AA.gdf")
aa.edge = aa[181:nrow(aa),1:3]
colnames(aa.edge) = c("node1", "node2", "weight")
aa.edge = aa.edge %>% mutate(node1 = trimws(node1), 
                             node2 = trimws(node2))
aa.node = aa[-c(180:nrow(aa)),c(1,3:4)]
colnames(aa.node) = c("name", "lat", "long")
aa.node$age = "aa"
aa.edge$age = "aa"

eia1e = read.csv("Data/Prignano/EIA1E.gdf")
eia1e.edge = eia1e[118:nrow(eia1e),1:3]
colnames(eia1e.edge) = c("node1", "node2", "weight")
eia1e.edge = eia1e.edge %>% mutate(node1 = trimws(node1), 
                             node2 = trimws(node2))
eia1e.node = eia1e[-c(117:nrow(eia1e)),c(1,3:4)]
colnames(eia1e.node) = c("name", "lat", "long")
eia1e.node$age = "eia1e"
eia1e.edge$age = "eia1e"

eia1l = read.csv("Data/Prignano/EIA1L.gdf")
eia1l.edge = eia1l[117:nrow(eia1l),1:3]
colnames(eia1l.edge) = c("node1", "node2", "weight")
eia1l.edge = eia1l.edge %>% mutate(node1 = trimws(node1), 
                                   node2 = trimws(node2))
eia1l.node = eia1l[-c(116:nrow(eia1l)),c(1,3:4)]
colnames(eia1l.node) = c("name", "lat", "long")
eia1l.node$age = "eia1l"
eia1l.edge$age = "eia1l"

eia2 = read.csv("Data/Prignano/EIA2.gdf")
eia2.edge = eia2[132:nrow(eia2),1:3]
colnames(eia2.edge) = c("node1", "node2", "weight")
eia2.edge = eia2.edge %>% mutate(node1 = trimws(node1), 
                                   node2 = trimws(node2))
eia2.node = eia2[-c(131:nrow(eia2)),c(1,3:4)]
colnames(eia2.node) = c("name", "lat", "long")
eia2.node$age = "eia2"
eia2.edge$age = "eia2"

oa = read.csv("Data/Prignano/OA.gdf")
oa.edge = oa[170:nrow(oa),1:3]
colnames(oa.edge) = c("node1", "node2", "weight")
oa.edge = oa.edge %>% mutate(node1 = trimws(node1), 
                                   node2 = trimws(node2))
oa.node = oa[-c(169:nrow(oa)),c(1,3:4)]
colnames(oa.node) = c("name", "lat", "long")
oa.node$age = "oa"
oa.edge$age = "oa"

#list of all nodes
all.nodes = rbind(aa.node, eia1e.node, eia1l.node, eia2.node, oa.node)
groups = all.nodes %>% group_by(lat, long) %>% 
  mutate(group = group_indices()) #group variable tells us if two nodes are the same

create_new_edge_list = function(edges, groups) {
  edges$node1 = as.character(edges$node1)
  edges$node2 = as.character(edges$node2)
  g1 = groups %>% filter(age == edges$age[1])
  g1$name = as.character(g1$name)
  
  edges$node1.n = edges$node1
  for(i in 1:nrow(edges)) {
    edges$node1.n[i] = g1$group[g1$name == trimws(edges$node1[i])]
  }
  edges$node2.n = edges$node2
  for(i in 1:nrow(edges)) {
    edges$node2.n[i] = g1$group[g1$name == trimws(edges$node2[i])]
  }
  return(edges)
}

####Graphs####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)
name.list = c("EIA1E", "EIA1L", "EIA2", "OA", "AA")

original_graphs = list(
  graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6])),
  graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6])), 
  graph_from_edgelist(as.matrix(create_new_edge_list(eia2.edge, groups)[,5:6])), 
  graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6])),
  graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6])) 
)

#EIA1E time-averaged graphs
gl1e = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6])), 
            average_two(eia1e.edge, eia1l.edge, groups), 
            average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
            average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
            average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl1e.ngl = list(1, 2, 3, 4, 5)

#EIA1L time-averaged graphs
gl1l = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6])), 
            average_two(eia1e.edge, eia1l.edge, groups), 
            average_two(eia1l.edge, eia2.edge, groups), 
            average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
            average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
            average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
            average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl1l.ngl = list(1, 2, 2, 3, 3, 4, 4, 5)

#EIA2 time-averaged graphs
gl2 = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia2.edge, groups)[,5:6])), 
           average_two(eia1l.edge, eia2.edge, groups), 
           average_two(eia2.edge, oa.edge, groups), 
           average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
           average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
           average_three(eia2.edge, oa.edge, aa.edge, groups), 
           average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
           average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
           average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gl2.ngl = list(1, 2, 2, 3, 3, 3, 4, 4, 5)

#OA time-averaged graphs
glo = list(graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6])), 
           average_two(oa.edge, aa.edge, groups), 
           average_two(eia2.edge, oa.edge, groups), 
           average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
           average_three(eia2.edge, oa.edge, aa.edge, groups), 
           average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
           average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
           average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
glo.ngl = list(1, 2, 2, 3, 3, 4, 4, 5)

#AA time-averaged graphs
gla = list(graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6])), 
           average_two(oa.edge, aa.edge, groups),
           average_three(eia2.edge, oa.edge, aa.edge, groups),
           average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
           average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
gla.ngl = list(1, 2, 3, 4, 5)


#all time-averaged graphs
ta_graphs = list(gl1e, gl1l, gl2, glo, gla)
ta_numbers = list(gl1e.ngl, gl1l.ngl, gl2.ngl, glo.ngl, gla.ngl)
Prignano_ta = list(ta_graphs, ta_numbers)

