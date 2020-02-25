library(igraph)
library(tidyverse)
source("scripts/network_metrics.R")

#read in data and clean
aa = read.csv("Data/Prignano/AA.gdf")
aa.edge = aa[181:nrow(aa),1:3]
colnames(aa.edge) = c("node1", "node2", "weight")
aa.node = aa[-c(180:nrow(aa)),c(1,3:4)]
colnames(aa.node) = c("name", "lat", "long")
aa.node$age = "aa"
aa.edge$age = "aa"

eia1e = read.csv("Data/Prignano/EIA1E.gdf")
eia1e.edge = eia1e[118:nrow(eia1e),1:3]
colnames(eia1e.edge) = c("node1", "node2", "weight")
eia1e.node = eia1e[-c(117:nrow(eia1e)),c(1,3:4)]
colnames(eia1e.node) = c("name", "lat", "long")
eia1e.node$age = "eia1e"
eia1e.edge$age = "eia1e"

eia1l = read.csv("Data/Prignano/EIA1L.gdf")
eia1l.edge = eia1l[117:nrow(eia1l),1:3]
colnames(eia1l.edge) = c("node1", "node2", "weight")
eia1l.node = eia1l[-c(116:nrow(eia1l)),c(1,3:4)]
colnames(eia1l.node) = c("name", "lat", "long")
eia1l.node$age = "eia1l"
eia1l.edge$age = "eia1l"

eia2 = read.csv("Data/Prignano/EIA2.gdf")
eia2.edge = eia2[132:nrow(eia2),1:3]
colnames(eia2.edge) = c("node1", "node2", "weight")
eia2.node = eia2[-c(131:nrow(eia2)),c(1,3:4)]
colnames(eia2.node) = c("name", "lat", "long")
eia2.node$age = "eia2"
eia2.edge$age = "eia2"

oa = read.csv("Data/Prignano/OA.gdf")
oa.edge = oa[170:nrow(oa),1:3]
colnames(oa.edge) = c("node1", "node2", "weight")
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

#### time averaging functions to create combined graphs ####
average_two = function(e1, e2, groups) {
  edge1 = create_new_edge_list(e1, groups)
  edge2 = create_new_edge_list(e2, groups)
  edges = rbind(edge1, edge2)
  graph = simplify(graph_from_edgelist(as.matrix(edges[,5:6])))
  return(graph)
}

average_three = function(e1, e2, e3, groups) {
  edge1 = create_new_edge_list(e1, groups)
  edge2 = create_new_edge_list(e2, groups)
  edge3 = create_new_edge_list(e3, groups)
  e = rbind(edge1, edge2, edge3)
  graph = simplify(graph_from_edgelist(as.matrix(e[,5:6])))
  return(graph)
}

average_four = function(e1, e2, e3, e4, groups) {
  edge1 = create_new_edge_list(e1, groups)
  edge2 = create_new_edge_list(e2, groups)
  edge3 = create_new_edge_list(e3, groups)
  edge4 = create_new_edge_list(e4, groups)
  e = rbind(edge1, edge2, edge3, edge4)
  graph = simplify(graph_from_edgelist(as.matrix(e[,5:6])))
  return(graph)
}

average_five = function(e1, e2, e3, e4, e5, groups) {
  edge1 = create_new_edge_list(e1, groups)
  edge2 = create_new_edge_list(e2, groups)
  edge3 = create_new_edge_list(e3, groups)
  edge4 = create_new_edge_list(e4, groups)
  edge5 = create_new_edge_list(e5, groups)
  e = rbind(edge1, edge2, edge3, edge4, edge5)
  graph = simplify(graph_from_edgelist(as.matrix(e[,5:6])))
  return(graph)
}

#### analysis ####
#functions to create dataframe for comparison
ta_compare = function(edge1, edge2, edge3 = NULL, edge4 = NULL, edge5 = NULL, 
                      groups) {
  df = data.frame(cc = integer(), 
                  diam = integer(), 
                  edge.dens = integer(), 
                  btwn = integer(), 
                  close = integer(), 
                  eigen = integer(), 
                  path.length = integer(), 
                  size = integer())
  g1 = NULL
  g2 = NULL
  g3 = NULL
  g4 = NULL
  g5 = NULL
  ga = NULL
  
  if(is.null(edge3) & is.null(edge4) & is.null(edge5)) {
    g1 = graph_from_edgelist(as.matrix(create_new_edge_list(edge1, groups)[,5:6]))
    g2 = graph_from_edgelist(as.matrix(create_new_edge_list(edge2, groups)[,5:6]))
    ga = average_two(edge1, edge2, groups)
  }else if(is.null(edge4) & is.null(edge5)) {
    g1 = graph_from_edgelist(as.matrix(create_new_edge_list(edge1, groups)[,5:6]))
    g2 = graph_from_edgelist(as.matrix(create_new_edge_list(edge2, groups)[,5:6]))
    g3 = graph_from_edgelist(as.matrix(create_new_edge_list(edge3, groups)[,5:6]))
    ga = average_three(edge1, edge2, edge3, groups)
  }else if(is.null(edge5)) {
    g1 = graph_from_edgelist(as.matrix(create_new_edge_list(edge1, groups)[,5:6]))
    g2 = graph_from_edgelist(as.matrix(create_new_edge_list(edge2, groups)[,5:6]))
    g3 = graph_from_edgelist(as.matrix(create_new_edge_list(edge3, groups)[,5:6]))
    g4 = graph_from_edgelist(as.matrix(create_new_edge_list(edge4, groups)[,5:6]))
    ga = average_four(edge1, edge2, edge3, edge4, groups)
  }else {
    g1 = graph_from_edgelist(as.matrix(create_new_edge_list(edge1, groups)[,5:6]))
    g2 = graph_from_edgelist(as.matrix(create_new_edge_list(edge2, groups)[,5:6]))
    g3 = graph_from_edgelist(as.matrix(create_new_edge_list(edge3, groups)[,5:6]))
    g4 = graph_from_edgelist(as.matrix(create_new_edge_list(edge4, groups)[,5:6]))
    g5 = graph_from_edgelist(as.matrix(create_new_edge_list(edge5, groups)[,5:6]))
    ga = average_five(edge1, edge2, edge3, edge4, edge5, groups)
  }
  
  if(is.null(g3) & is.null(g4) & is.null(g5)) {
    df[nrow(df) + 1, ] = get_row(g1)
    df[nrow(df) + 1, ] = get_row(g2)
    df[nrow(df) + 1, ] = get_row(ga)
    df$names = c(edge1$age[1], edge2$age[1], "ta2")
  }else if (is.null(g4) & is.null(g5)) {
    df[nrow(df) + 1, ] = get_row(g1)
    df[nrow(df) + 1, ] = get_row(g2)
    df[nrow(df) + 1, ] = get_row(g3)
    df[nrow(df) + 1, ] = get_row(ga)
    df$names = c(edge1$age[1], edge2$age[1], edge3$age[1], "ta3")
  }else if(is.null(g5)){
    df[nrow(df) + 1, ] = get_row(g1)
    df[nrow(df) + 1, ] = get_row(g2)
    df[nrow(df) + 1, ] = get_row(g3)
    df[nrow(df) + 1, ] = get_row(g4)
    df[nrow(df) + 1, ] = get_row(ga)
    df$names = c(edge1$age[1], edge2$age[1], edge3$age[1], edge4$age[1], "ta4")
  }else {
    df[nrow(df) + 1, ] = get_row(g1)
    df[nrow(df) + 1, ] = get_row(g2)
    df[nrow(df) + 1, ] = get_row(g3)
    df[nrow(df) + 1, ] = get_row(g4)
    df[nrow(df) + 1, ] = get_row(g5)
    df[nrow(df) + 1, ] = get_row(ga)
    df$names = c(edge1$age[1], edge2$age[1], edge3$age[1], 
                 edge4$age[1], edge5$age[1], "ta5")
  }
  return(df)
}

get_row = function(graph) {
  return(c(calc.cc(graph), calc.diam(graph), calc.edge.dens(graph), 
           calc.mean.between(graph), calc.mean.close(graph), 
           calc.mean.eigen(graph), calc.mean.path.length(graph), 
           calc.S(graph)))
}

##analysis
ta2 = ta_compare(eia1e.edge, eia1l.edge, groups = groups)
ta3 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, groups = groups)
ta4 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups = groups)
ta5 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups = groups)
