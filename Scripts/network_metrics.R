###Network Metrics
library(igraph)

#betweenness centrality normalized by number of nodes
calc.mean.between <- function(g) {
  btwn <- betweenness(g, normalized = T)
  return(mean(btwn))
}
#eigenvector centrality 
calc.mean.eigen <- function(g) {
  eigen <- eigen_centrality(g)
  return(mean(eigen$vector))
}
#average path length normalized by size
calc.mean.path.length <- function(g) {
  avpath <- igraph::average.path.length(g)
  return(avpath/calc.S(g))
}
#diameter normalized by size
calc.diam <- function(g) {
  diam <- diameter(g)
  return(diam/calc.S(g))
}
#edge density
calc.edge.dens <- function(g) {
  ed <- edge_density(g)
  return(ed)
}
#size
calc.S <- function(g){
  S <- vcount(g)
  return(S)
}
#clustering coefficient
calc.cc <- function(g) {
  cc <- transitivity(g, "global")
  return(cc)
}
#modularity
calc.mod <- function(g) {
  wtc = cluster_walktrap(g)
  mod <- modularity(as.undirected(g), membership(wtc))
  return(mod)
}
#mean degree
calc.mean.deg = function(g) {
  return(mean(degree(g, mode = "all", normalized = T)))
}
#mean in degree
calc.mean.in = function(g) {
  return(mean(degree(g, mode = "in", normalized = T)))
}
#mean out degree
calc.mean.out = function(g) {
  return(mean(degree(g, mode = "out", normalized = T)))
}
