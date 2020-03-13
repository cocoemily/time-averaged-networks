###Network Metrics
library(igraph)

#closeness centrality
calc.mean.close <- function(g) {
  close <- closeness(g)
  return(mean(close))
}
#betweenness centrality
calc.mean.between <- function(g) {
  btwn <- betweenness(g)
  return(mean(btwn))
}
#eigenvector centrality
calc.mean.eigen <- function(g) {
  eigen <- eigen_centrality(g)
  return(mean(eigen$vector))
}
#average path length
calc.mean.path.length <- function(g) {
  avpath <- igraph::average.path.length(g)
  return(avpath)
}
#diameter
calc.diam <- function(g) {
  diam <- diameter(g)
  return(diam)
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
  return(mean(degree(g, mode = "all")))
}
#mean in degree
calc.mean.in = function(g) {
  return(mean(degree(g, mode = "in")))
}
#mean out degree
calc.mean.out = function(g) {
  return(mean(degree(g, mode = "out")))
}
