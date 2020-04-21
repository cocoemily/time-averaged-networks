calc.node.in = function(g) {
  return(degree(g, mode = "in"))
}

calc.node.out = function(g) {
  return(degree(g, mode = "out"))
}

calc.node.deg = function(g) {
  return(degree(g, mode = "all"))
}

calc.deg.dist = function(g) {
  return(degree_distribution(g))
}

calc.eigen = function(g) {
  ec = eigen_centrality(g)$vector
  ecdf = as.data.frame(ec)
  ecdf$node = as.numeric(rownames(ecdf))
  return(ecdf)
}

calc.btwn = function(g) {
  return(betweenness(g))
}

calc.close = function(g) {
  return(closeness(g))
}