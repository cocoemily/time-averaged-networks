library(igraph)

calc.node.in = function(g) {
  din = as.data.frame(degree(g, mode = "in"))
  din$node = rownames(din)
  colnames(din) = c("metric", "node")
  return(din)
}

calc.node.out = function(g) {
  dout = as.data.frame(degree(g, mode = "out"))
  dout$node = rownames(dout)
  colnames(dout) = c("metric", "node")
  return(dout)
}

calc.node.deg = function(g) {
  dall = as.data.frame(degree(g, mode = "all"))
  dall$node = rownames(dall)
  colnames(dall) = c("metric", "node")
  return(dall)
}

calc.eigen = function(g) {
  ec = eigen_centrality(g)$vector
  ecdf = as.data.frame(ec)
  ecdf$node = as.numeric(rownames(ecdf))
  colnames(ecdf) = c("metric", "node")
  return(ecdf)
}

calc.btwn = function(g) {
  bdf = as.data.frame(betweenness(g))
  bdf$node = as.numeric(rownames(bdf))
  colnames(bdf) = c("metric", "node")
  return(bdf)
}

# calc.close = function(g) {
#   return(closeness(g))
# }

calc.deg.dist = function(g) {
  ddist = as.data.frame(degree.distribution(g))
  ddist$degree = as.numeric(rownames(ddist))
  colnames(ddist) = c("freq", "degree")
  return(ddist)
}
