calc.node.in = function(g) {
  return(degree(g, mode = "in"))
}

calc.node.out = function(g) {
  return(degree(g, mode = "out"))
}

calc.node.deg = function(g) {
  return(degree(g, mode = "deg"))
}

calc.deg.dist = function(g) {
  return(degree_distribution(g))
}