source("scripts/Chaco-time-average.R")
source("scripts/node_metrics.R")
library(OmicsMarkeR)
library(ggthemes)

#probably going to do node metrics on only a subset of graphs, since there would be so many otherwise
get_ta_graphs = function(original, index, graphs, o_name) {
  gl = list(original)
  ngl = list(1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      #print(i)
      gl = c(gl, time_average(graphs, index, index+i))
      ngl = c(ngl, i+1)
    }
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      gl = c(gl, time_average(graphs, i, index))
      ngl = c(ngl, i+1)
    }
  } else { 
    for(i in 1:(length(graphs)-index)) {
      gl = c(gl, time_average(graphs, index, index+i))
      ngl = c(ngl, i+1)
    }
    for(i in 1:(index-1)) {
      gl = c(gl, time_average(graphs, i, index))
      ngl = c(ngl, i+1)
    }
  }
  return(list(gl, ngl))
}

#this is not working for some reason
c800 = get_ta_graphs(graphs[[1]], 1, graphs)
plot(plot_jaccard_similarity(
  jaccard_similarity_df(as.list(c800[[1]]), c800[[2]], top = T, FUN = calc.node.deg, "chaco800")
))
