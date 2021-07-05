library(rlist)

#'Helper function for creating the time-averaged graphs
time_average = function(graphs, start, end) {
  el = as_edgelist(graphs[[start]])
  for(i in (start+1):end) {
    el = rbind(el, as_edgelist(graphs[[i]]))
  }
  return(igraph::simplify(graph_from_edgelist(el, directed = F)))
}

get_ta_graphs = function(original, index, graphs) {
  gl = list(original)
  ngl = list(1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      gl = list.append(gl, time_average(graphs, index, index+i))
      #ngl = list.append(ngl, i+1)
    }
    num.graphs = seq(2, length(graphs), by = 1)
    for(n in num.graphs) { 
      ngl = list.append(ngl, n)
    }
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      gl = list.append(gl, time_average(graphs, i, index))
      #ngl = list.append(ngl, i+1)
    }
    num.graphs = seq(length(graphs), 2 , by = -1)
    for(n in num.graphs) { 
      ngl = list.append(ngl, n)
    }
  } else { 
    for(i in 1:index) {
      if(i != index) {
        for(j in index:length(graphs)) {
          gl = list.append(gl, time_average(graphs, i, j))
          ngl = list.append(ngl, (j-i+1))
        }
      } else {
        for(j in (index+1):length(graphs)) {
          gl = list.append(gl, time_average(graphs, i, j))
          ngl = list.append(ngl, (j-i+1))
        }
      }
    }
  }
  return(list(gl, ngl))
  #return(gl)
}

#' Function for calculating network metrics for original and time-averaged graphs
#' Can only do time-averaging starting at the original graph and working forward through the list of all graphs
#' 
#' @param original starting network that all time-averaged networks will be compared to
#' @param index index in the graphs list of the original network
#' @param graphs list of all graphs in order to time-average among them
#' @param o_name string for identifying the original graph within the produced dataframe
#' 
#' @return df containing network metrics for original graph and all time-averaged graphs including the original graph
#' 
ICRATES_ta_compare = function(original, index, graphs, o_name) { 
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
                  mean.out = integer(), 
                  num.graphs = integer())
  df[nrow(df) + 1, ] = c(get_row(original), 1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      #print(i)
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, index, index+i)), i+1)
    }
    df$num.graphs = c(seq(1, nrow(df), by = 1))
    df$network = c(replicate(nrow(df), o_name))
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, index)), i+1)
    }
    df$num.graphs = c(1, seq(nrow(df), 2 , by = -1))
    df$network = c(replicate(nrow(df), o_name))
  } else {
    for(i in 1:index) {
      if(i != index) {
        for(j in index:length(graphs)) {
          df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, j)), (j-i+1))
        }
      } else {
        for(j in (index+1):length(graphs)) {
          df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, j)), (j-i+1))
        }
      }
    }
    df$network = c(replicate(nrow(df), o_name))
  }
  return(df)
}

#' Function for calculating network metrics for original and time-averaged graphs
#' Can only do time-averaging starting at the original graph and working forward through the list of all graphs
#' 
#' @param original starting network that all time-averaged networks will be compared to
#' @param index index in the graphs list of the original network
#' @param graphs list of all graphs in order to time-average among them
#' @param o_name string for identifying the original graph within the produced dataframe
#' 
#' @return df containing network metrics for original graph and all time-averaged graphs including the original graph
#' 
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
                  mean.out = integer(), 
                  num.graphs = integer())
  df[nrow(df) + 1, ] = c(get_row(original), 1)
  if(index == 1) { 
    for(i in 1:(length(graphs)-index)) {
      #print(i)
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, index, index+i)), i+1)
    }
    df$num.graphs = c(seq(1, nrow(df), by = 1))
    df$network = c(replicate(nrow(df), o_name))
  } else if(index == length(graphs)) {
    for(i in 1:(length(graphs)-1)) {
      df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, index)), i+1)
    }
    df$num.graphs = c(1, seq(nrow(df), 2 , by = -1))
    df$network = c(replicate(nrow(df), o_name))
  } else { 
    # for(i in 1:(length(graphs)-index)) {
    #   df[nrow(df) + 1, ] = c(get_row(time_average(graphs, index, index+i)), i+1)
    # }
    # for(i in 1:(index-1)) {
    #   df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, index)), i+1)
    # }
    for(i in 1:index) {
      #print(i)
      if(i != index) {
        for(j in index:length(graphs)) {
          df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, j)), (j-i+1))
        }
      } else {
        for(j in (index+1):length(graphs)) {
          #print(j)
          df[nrow(df) + 1, ] = c(get_row(time_average(graphs, i, j)), (j-i+1))
        }
      }
    }
    df$network = c(replicate(nrow(df), o_name))
  }
  return(df)
}

#### time averaging functions to create combined graphs for Prignano ####
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

#functions to create dataframe for comparison
ta_compare = function(edge1, edge2, edge3 = NULL, edge4 = NULL, edge5 = NULL, 
                      groups) {
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