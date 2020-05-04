#Prignano dataset node-based analysis
library(OmicsMarkeR)
source("scripts/Prignano-time-average.R")
source("scripts/node_metrics.R")

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)

compdf = calc.btwn(o1.graph)
compdf = compdf %>% full_join(calc.btwn(ta2.graph), by = "node", suffix = c("", ".2")) %>%
  full_join(calc.btwn(ta3.graph), by = "node", suffix = c("", ".3")) %>%
  full_join(calc.btwn(ta4.graph), by = "node", suffix = c("", ".4")) %>%
  full_join(calc.btwn(ta5.graph), by = "node", suffix = c("", ".5"))

calc_jaccard_similarity_high = function(compdf) { #calculate similarity between 
  set1 = as.character(compdf[order(-compdf$metric),]$node[1:5])
  set2 = as.character(compdf[order(-compdf$metric.2),]$node[1:5])
  set3 = as.character(compdf[order(-compdf$metric.3),]$node[1:5])
  set4 = as.character(compdf[order(-compdf$metric.4),]$node[1:5])
  set5 = as.character(compdf[order(-compdf$metric.5),]$node[1:5])
  
  df = data.frame(num.graphs = c(2,3,4,5), 
                  sim = c(jaccard(set1, set2), 
                          jaccard(set1, set3), 
                          jaccard(set1, set4), 
                          jaccard(set1, set5)))
  return(df)
}

calc_jaccard_similarity_low = function(compdf) { #calculate similarity between 
  set1 = as.character(compdf[order(compdf$metric),]$node[1:5])
  set2 = as.character(compdf[order(compdf$metric.2),]$node[1:5])
  set3 = as.character(compdf[order(compdf$metric.3),]$node[1:5])
  set4 = as.character(compdf[order(compdf$metric.4),]$node[1:5])
  set5 = as.character(compdf[order(compdf$metric.5),]$node[1:5])
  
  df = data.frame(num.graphs = c(2,3,4,5), 
                  sim = c(jaccard(set1, set2), 
                          jaccard(set1, set3), 
                          jaccard(set1, set4), 
                          jaccard(set1, set5)))
  return(df)
}

#EIA1E & EIA1L test analysis
eia1e.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6]))
ta2.graph = average_two(eia1e.edge, eia1l.edge, groups)
ta3.graph = average_three(eia1e.edge, eia1l.edge, eia2.edge, groups)
ta4.graph = average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups)
ta5.graph = average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)

compdf1 = calc.btwn(eia1e.graph)
compdf1 = compdf1 %>% full_join(calc.btwn(ta2.graph), by = "node", suffix = c("", ".2")) %>%
  full_join(calc.btwn(ta3.graph), by = "node", suffix = c("", ".3")) %>%
  full_join(calc.btwn(ta4.graph), by = "node", suffix = c("", ".4")) %>%
  full_join(calc.btwn(ta5.graph), by = "node", suffix = c("", ".5"))

sim.df = calc_jaccard_similarity_high(compdf1)
sim.df$original = "eia1e"

eia1l.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6]))
ta2.graph1 = average_two(eia1e.edge, eia1l.edge, groups)
ta2.graph2 = average_two(eia1l.edge, eia2.edge, groups)
ta3.graph1 = average_three(eia1e.edge, eia1l.edge, eia2.edge, groups)
ta3.graph2 = average_three(eia1l.edge, eia2.edge, oa.edge, groups)
ta4.graph1 = average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups)
ta4.graph2 = average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)
ta5.graph = average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)

compdf1 = calc.btwn(eia1l.graph)
compdf1 = compdf1 %>% full_join(calc.btwn(ta2.graph1), by = "node", suffix = c("", ".2.1")) %>%
  full_join(calc.btwn(ta3.graph), by = "node", suffix = c("", ".3")) %>%
  full_join(calc.btwn(ta4.graph), by = "node", suffix = c("", ".4")) %>%
  full_join(calc.btwn(ta5.graph), by = "node", suffix = c("", ".5"))
