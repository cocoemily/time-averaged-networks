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
calc_jaccard_similarity_high = function(compdf, num.graphs) { #calculate similarity between 
  comp.set = as.character(compdf[order(-compdf$metric),]$node[1:5])
  df = data.frame(num.graphs = integer(), 
                  sim = integer())
  
  for(i in 3:ncol(compdf)) {
    df[nrow(df) + 1, ] = c(num.graphs[i-2], jaccard(comp.set, as.character(compdf[order(-compdf[,i]),]$node[1:5])))
  }
  return(df)
}

calc_jaccard_similarity_low = function(compdf, num.graphs) { 
  comp.set = as.character(compdf[order(compdf$metric),]$node[1:5])
  df = data.frame(num.graphs = integer(), 
                  sim = integer())
  
  for(i in 3:ncol(compdf)) {
    df[nrow(df) + 1, ] = c(num.graphs[i-2], jaccard(comp.set, as.character(compdf[order(compdf[,i]),]$node[1:5])))
  }
  return(df)
}

get_comparison_dataframe = function(graphlist, suffixes, FUN = calc.btwn) {
  compdf = FUN(graphlist[[1]])
  for(i in 2:length(graphlist)) {
    compdf = compdf %>% full_join(FUN(graphlist[[i]]), by = "node", suffix = c("", suffixes[i-1]))
  }
  return(compdf)
}


#EIA1E analysis
eia1e.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1e.edge, groups)[,5:6]))
ta2.graph = average_two(eia1e.edge, eia1l.edge, groups)
ta3.graph = average_three(eia1e.edge, eia1l.edge, eia2.edge, groups)
ta4.graph = average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups)
ta5.graph = average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)

graphlist = list(eia1e.graph, ta2.graph, ta3.graph, ta4.graph, ta5.graph)
suffixes = c(".2", ".3", ".4", ".5")
compdf1e = get_comparison_dataframe(graphlist, suffixes, FUN = calc.btwn)
sim.df = calc_jaccard_similarity_high(compdf1e, c(2, 3, 4, 5))
sim.df$original = "eia1e"

#EIA1L analysis
eia1l.graph = graph_from_edgelist(as.matrix(create_new_edge_list(eia1l.edge, groups)[,5:6]))
ta2.graph1 = average_two(eia1e.edge, eia1l.edge, groups)
ta2.graph2 = average_two(eia1l.edge, eia2.edge, groups)
ta3.graph1 = average_three(eia1e.edge, eia1l.edge, eia2.edge, groups)
ta3.graph2 = average_three(eia1l.edge, eia2.edge, oa.edge, groups)
ta4.graph1 = average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups)
ta4.graph2 = average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)
ta5.graph = average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups)

graphlist = list(eia1e.graph, ta2.graph1, ta2.graph2, ta3.graph1, ta3.graph2, ta4.graph1, ta4.graph2, ta5.graph)
suffixes = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
compdf1l = get_comparison_dataframe(graphlist, suffixes, FUN = calc.btwn)
sim.df1l = calc_jaccard_similarity_high(compdf1l, c(2, 2, 3, 3, 4, 4, 5))
sim.df1l$original = "eia1l"

#EIA2 analysis
graphlist = list(graph_from_edgelist(as.matrix(create_new_edge_list(eia2.edge, groups)[,5:6])), 
                 average_two(eia1l.edge, eia2.edge, groups), 
                 average_two(eia2.edge, oa.edge, groups), 
                 average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), 
                 average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_three(eia2.edge, oa.edge, aa.edge, groups), 
                 average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
suffixes = c(".2.1", ".2.2", ".3.1", ".3.2", ".3.3", ".4.1", ".4.2", ".5")
compdf2 = get_comparison_dataframe(graphlist, suffixes, FUN = calc.btwn)
sim.df2 = calc_jaccard_similarity_high(compdf2, c(2, 2, 3, 3, 3, 4, 4, 5))
sim.df2$original = "eia2"

#OA analysis
graphlist = list(graph_from_edgelist(as.matrix(create_new_edge_list(oa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups), 
                 average_two(eia2.edge, oa.edge, groups), 
                 average_three(eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_three(eia2.edge, oa.edge, aa.edge, groups), 
                 average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), 
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
suffixes = c(".2.1", ".2.2", ".3.1", ".3.2", ".4.1", ".4.2", ".5")
compdfo = get_comparison_dataframe(graphlist, suffixes, FUN = calc.btwn)
sim.dfo = calc_jaccard_similarity_high(compdfo, c(2, 2, 3, 3, 4, 4, 5))
sim.dfo$original = "oa"

#AA analysis
graphlist = list(graph_from_edgelist(as.matrix(create_new_edge_list(aa.edge, groups)[,5:6])), 
                 average_two(oa.edge, aa.edge, groups),
                 average_three(eia2.edge, oa.edge, aa.edge, groups),
                 average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), 
                 average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups))
suffixes = c(".2", ".3", ".4", ".5")
compdfa = get_comparison_dataframe(graphlist, suffixes, FUN = calc.btwn)
sim.dfa = calc_jaccard_similarity_high(compdfa, c(2,3,4,5))
sim.dfa$original = "aa"



#all similarity measures
allsim = rbind(sim.df, sim.df1l, sim.df2, sim.dfo, sim.dfa)

simfit1 = lm(sim ~ num.graphs + original, data = allsim)
summary(simfit1)
simfit2 = lm(sim ~ num.graphs, data = allsim)
summary(simfit2)
simfit3 = lmer(sim ~ num.graphs + (1 | original), data = allsim)
summary(simfit3)
AICtab(simfit2, simfit3, base = T, weights = T)


