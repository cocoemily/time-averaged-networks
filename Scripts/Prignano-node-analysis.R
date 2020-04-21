#Prignano dataset node-based analysis
source("scripts/Prignano-time-average.R")
source("scripts/node_metrics.R")

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)

eia1l.graph = graph_from_edgelist(as.matrix(eia1l.edge[1:2]))
ta2 = average_two(eia1e.edge, eia1l.edge, groups)

 
orig = calc.close(eia1l.graph)
avg = calc.close(ta2)
View(orig)

plot(eia1l.graph, edge.arrow.size = 0.1, vertex.size = 10, 
     vertex.label.cex = 0.8)
