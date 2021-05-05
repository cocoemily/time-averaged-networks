#Chaco
source("Scripts/Chaco-time-average.R")
Chaco_original_graphs = graphs
Chaco_ta_graphs = chaco_tagraphs

#ICRATES
source("Scripts/ICRATES-time-average.R")
ICRATES_original_graphs = graphs
ICRATES_ta_graphs = icrates_tagraphs

#Prignano
source("Scripts/Prignano-time-average.R")
Prignano_original_graphs = original_graphs
Prignano_ta_graphs = Prignano_ta

rm(list = setdiff(ls(), c("Chaco_original_graphs", "Chaco_ta_graphs",
                          "Prignano_original_graphs", "Prignano_ta_graphs",
                          "ICRATES_original_graphs", "ICRATES_ta_graphs")))

save(Chaco_original_graphs, Chaco_ta_graphs, file = "Data/Chaco_graph_objects.RData")
save(Prignano_original_graphs, Prignano_ta_graphs, file = "Data/Prignano_graph_objects.RData")
save(ICRATES_original_graphs, ICRATES_ta_graphs, file = "Data/ICRATES_graph_objects.RData")


