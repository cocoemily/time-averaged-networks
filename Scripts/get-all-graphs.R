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

save(Chaco_original_graphs, file = "Data/Chaco_original_graph_objects.RData")
save(Chaco_ta_graphs, file = "Data/Chaco_ta_graph_objects.RData")
save(Prignano_original_graphs, Prignano_ta_graphs, file = "Data/Prignano_graph_objects.RData")
save(ICRATES_original_graphs, file = "Data/ICRATES_original_graph_objects.RData")
first = ICRATES_ta_graphs[1:12]
second = ICRATES_ta_graphs[13:25]
third = ICRATES_ta_graphs[26:38]
fourth = ICRATES_ta_graphs[39:50]
save(first, file = "Data/ICRATES_ta_graph_objects_1.RData")
save(second, file = "Data/ICRATES_ta_graph_objects_2.RData")
save(third, file = "Data/ICRATES_ta_graph_objects_3.RData")
save(fourth, file = "Data/ICRATES_ta_graph_objects_4.RData")


