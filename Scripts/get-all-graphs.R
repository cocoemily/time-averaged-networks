# ##Chaco
# source("Scripts/Chaco-time-average.R")
# Chaco_original_graphs = graphs
# Chaco_ta_graphs = tagraphs
# 
# #ICRATES
# source("Scripts/ICRATES-time-average.R")
# ICRATES_original_graphs = graphs
# ICRATES_ta_graphs = tagraphs
# 
# rm(list = setdiff(ls(), c("Chaco_original_graphs", "Chaco_ta_graphs", 
#                           "Prignano_original_graphs", "Prignano_ta_graphs", 
#                           "ICRATES_original_graphs", "ICRATES_ta_graphs")))
# 
# save(Chaco_original_graphs, Chaco_ta_graphs, 
#      #Prignano_original_graphs, Prignano_ta_graphs, 
#      ICRATES_original_graphs, ICRATES_ta_graphs, file = "Data/chaco-icrates_graph_objects.RData")


#Load in Chaco and ICRATES graphs made with above code on HPC
load("Data/chaco-icrates_graph_objects.RData")


#Prignano
source("Scripts/Prignano-time-average.R")
Prignano_original_graphs = original_graphs
Prignano_ta_graphs = Prignano_ta

rm(list = setdiff(ls(), c("Chaco_original_graphs", "Chaco_ta_graphs",
                          "Prignano_original_graphs", "Prignano_ta_graphs",
                          "ICRATES_original_graphs", "ICRATES_ta_graphs")))


