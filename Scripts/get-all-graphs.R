#Chaco
source("Scripts/Chaco-time-average.R")
Chaco_original_graphs = graphs
Chaco_ta_graphs = chaco_tagraphs

#ICRATES
# source("Scripts/ICRATES-time-average.R")
# ICRATES_original_graphs = graphs
# ICRATES_ta_graphs = icrates_tagraphs

#Prignano
#source("Scripts/Prignano-time-average.R")
#Prignano_original_graphs = original_graphs
#Prignano_ta_graphs = Prignano_ta

rm(list = setdiff(ls(), c("Chaco_original_graphs", "Chaco_ta_graphs",
                          "Prignano_original_graphs", "Prignano_ta_graphs",
                          "ICRATES_original_graphs", "ICRATES_ta_graphs")))

save(Chaco_original_graphs, file = "Data/Chaco_original_graph_objects.RData")
c1 = Chaco_ta_graphs[1:10]
c2 = Chaco_ta_graphs[11:15]
c3 = Chaco_ta_graphs[16:20]

save(c1, file = "Data/Chaco_ta_graph_objects_1.RData")
save(c2, file = "Data/Chaco_ta_graph_objects_2.RData")
save(c3, file = "Data/Chaco_ta_graph_objects_3.RData")

#save(Prignano_original_graphs, Prignano_ta_graphs, file = "Data/Prignano_graph_objects.RData")

# save(ICRATES_original_graphs, file = "Data/ICRATES_original_graph_objects.RData")
# 
# i1 = ICRATES_ta_graphs[1:2]
# i2 = ICRATES_ta_graphs[3:4]
# i3 = ICRATES_ta_graphs[5:6]
# i4 = ICRATES_ta_graphs[7:8]
# i5 = ICRATES_ta_graphs[9:10]
# i6 = ICRATES_ta_graphs[11:12]
# i7 = ICRATES_ta_graphs[13:14]
# i8 = ICRATES_ta_graphs[15:16]
# i9 = ICRATES_ta_graphs[17:18]
# i10 = ICRATES_ta_graphs[19]
# i11 = ICRATES_ta_graphs[20]
# i12 = ICRATES_ta_graphs[21]
# i13 = ICRATES_ta_graphs[22]
# i14 = ICRATES_ta_graphs[23:24]
# i15 = ICRATES_ta_graphs[25:26]
# i16 = ICRATES_ta_graphs[27:28]
# i17 = ICRATES_ta_graphs[29:30]
# i18 = ICRATES_ta_graphs[31:32]
# i19 = ICRATES_ta_graphs[33:34]
# i20 = ICRATES_ta_graphs[35:36]
# i21 = ICRATES_ta_graphs[37:38]
# i22 = ICRATES_ta_graphs[39:40]
# i23 = ICRATES_ta_graphs[41:42]
# i24 = ICRATES_ta_graphs[43:44]
# i25 = ICRATES_ta_graphs[45:46]
# i26 = ICRATES_ta_graphs[47:48]
# i27 = ICRATES_ta_graphs[49:50]
# 
# save(i1, file = "Data/ICRATES_ta_graph_objects_1.RData")
# save(i2, file = "Data/ICRATES_ta_graph_objects_2.RData")
# save(i3, file = "Data/ICRATES_ta_graph_objects_3.RData")
# save(i4, file = "Data/ICRATES_ta_graph_objects_4.RData")
# save(i5, file = "Data/ICRATES_ta_graph_objects_5.RData")
# save(i6, file = "Data/ICRATES_ta_graph_objects_6.RData")
# save(i7, file = "Data/ICRATES_ta_graph_objects_7.RData")
# save(i8, file = "Data/ICRATES_ta_graph_objects_8.RData")
# save(i9, file = "Data/ICRATES_ta_graph_objects_9.RData")
# save(i10, file = "Data/ICRATES_ta_graph_objects_10.RData")
# save(i11, file = "Data/ICRATES_ta_graph_objects_11.RData")
# save(i12, file = "Data/ICRATES_ta_graph_objects_12.RData")
# save(i13, file = "Data/ICRATES_ta_graph_objects_13.RData")
# save(i14, file = "Data/ICRATES_ta_graph_objects_14.RData")
# save(i15, file = "Data/ICRATES_ta_graph_objects_15.RData")
# save(i16, file = "Data/ICRATES_ta_graph_objects_16.RData")
# save(i17, file = "Data/ICRATES_ta_graph_objects_17.RData")
# save(i18, file = "Data/ICRATES_ta_graph_objects_18.RData")
# save(i19, file = "Data/ICRATES_ta_graph_objects_19.RData")
# save(i20, file = "Data/ICRATES_ta_graph_objects_20.RData")
# save(i21, file = "Data/ICRATES_ta_graph_objects_21.RData")
# save(i22, file = "Data/ICRATES_ta_graph_objects_22.RData")
# save(i23, file = "Data/ICRATES_ta_graph_objects_23.RData")
# save(i24, file = "Data/ICRATES_ta_graph_objects_24.RData")
# save(i25, file = "Data/ICRATES_ta_graph_objects_25.RData")
# save(i26, file = "Data/ICRATES_ta_graph_objects_26.RData")
# save(i27, file = "Data/ICRATES_ta_graph_objects_27.RData")
