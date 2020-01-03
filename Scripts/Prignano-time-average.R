library(igraph)

#read in data and clean
aa = read.csv("Data/Prignano/AA.gdf")
aa.edge = aa[181:nrow(aa),1:3]
colnames(aa.edge) = c("node1", "node2", "weight")
aa.node = aa[-c(180:nrow(aa)),c(1,3:4)]
colnames(aa.node) = c("name", "lat", "long")
aa.node$age = "aa"

eia1e = read.csv("Data/Prignano/EIA1E.gdf")
eia1e.edge = eia1e[118:nrow(eia1e),1:3]
colnames(eia1e.edge) = c("node1", "node2", "weight")
eia1e.node = eia1e[-c(117:nrow(eia1e)),c(1,3:4)]
colnames(eia1e.node) = c("name", "lat", "long")
eia1e.node$age = "eia1e"

eia1l = read.csv("Data/Prignano/EIA1L.gdf")
eia1l.edge = eia1l[117:nrow(eia1l),1:3]
colnames(eia1l.edge) = c("node1", "node2", "weight")
eia1l.node = eia1l[-c(116:nrow(eia1l)),c(1,3:4)]
colnames(eia1l.node) = c("name", "lat", "long")
eia1l.node$age = "eia1l"

eia2 = read.csv("Data/Prignano/EIA2.gdf")
eia2.edge = eia2[132:nrow(eia2),1:3]
colnames(eia2.edge) = c("node1", "node2", "weight")
eia2.node = eia2[-c(131:nrow(eia2)),c(1,3:4)]
colnames(eia2.node) = c("name", "lat", "long")
eia2.node$age = "eia2"

oa = read.csv("Data/Prignano/OA.gdf")
oa.edge = oa[170:nrow(oa),1:3]
colnames(oa.edge) = c("node1", "node2", "weight")
oa.node = oa[-c(169:nrow(oa)),c(1,3:4)]
colnames(oa.node) = c("name", "lat", "long")
oa.node$age = "oa"

#list of all nodes
all.nodes = rbind(aa.node, eia1e.node, eia1l.node, eia2.node, oa.node)
groups = all.nodes %>% group_by(lat, long) %>% 
  mutate(group = group_indices()) #group variable tells us if two nodes are the same

#time averaging functions
