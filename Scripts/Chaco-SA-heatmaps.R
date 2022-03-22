# Visualize Sensitivy Analysis Results in Heatmaps

# name of dataset
dtName = "Chaco"

# heigh of heatmap
rowheight = 380

# names of networks
load("../Data/chaco_network_names.RData")
network.names.full = chaco.net.names.full$period_network

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes" 
source("Scripts/sensitivity-analysis-summary-visualisation.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation.R")


