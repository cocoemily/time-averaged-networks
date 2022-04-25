# Visualize Sensitivy Analysis Results in Heatmaps

# name of dataset
dtName = "ICRATES"

# heigh of heatmap
rowheight = 1000 # or potentially larger, please change as you see fit.

# names of networks
load("/scratch/ec3307/time-averaged-networks/Data/icrates_network_names.RData")
network.names.full = icrates.net.names.full$period_network

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes" 
source("Scripts/sensitivity-analysis-summary-visualisation-heatmap.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation-heatmap.R")

