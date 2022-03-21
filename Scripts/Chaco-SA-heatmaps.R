# Visualize Sensitivy Analysis Results in Heatmaps

# name of dataset
dtName = "Chaco"

# heigh of heatmap
rowheight = 380

# names of networks
network.names.full = NULL # please hard code the network names <<------

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes" 
source("Scripts/sensitivity-analysis-summary-visualisation.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation.R")


