# Visualize Sensitivy Analysis Results in Heatmaps

# name of dataset
dtName = "Prignano"

# heigh of heatmap
rowheight = 380

# names of networks
network.names.full = c("EIA1E_network_1", "EIA1E_network_2", "EIA1E_network_3", "EIA1E_network_4", 
                       "EIA1E_network_5", "EIA1L_network_1", "EIA1L_network_2a", "EIA1L_network_2b", 
                       "EIA1L_network_3a", "EIA1L_network_3b", "EIA1L_network_4a", "EIA1L_network_4b", 
                       "EIA1L_network_5", "EIA2_network_1", "EIA2_network_2a", "EIA2_network_2b", 
                       "EIA2_network_3a", "EIA2_network_3b", "EIA2_network_3c", "EIA2_network_4a", 
                       "EIA2_network_4b", "EIA2_network_5", "OA_network_1", "OA_network_2a", 
                       "OA_network_2b", "OA_network_3a", "OA_network_3b", "OA_network_4a", 
                       "OA_network_4b", "OA_network_5", "AA_network_1", "AA_network_2", 
                       "AA_network_3", "AA_network_4", "AA_network_5")


# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes" 
source("Scripts/sensitivity-analysis-summary-visualisation-heatmap.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation-heatmap.R")

