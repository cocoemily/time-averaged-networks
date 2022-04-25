# Visualize Sensitivy Analysis Results in Point and Box Plots

# name of dataset
dtName = "ICRATES"

# data specific plot attributes
pointsize = 0.1 # size of points
alphavalue = 0.4 # opacity of points

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes"
source("Scripts/sensitivity-analysis-summary-visualisation-points.R")

# # Get output visuals - potential impact of missing edges
# dtAnalysis = "edges"
# source("Scripts/sensitivity-analysis-summary-visualisation-points.R")


