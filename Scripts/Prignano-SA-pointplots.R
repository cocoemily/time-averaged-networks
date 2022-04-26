# Visualize Sensitivy Analysis Results in Point and Box Plots

# name of dataset
dtName = "Prignano"

# data specific plot attributes
pointsize = 2 # size of points
alphavalue = 0.7 # opacity of points

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes"
source("Scripts/sensitivity-analysis-summary-visualisation-points.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation-points.R")
