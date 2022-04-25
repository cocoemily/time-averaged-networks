# Visualize Sensitivy Analysis Results in Point and Box Plots

# name of dataset
dtName = "Prignano"

# Get output visuals - potential impact of missing nodes
dtAnalysis = "nodes"
source("Scripts/sensitivity-analysis-summary-visualisation-points.R")

# Get output visuals - potential impact of missing edges
dtAnalysis = "edges"
source("Scripts/sensitivity-analysis-summary-visualisation-points.R")
