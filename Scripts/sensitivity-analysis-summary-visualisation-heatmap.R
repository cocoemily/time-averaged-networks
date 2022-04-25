source("Scripts/sensitivity-analysis-summary-visualisation-data.R")

# plots
# degree centrality
hc.dc.a = plot_hc_heatmap(melted_df = melted_median_dc, title = paste0("Median Degree Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.dc.b = plot_hc_heatmap(melted_df = melted_sd_dc, title = paste0("SD Degree Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.dc.c = plot_hc_heatmap(melted_df = melted_IQR_dc, title = paste0("IQR Degree Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.dc.d = plot_hc_heatmap(melted_df = melted_outliers_dc, title = paste0("Outliers Degree Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Cat, countdata = TRUE)
# eigenvector centrality
hc.ec.a = plot_hc_heatmap(melted_df = melted_median_ec, title = paste0("Median Eigenvector Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.ec.b = plot_hc_heatmap(melted_df = melted_sd_ec, title = paste0("SD Eigenvector Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.ec.c = plot_hc_heatmap(melted_df = melted_IQR_ec, title = paste0("IQR Eigenvector Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.ec.d = plot_hc_heatmap(melted_df = melted_outliers_ec, title = paste0("Outliers Eigenvector Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Cat, countdata = TRUE)
# betweeness centrality
hc.bc.a = plot_hc_heatmap(melted_df = melted_median_bc, title = paste0("Median Betweeness Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.bc.b = plot_hc_heatmap(melted_df = melted_sd_bc, title = paste0("SD Betweeness Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.bc.c = plot_hc_heatmap(melted_df = melted_IQR_bc, title = paste0("IQR Betweeness Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
hc.bc.d = plot_hc_heatmap(melted_df = melted_outliers_bc, title = paste0("Outliers Betweeness Centrality (", dtAnalysis, ")"), brewerset_Col = brewerset_Cat, countdata = TRUE)
# clustering coefficient
hc.cc = plot_hc_heatmap(melted_df = melted_cc, title = paste0("Clustering Coefficient (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)
# modularity
hc.mo = plot_hc_heatmap(melted_df = melted_mo, title = paste0("Modularity (", dtAnalysis, ")"), brewerset_Col = brewerset_Con, countdata = FALSE)

# arrange plots in one page
charts.dc = list(hc.dc.a, hc.dc.b, hc.dc.c, hc.dc.d)
hc.dc.grid = hw_grid(charts.dc, ncol = 2, rowheight = rowheight)
charts.ec = list(hc.ec.a, hc.ec.b, hc.ec.c, hc.ec.d)
hc.ec.grid = hw_grid(charts.ec, ncol = 2, rowheight = rowheight)
charts.bc = list(hc.bc.a, hc.bc.b, hc.bc.c, hc.bc.d)
hc.bc.grid = hw_grid(charts.bc, ncol = 2, rowheight = rowheight)

# save plots in html file
htmltools::save_html(hc.dc.grid, paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_viz_summary_DC_", dtAnalysis, ".html"))
htmltools::save_html(hc.ec.grid, paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_viz_summary_EC_", dtAnalysis, ".html"))
htmltools::save_html(hc.bc.grid, paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_viz_summary_BC_", dtAnalysis, ".html"))
htmltools::save_html(hc.cc, paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_viz_summary_CC_", dtAnalysis, ".html"))
htmltools::save_html(hc.mo, paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_viz_summary_MO_", dtAnalysis, ".html"))

