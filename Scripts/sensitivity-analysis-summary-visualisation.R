source("Scripts/sensitivity-analysis-summary-visualisation-functions.R")

# load data according to the input
SA_results = readRDS(paste0("Data/sensitivity_analysis_", dtAnalysis, "_", dtName, ".rds"))

# define colors
brewerset_Con = brewer.pal(11, name = "BrBG")[c(1:4, 8:11)]
newcol_Con = colorRampPalette(brewerset_Con)
brewerset_Cat = brewer.pal(9, name = "BuPu")
newcol_Cat = colorRampPalette(brewerset_Cat)

# number of networks
dNets = length(SA_results)

# sampling fraction
samp.frac = c("S90", "S80", "S70", "S60", "S50", "S40", "S30", "S20", "S10")
dSF = length(samp.frac)

# initialize results data
list_res = initialize_data(dNets = dNets, dSF = dSF, samp.frac = samp.frac, network.names.full = NULL)

# loop over all networks
for (i in 1:dNets) {
  
  # data per network
  SA.rs.i = SA_results[[i]]
  SA.rs.i.v1 = as.data.frame(SA.rs.i[[1]])
  SA.rs.i.v2 = as.data.frame(SA.rs.i[[2]])
  SA.rs.i.v3 = as.data.frame(SA.rs.i[[3]])
  SA.rs.i.v4 = as.data.frame(SA.rs.i[[4]])
  SA.rs.i.v5 = as.data.frame(SA.rs.i[[5]])
  
  # initialize outliers data 1D vector
  voutliers.1 = rep(NA, dSF)
  voutliers.2 = rep(NA, dSF)
  voutliers.3 = rep(NA, dSF)
  
  # get summary results: median
  vmedian.1 = apply(SA.rs.i.v1, 2, fmedian)
  vmedian.2 = apply(SA.rs.i.v2, 2, fmedian)
  vmedian.3 = apply(SA.rs.i.v3, 2, fmedian)
  
  # get summary results: standard deviation
  vsd.1 = apply(SA.rs.i.v1, 2, fsd)
  vsd.2 = apply(SA.rs.i.v2, 2, fsd)
  vsd.3 = apply(SA.rs.i.v3, 2, fsd)
  
  # get summary results: interquartile range
  vIQR.1 = apply(SA.rs.i.v1, 2, fIQR)
  vIQR.2 = apply(SA.rs.i.v2, 2, fIQR)
  vIQR.3 = apply(SA.rs.i.v3, 2, fIQR)
  
  # get summary results: number of outliers
  for(j in 1:dSF) {
    voutliers.1[j] = foutliers(SA.rs.i.v1[,j], vIQR.1[j])
    voutliers.2[j] = foutliers(SA.rs.i.v2[,j], vIQR.2[j])
    voutliers.3[j] = foutliers(SA.rs.i.v3[,j], vIQR.3[j])
  }
  
  # update data in results list
  list_res$df_median_dc[i,] = vmedian.1
  list_res$df_sd_dc[i,] = vsd.1
  list_res$df_IQR_dc[i,] = vIQR.1
  list_res$df_outliers_dc[i,] = voutliers.1
  
  list_res$df_median_ec[i,] = vmedian.2
  list_res$df_sd_ec[i,] = vsd.2
  list_res$df_IQR_ec[i,] = vIQR.2
  list_res$df_outliers_ec[i,] = voutliers.2
  
  list_res$df_median_bc[i,] = vmedian.3
  list_res$df_sd_bc[i,] = vsd.3
  list_res$df_IQR_bc[i,] = vIQR.3
  list_res$df_outliers_bc[i,] = voutliers.3
  
  list_res$df_cc[i,] = SA.rs.i.v4
  list_res$df_modularity[i,] = SA.rs.i.v5
}

# melt data
# degree cetrality
melted_median_dc = melt(as.matrix(list_res$df_median_dc)); melted_median_dc$value = round(melted_median_dc$value, 2)
melted_sd_dc = melt(as.matrix(list_res$df_sd_dc)); melted_sd_dc$value = round(melted_sd_dc$value, 2)
melted_IQR_dc = melt(as.matrix(list_res$df_IQR_dc)); melted_IQR_dc$value = round(melted_IQR_dc$value, 2)
melted_outliers_dc = melt(as.matrix(list_res$df_outliers_dc)); melted_outliers_dc$value = round(melted_outliers_dc$value, 2)
# eigenvector centrality
melted_median_ec = melt(as.matrix(list_res$df_median_ec)); melted_median_ec$value = round(melted_median_ec$value, 2)
melted_sd_ec = melt(as.matrix(list_res$df_sd_ec)); melted_sd_ec$value = round(melted_sd_ec$value, 2)
melted_IQR_ec = melt(as.matrix(list_res$df_IQR_ec)); melted_IQR_ec$value = round(melted_IQR_ec$value, 2)
melted_outliers_ec = melt(as.matrix(list_res$df_outliers_ec)); melted_outliers_ec$value = round(melted_outliers_ec$value, 2)
# betweeness centrality
melted_median_bc = melt(as.matrix(list_res$df_median_bc)); melted_median_bc$value = round(melted_median_bc$value, 2)
melted_sd_bc = melt(as.matrix(list_res$df_sd_bc)); melted_sd_bc$value = round(melted_sd_bc$value, 2)
melted_IQR_bc = melt(as.matrix(list_res$df_IQR_bc)); melted_IQR_bc$value = round(melted_IQR_bc$value, 2)
melted_outliers_bc = melt(as.matrix(list_res$df_outliers_bc)); melted_outliers_bc$value = round(melted_outliers_bc$value, 2)
# clustering coefficient
melted_cc = melt(as.matrix(list_res$df_cc)); melted_cc$value = round(melted_cc$value, 2)
# modularity
melted_mo = melt(as.matrix(list_res$df_mo)); melted_mo$value = round(melted_mo$value, 2)

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

