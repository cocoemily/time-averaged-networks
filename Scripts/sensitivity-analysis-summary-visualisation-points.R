# setwd("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/Time_Averaging/time-averaged-networks_Dec21")
# library(ggplot2)
source("Scripts/sensitivity-analysis-summary-visualisation-data.R")

# Spearman's Rho summary data
# degree cetrality summary data
melted_median_dc_avg = melted_median_dc[melted_median_dc$Var1 != 1,]; melted_median_dc_avg$statistic = "median"
melted_IQR_dc_avg = melted_IQR_dc[melted_IQR_dc$Var1 != 1,]; melted_IQR_dc_avg$statistic = "IQR"
melted_dc_avg = rbind.data.frame(melted_median_dc_avg, melted_IQR_dc_avg); melted_dc_avg$metric = "degree centrality"
# eigenvector centrality summary data
melted_median_ec_avg = melted_median_ec[melted_median_ec$Var1 != 1,]; melted_median_ec_avg$statistic = "median"
melted_IQR_ec_avg = melted_IQR_ec[melted_IQR_ec$Var1 != 1,]; melted_IQR_ec_avg$statistic = "IQR"
melted_ec_avg = rbind.data.frame(melted_median_ec_avg, melted_IQR_ec_avg); melted_ec_avg$metric = "eigenvector centrality"
# betweeness centrality summary data
melted_median_bc_avg = melted_median_bc[melted_median_bc$Var1 != 1,]; melted_median_bc_avg$statistic = "median"
melted_IQR_bc_avg = melted_IQR_bc[melted_IQR_bc$Var1 != 1,]; melted_IQR_bc_avg$statistic = "IQR"
melted_bc_avg = rbind.data.frame(melted_median_bc_avg, melted_IQR_bc_avg); melted_bc_avg$metric = "betweeness centrality"
# bind data
melted_avg_summary = rbind.data.frame(melted_dc_avg, melted_ec_avg, melted_bc_avg)

# Actual value summary data
# clustering coefficient summary data
melted_cc$statistic = "value"; melted_cc$metric = "clustering coefficient"
# modularity summary data
melted_mo$statistic = "value"; melted_mo$metric = "modularity"
# bind data
melted_avg_actual = rbind.data.frame(melted_cc, melted_mo)

# plot specifications
yaxislab_1 = "Spearman's Rho"
yaxislab_2 = "Value"
jitterpos = position_jitter(width = 0.2, height = 0, seed = 1)

# plot
ps1 = ggplot(melted_avg_summary, aes(x = Var2, y = value)) + 
        geom_boxplot(color = "#50505080", size = 0.8, outlier.size = 0, outlier.stroke = 0) +
        geom_point(alpha = alphavalue, size = pointsize, position = jitterpos, aes(col = Var1)) +
        labs(x = "Sampling fraction", y = yaxislab_1, color = "number\nof\ngraphs") +
        facet_wrap(~statistic+metric) +
        theme_minimal() + 
        scale_color_gradient(low = "blue", high = "orange")

ps2 = ggplot(melted_avg_actual, aes(x = Var2, y = value)) + 
        geom_boxplot(color = "#50505080", size = 0.8, outlier.size = 0, outlier.stroke = 0) +
        geom_point(alpha = alphavalue, size = pointsize, position = jitterpos, aes(col = Var1)) +
        labs(x = "Sampling fraction", y = yaxislab_2, color = "number\nof\ngraphs") +
        facet_wrap(~metric) +
        theme_minimal() + 
        scale_color_gradient(low = "blue", high = "orange")

# save plots
ggsave(paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_SA_viz_", dtAnalysis, "_summary_value.png"), plot = ps1, height = 10, width = 10)
ggsave(paste0("figures/sensitivity-analysis/", dtName, "/", dtName, "_SA_viz_", dtAnalysis, "_actual_value.png"), plot = ps2, height = 5, width = 10)





