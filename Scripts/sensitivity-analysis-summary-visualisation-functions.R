
library(reshape2)
library(RColorBrewer)
library(highcharter)
library(htmltools)

# initialize summary results dataframes
initialize_data = function(dNets, dSF, samp.frac, network.names.full) {

  df_median_dc = df_median_ec = df_median_bc = 
    df_sd_dc = df_sd_ec = df_sd_bc = 
    df_IQR_dc = df_IQR_ec = df_IQR_bc = 
    df_outliers_dc = df_outliers_ec = df_outliers_bc = 
    as.data.frame(matrix(NA, nrow = dNets, ncol = dSF))
  colnames(df_median_dc) = colnames(df_median_ec) = colnames(df_median_bc) = 
    colnames(df_sd_dc) = colnames(df_sd_ec) = colnames(df_sd_bc) = 
    colnames(df_IQR_dc) = colnames(df_IQR_ec) = colnames(df_IQR_bc) = 
    colnames(df_outliers_dc) = colnames(df_outliers_ec) = colnames(df_outliers_bc) = samp.frac
  rownames(df_median_dc) = rownames(df_median_ec) = rownames(df_median_bc) = 
    rownames(df_sd_dc) = rownames(df_sd_ec) = rownames(df_sd_bc) = 
    rownames(df_IQR_dc) = rownames(df_IQR_ec) = rownames(df_IQR_bc) = 
    rownames(df_outliers_dc) = rownames(df_outliers_ec) = rownames(df_outliers_bc) = network.names.full
  df_cc = df_modularity = as.data.frame(matrix(NA, nrow = dNets, ncol = dSF+1))
  colnames(df_cc) = colnames(df_modularity) = c("S100", samp.frac)
  rownames(df_cc) = rownames(df_modularity) = network.names.full
  
  return(as.list(environment()))
  
}

# summary statistics functions
fmedian = function(x) {round(median(x, na.rm = TRUE), 3)}
fsd = function(x) {round(sd(x, na.rm = TRUE), 3)}
fIQR = function(x) {round(IQR(x, na.rm = TRUE), 3)}
foutliers = function(x, pIQR) {
  x = na.omit(x)
  sum(x < (quantile(x, 0.25) - 1.5*pIQR)) + sum(x > (quantile(x, 0.75) + 1.5*pIQR))
}

# heatmap plot function
plot_hc_heatmap = function(melted_df, title, brewerset_Col, countdata) {
  if(countdata) {
    colorset = newcol_Cat(length(na.omit(unique(melted_df$value))))
  } else {
    colorset = newcol_Con(length(na.omit(unique(melted_df$value))))   
  } 
  df_colorVar = cbind.data.frame(sort(na.omit(unique(melted_df$value))), colorset, stringsAsFactors = FALSE)
  df_colorVar = rbind(df_colorVar, c(NA, "#ffffff"))
  names(df_colorVar)[1] = "value"
  names(df_colorVar)[2] = "colvar"
  melted_df = merge(melted_df, df_colorVar, by = "value")
  hc = hchart(melted_df, 
              type = "heatmap", 
              hcaes(x = Var2, y = Var1, value = value, color = colvar)) %>%
    hc_xAxis(title = list(text = "Sampling fraction")) %>% 
    hc_yAxis(title = list(text = "All networks")) %>% 
    hc_tooltip(pointFormat = paste("Network: {point.y} <br> Spearman's Rho: {point.value}")) %>%
    hc_colorAxis(minColor = brewerset_Col[1], maxColor = brewerset_Col[length(brewerset_Col)],
                 stops = color_stops(length(brewerset_Col), brewerset_Col)) %>%
    hc_plotOptions(series = list(turboThreshold = ceiling(0.8*dim(melted_df)[1])))  %>% 
    hc_title(text = title)
  return(hc)
}

