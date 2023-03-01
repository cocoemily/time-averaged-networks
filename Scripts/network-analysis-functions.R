source("Scripts/network_metrics.R")
source("Scripts/node_metrics.R")

library(factoextra)

#functions for running network-level analyses

#'Helper function for calculating all relevant network indices
get_row = function(graph) {
  return(c(calc.cc(graph), calc.diam(graph), calc.edge.dens(graph), 
           calc.mean.between(graph),
           calc.mean.eigen(graph), calc.mean.path.length(graph), 
           calc.S(graph), calc.mod(graph), calc.mean.deg(graph), 
           calc.mean.in(graph), calc.mean.out(graph)))
}

####PCA Analysis####
get_pca_variable_contribs = function(alldata, variables) {
  avg = alldata %>% filter(num.graphs != 1)
  pca_avg = prcomp(avg %>% dplyr::select(c(variables)), scale = T)
  res.var = get_pca_var(pca_avg)
  return(as.data.frame(res.var$contrib))
}


#'
#'Function produces a PCA biplot based on the time-averaged data on which the original data is plotted
#' @param alldata all of the network metric values for each original/time-averaged network
#' @param variables list specifying the network metrics of interest
#' @return PCA biplot graph
#' 
pca_biplot = function(alldata, variables) { 
  avg = alldata %>% filter(num.graphs != 1)
  orig = alldata %>% filter(num.graphs == 1)
  
  #pca_avg = prcomp(avg[,c(1, 3:6, 8:9)], scale = T)
  pca_avg = prcomp(avg %>% dplyr::select(c(variables)), scale = T)
  avgdf = data.frame(pca_avg$x)
  avgdf$network = avg$network
  
  orig.coord = as.data.frame(predict(pca_avg, orig %>% dplyr::select(c(variables))))
  rownames(orig.coord) = as.character(orig$network)
  p.pca = fviz_pca_biplot(pca_avg, repel = T, pointsize = 1, pointshape = 21, col.var = "contrib", fill.ind = avg$network,
                          label = "var", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  p.pca = fviz_add(p.pca, orig.coord, shape = 15, labelsize = 0)
  return(p.pca)
}

#'
#'Function produces a PCA biplot based on the time-averaged data on which the original data is plotted
#' @param alldata all of the network metric values for each original/time-averaged network
#' @param variables list specifying the network metrics of interest
#' @param top.a whether the first set of graphs should be plot last therefore be on top
#' @param pointsize numeric scalar defining the size of the points 
#' @param repel whether to repel or not
#' @param rlo smallest circle size
#' @param rup largest circle size
#' @param plot.nets selection of networks to be plotted; if NULL all networks are plotted
#' @return PCA biplot graph
#' 
pca_biplot3 = function(alldata, variables, top.a, pointsize, repel, rlo, rup, plot.nets) { 
  
  alldata = na.omit(alldata)
  alldata = alldata[order(alldata$network, decreasing = top.a), ]
  
  colorpal = RColorBrewer::brewer.pal(11, "Spectral")
  newcol_Cat = colorRampPalette(colorpal)
  colorcodes = newcol_Cat(length(unique(alldata$network)))
  df_colorcodes = data.frame("network" = sort(unique(alldata$network), decreasing = top.a), "colorcodes" = colorcodes)
  alldata = merge(alldata, df_colorcodes, by = "network")
  alldata = alldata[order(alldata$network, decreasing = top.a), ]
  
  avg = alldata %>% filter(num.graphs != 1)
  orig = alldata %>% filter(num.graphs == 1)
   
  pca_avg = prcomp(avg %>% dplyr::select(c(variables)), scale = T, center = T)
   
  orig.coord = as.data.frame(predict(pca_avg, orig %>% dplyr::select(c(variables))))
  rownames(orig.coord) = as.character(orig$network)
  
  if (is.null(plot.nets)) {
    keep.ind = 1:dim(avg)[1]
    colorpalette = df_colorcodes$colorcodes
  } else {
    keep.ind = which(avg$network %in% plot.nets)
    colorpalette = df_colorcodes$colorcodes[which(df_colorcodes$network %in% plot.nets)]
  }
   
  p.pca = fviz_pca_biplot(pca_avg, axes = c(1, 2), repel = repel, pointsize = avg$num.graphs, pointshape = 21, col.var = "contrib", fill.ind = avg$network,
                          label = "var", select.ind = list(name = keep.ind), gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), palette = colorpalette, mean.point = FALSE, title = "")
  p.pca = p.pca + scale_size_continuous(range = c(rlo, rup))
  p.pca = fviz_add(p.pca, orig.coord, shape = 17, labelsize = 0, color = df_colorcodes$colorcodes, pointsize = pointsize)
  # NOTE: for ICRATES :: df_colorcodes$colorcodes[5:50]
  return(p.pca)
}

####Model Error Analysis####
#'
#' Rewire the network 100 times and calculate value of specified function
#' @param graph graph that the null model is based on
#' @param FUN function of the metric of interest
#' @return dataframe with null model values for specified metric
#' 
get_null_model_values = function(graph, FUN = calc.diam) {
  values = c()
  for(i in 1:100) {
    ngraph = rewire(graph, each_edge(p = 1))
    values = c(values, FUN(ngraph))
  }
  return(values)
}

#'
#'Calculates the model errors for multiple different metrics 
#' @param graph graph that null models are based on
#' @param df dataframe of real values for original and time-averaged graphs
#' @return dataframe with model errors
#'
calculate_model_error = function(graph, df) {
  
  if(gsize(graph) != 0) { #check for empty graph
    
    null.btwn = get_null_model_values(graph, FUN = calc.mean.between)
    if(sum(median(null.btwn) > df$btwn) == length(df$btwn)) {
      df$btwn_me = (median(null.btwn) - df$btwn)/(quantile(null.btwn, 0.95) - median(null.btwn))
    } else if (sum(median(null.btwn) < df$btwn) == length(df$btwn)) {
      df$btwn_me = (median(null.btwn) - df$btwn)/(median(null.btwn) - quantile(null.btwn, 0.05))     
    } else {
      df$btwn_me = ifelse(median(null.btwn) > df$btwn,
                          (median(null.btwn) - df$btwn)/(quantile(null.btwn, 0.975) - median(null.btwn)),
                          (median(null.btwn) - df$btwn)/(median(null.btwn) - quantile(null.btwn, 0.025)))
    }                   
    
    null.eigen = get_null_model_values(graph, FUN = calc.mean.eigen)
    if(sum(median(null.eigen) > df$eigen) == length(df$eigen)) {
      df$eigen_me = (median(null.eigen) - df$eigen)/(quantile(null.eigen, 0.95) - median(null.eigen))
    } else if (sum(median(null.eigen) < df$eigen) == length(df$eigen)) {
      df$eigen_me = (median(null.eigen) - df$eigen)/(median(null.eigen) - quantile(null.eigen, 0.05))     
    } else {
      df$eigen_me = ifelse(median(null.eigen) > df$eigen, 
                           (median(null.eigen) - df$eigen)/(quantile(null.eigen, 0.975) - median(null.eigen)), 
                           (median(null.eigen) - df$eigen)/(median(null.eigen) - quantile(null.eigen, 0.025)))
    }                   
    
    # ---->> does not work + is not visualisaed originally either <<----
    # null.pl = get_null_model_values(graph, FUN = calc.mean.path.length) # function seems to work here
    # if(sum(median(null.pl) > df$pl) == length(df$pl)) {
    #   df$pl_me = (median(null.pl) - df$pl)/(quantile(null.pl, 0.95) - median(null.pl))
    # } else if (sum(median(null.pl) < df$pl) == length(df$pl)) {
    #   df$pl_me = (median(null.pl) - df$pl)/(median(null.pl) - quantile(null.pl, 0.05))     
    # } else {
    #   df$pl_me = ifelse(median(null.pl) > df$path.length, 
    #                     (median(null.pl) - df$path.length)/(quantile(null.pl, 0.975) - median(null.pl)), 
    #                     (median(null.pl) - df$path.length)/(median(null.pl) - quantile(null.pl, 0.025)))
    # }                   
    
    null.diam = get_null_model_values(graph, FUN = calc.diam)
    if(sum(median(null.diam) > df$diam) == length(df$diam)) {
      df$diam_me = (median(null.diam) - df$diam)/(quantile(null.diam, 0.95) - median(null.diam))
    } else if (sum(median(null.diam) < df$diam) == length(df$diam)) {
      df$diam_me = (median(null.diam) - df$diam)/(median(null.diam) - quantile(null.diam, 0.05))     
    } else {
      df$diam_me = ifelse(median(null.diam) > df$diam, 
                          (median(null.diam) - df$diam)/(quantile(null.diam, 0.975) - median(null.diam)), 
                          (median(null.diam) - df$diam)/(median(null.diam) - quantile(null.diam, 0.025)))
    }
    
    null.cc = get_null_model_values(graph, FUN = calc.cc)
    if(sum(median(null.cc) > df$cc) == length(df$cc)) {
      df$cc_me = (median(null.cc) - df$cc)/(quantile(null.cc, 0.95) - median(null.cc))
    } else if (sum(median(null.cc) < df$cc) == length(df$cc)) {
      df$cc_me = (median(null.cc) - df$cc)/(median(null.cc) - quantile(null.cc, 0.05))     
    } else {
      df$cc_me = ifelse(median(null.cc) > df$cc, 
                        (median(null.cc) - df$cc)/(quantile(null.cc, 0.975) - median(null.cc)), 
                        (median(null.cc) - df$cc)/(median(null.cc) - quantile(null.cc, 0.025)))
    }  
    
    null.mod = get_null_model_values(graph, FUN = calc.mod)
    if(sum(median(null.mod) > df$mod) == length(df$mod)) {
      df$mod_me = (median(null.mod) - df$mod)/(quantile(null.mod, 0.95) - median(null.mod))
    } else if (sum(median(null.mod) < df$mod) == length(df$mod)) {
      df$mod_me = (median(null.mod) - df$mod)/(median(null.mod) - quantile(null.mod, 0.05))     
    } else {
      df$mod_me = ifelse(median(null.mod) > df$mod, 
                         (median(null.mod) - df$mod)/(quantile(null.mod, 0.975) - median(null.mod)), 
                         (median(null.mod) - df$mod)/(median(null.mod) - quantile(null.mod, 0.025)))
    }  
    
    null.ed = get_null_model_values(graph, FUN = calc.edge.dens)
    if(sum(median(null.ed) > df$ed) == length(df$ed)) {
      df$ed_me = (median(null.ed) - df$ed)/(quantile(null.ed, 0.95) - median(null.ed))
    } else if (sum(median(null.ed) < df$ed) == length(df$ed)) {
      df$ed_me = (median(null.ed) - df$ed)/(median(null.ed) - quantile(null.ed, 0.05))     
    } else {
      df$ed_me = ifelse(median(null.ed) > df$edge.dens,
                        (median(null.ed) - df$edge.dens)/(quantile(null.ed, 0.975) - median(null.ed)),
                        (median(null.ed) - df$edge.dens)/(median(null.ed) - quantile(null.ed, 0.025)))
    }  
    
    # ---->> does not work + is not visualisaed originally either <<----
    # null.deg = get_null_model_values(graph, FUN = calc.deg.dist)
    # if(sum(median(null.deg) > df$deg) == length(df$deg)) {
    #   df$deg_me = (median(null.deg) - df$deg)/(quantile(null.deg, 0.95) - median(null.deg))
    # } else if (sum(median(null.deg) < df$deg) == length(df$deg)) {
    #   df$deg_me = (median(null.deg) - df$deg)/(median(null.deg) - quantile(null.deg, 0.05))     
    # } else {
    #   df$deg_me = ifelse(median(null.deg) > df$mean.deg,
    #                      (median(null.deg) - df$mean.deg)/(quantile(null.deg, 0.975) - median(null.deg)),
    #                      (median(null.deg) - df$mean.deg)/(median(null.deg) - quantile(null.deg, 0.025)))
    # } 
    return(df)
  }
}

#' 
#' Plots model errors for specific variables
#' @param modelerrors dataframe of model errors obtained from calculate_model_error function
#' @param variables list of variables of interest (must specific with "_me" suffix)
#' @return facetted graph looking at change in model error as number of graphs included in time-averaging increases
#' 
plot_model_errors = function(modelerrors, variables, span = 0.4) {
  me = modelerrors %>% gather(key = "modelerror", value = "value", variables)
  metric.labs = c("betweenness centrality", "clustering coefficient", "diameter", 
                  "eigenvector centrality", "modularity", "path length", 
                  "edge density", "degree")
  names(metric.labs) = c("btwn_me", "cc_me", "diam_me", "eigen_me", "mod_me", "pl_me", "ed_me", "deg_me")
  meplot = ggplot(me, aes(x = num.graphs, y = value, group = network, color = network)) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, alpha = 0.05, color = NA, fill = "grey80") +
    geom_hline(yintercept = 0) +
    geom_smooth(se=F, size=0.5, span=span) +
    facet_wrap(~ modelerror, scales = "free_y", labeller = labeller(modelerror = metric.labs)) +
    labs(x = "number of graphs") +
    theme(axis.title.y = element_blank())+
    guides(color = FALSE) +
    theme_minimal()
  return(meplot)
}

#' 
#' Plots percentage of significant model errors for specific variables
#' @param modelerrors dataframe of model errors obtained from calculate_model_error function
#' @param variables list of variables of interest (must specific with "_me" suffix)
#' @param labsize numeric value for the label text size
#' @return facetted graph looking at percentage of metric values different from the null model
#' 
plot_model_errors_bars = function(modelerrors, variables, labsize) {
  me = modelerrors %>% gather(key = "modelerror", value = "value", variables)
  metric.labs = c("betweenness centrality", "clustering coefficient", "diameter", 
                  "eigenvector centrality", "modularity", "path length", 
                  "edge density", "degree")
  names(metric.labs) = c("btwn_me", "cc_me", "diam_me", "eigen_me", "mod_me", "pl_me", "ed_me", "deg_me")
  me$count = NA
  for (val_i in 1:dim(me)[1]) {
    if (is.infinite(me$value[val_i])) {
      me$count[val_i] = "Inf"
    } else if (is.na(me$value[val_i])) {
      me$count[val_i] = "NA"
    } else if (me$value[val_i] <= 1 & me$value[val_i] >= -1) {
      me$count[val_i] = "not different"
    } else if (me$value[val_i] > 1 | me$value[val_i] < -1) {
      me$count[val_i] = "different"
    }
  }
  tab_obs = me %>% group_by(modelerror, num.graphs) %>% summarize(obs = n(), .groups = 'rowwise')
  tab_count = me %>% group_by(modelerror, num.graphs, count) %>% summarize(sum = n(), .groups = 'rowwise')
  tab_all = merge(tab_obs, tab_count, by = c("modelerror", "num.graphs"))
  tab_all$perc = tab_all$sum/tab_all$obs
  tab_all$perc_lab = paste0(round(tab_all$perc*100, 1), "%")
  tab_all$count = as.factor(tab_all$count)
  barplot = ggplot(data = tab_all) +
    aes(x = num.graphs, y = perc, fill = count, label = perc) +
    geom_bar(stat = "identity", position = "fill", width = 0.95) + 
    facet_wrap(~ modelerror, labeller = labeller(modelerror = metric.labs)) +
    labs(x = "number of graphs", y = "") +
    theme(legend.position = c(0.83, 0.1),
          legend.direction = "vertical",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "grey10"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid") )+
    scale_fill_manual(values = c("not different" = "#202020", "different" = "#909090", "Inf" = "#aab7b8", "NA" = "#f8f9f9"))
  if (labsize > 0) {
    barplot = barplot + geom_label(position = position_fill(vjust = 0.5), colour = "#e5e6d8", label.size = 0, size = labsize, aes(x = num.graphs, y = perc, fill = count, label = perc_lab))    
  }  
  return(barplot)
}
