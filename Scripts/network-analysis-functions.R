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
  null.btwn = get_null_model_values(graph, FUN = calc.mean.between)
  df$btwn_me = ifelse(median(null.btwn) > df$btwn, 
                      (median(null.btwn) - df$btwn)/(quantile(null.btwn, 0.975) - median(null.btwn)), 
                      (median(null.btwn) - df$btwn)/(median(null.btwn) - quantile(null.btwn, 0.025))) 
  
  null.eigen = get_null_model_values(graph, FUN = calc.mean.eigen)
  df$eigen_me = ifelse(median(null.eigen) > df$eigen, 
                       (median(null.eigen) - df$eigen)/(quantile(null.eigen, 0.975) - median(null.eigen)), 
                       (median(null.eigen) - df$eigen)/(median(null.eigen) - quantile(null.eigen, 0.025)))
  
  null.pl = get_null_model_values(graph, FUN = calc.mean.path.length)
  df$pl_me = ifelse(median(null.pl) > df$path.length, 
                    (median(null.pl) - df$path.length)/(quantile(null.pl, 0.975) - median(null.pl)), 
                    (median(null.pl) - df$path.length)/(median(null.pl) - quantile(null.pl, 0.025)))
  
  null.diam = get_null_model_values(graph, FUN = calc.diam)
  df$diam_me = ifelse(median(null.diam) > df$diam, 
                      (median(null.diam) - df$diam)/(quantile(null.diam, 0.975) - median(null.diam)), 
                      (median(null.diam) - df$diam)/(median(null.diam) - quantile(null.diam, 0.025)))
  
  null.cc = get_null_model_values(graph, FUN = calc.cc)
  df$cc_me = ifelse(median(null.cc) > df$cc, 
                    (median(null.cc) - df$cc)/(quantile(null.cc, 0.975) - median(null.cc)), 
                    (median(null.cc) - df$cc)/(median(null.cc) - quantile(null.cc, 0.025)))
  
  null.mod = get_null_model_values(graph, FUN = calc.mod)
  df$mod_me = ifelse(median(null.mod) > df$mod, 
                     (median(null.mod) - df$mod)/(quantile(null.mod, 0.975) - median(null.mod)), 
                     (median(null.mod) - df$mod)/(median(null.mod) - quantile(null.mod, 0.025)))
  
  null.ed = get_null_model_values(graph, FUN = calc.edge.dens)
  df$ed_me = ifelse(median(null.ed) > df$edge.dens,
                     (median(null.ed) - df$edge.dens)/(quantile(null.ed, 0.975) - median(null.ed)),
                     (median(null.ed) - df$edge.dens)/(median(null.ed) - quantile(null.ed, 0.025)))

  null.deg = get_null_model_values(graph, FUN = calc.mean.deg)
  df$deg_me = ifelse(median(null.deg) > df$mean.deg,
                    (median(null.deg) - df$mean.deg)/(quantile(null.deg, 0.975) - median(null.deg)),
                    (median(null.deg) - df$mean.deg)/(median(null.deg) - quantile(null.deg, 0.025)))
  
  
  return(df)
}

#' 
#' Plots model errors for specific variables
#' @param modelerrors dataframe of model errors obtained from calculate_model_error function
#' @param variables list of variables of interest (must specific with "_me" suffix)
#' @return facetted graph looking at change in model error as number of graphs included in time-averaging increases
#' 
plot_model_errors = function(modelerrors, variables) {
  me = modelerrors %>% gather(key = "modelerror", value = "value", variables)
  metric.labs = c("betweenness centrality", "clustering coefficient", "diameter", 
                  "eigenvector centrality", "modularity", "path length", 
                  "edge density", "degree")
  names(metric.labs) = c("btwn_me", "cc_me", "diam_me", "eigen_me", "mod_me", "pl_me", "ed_me", "deg_me")
  meplot = ggplot(me, aes(x = num.graphs, y = value, group = network, color = network)) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, alpha = 0.05, color = NA, fill = "grey80") +
    geom_hline(yintercept = 0) +
    geom_smooth(se=F, size=0.5, span=0.4) +
    facet_wrap(~ modelerror, scales = "free_y", labeller = labeller(modelerror = metric.labs)) +
    labs(x = "number of graphs") +
    theme(axis.title.y = element_blank())+
    guides(color = FALSE) +
    theme_minimal()
  return(meplot)
}
