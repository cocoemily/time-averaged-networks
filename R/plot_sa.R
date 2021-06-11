plot_sa <- function(sa.data) {
  # Prepare data for plotting.
  dg.mat <- as.data.frame(sa.data[[1]]) %>%
    tidyr::gather(s.fraction, values)
  ev.mat <- as.data.frame(sa.data[[2]]) %>%
    tidyr::gather(s.fraction, values)
  bw.mat <- as.data.frame(sa.data[[3]]) %>%
    tidyr::gather(s.fraction, values)
  
  cc.mat.long <- as.data.frame(sa.data[[4]]) %>%
    tidyr::gather(
      "S100",
      "S90",
      "S80",
      "S70",
      "S60",
      "S50",
      "S40",
      "S30",
      "S20",
      "S10",
      key = sample.perc,
      value = cc.score
    )
  
  mod.mat.long <- as.data.frame(sa.data[[5]]) %>%
    tidyr::gather(
      "S100",
      "S90",
      "S80",
      "S70",
      "S60",
      "S50",
      "S40",
      "S30",
      "S20",
      "S10",
      key = sample.perc,
      value = mod.score
    )
  
  
  # Save individual plots, then can create an arranged plot, so that each network has 1 set of figures.
  dg.mat.plot <- ggpubr::ggboxplot(
    dg.mat,
    title = "Degree Centrality",
    x = "s.fraction",
    y = "values",
    xlab = "Sampling Fraction",
    ylab = "Spearman's Rho",
    bxp.errorbar = TRUE
  ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ev.mat.plot <- ggpubr::ggboxplot(
    ev.mat,
    title = "Eigenvector Centrality",
    x = "s.fraction",
    y = "values",
    xlab = "Sampling Fraction",
    ylab = "Spearman's Rho",
    bxp.errorbar = TRUE
  ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  bw.mat.plot <- ggpubr::ggboxplot(
    bw.mat,
    title = "Betweenness Centrality",
    x = "s.fraction",
    y = "values",
    xlab = "Sampling Fraction",
    ylab = "Spearman's Rho",
    bxp.errorbar = TRUE
  ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  cc.mat.long.plot <- ggpubr::ggline(
    cc.mat.long,
    title = "Clustering Coefficient",
    x = "sample.perc",
    y = "cc.score",
    xlab = "Sampling Fraction",
    ylab = "Clustering Coefficient Value"
  ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  mod.mat.long.plot <- ggpubr::ggline(
    mod.mat.long,
    title = "Modularity",
    x = "sample.perc",
    y = "mod.score",
    xlab = "Sampling Fraction",
    ylab = "Modularity Value"
  ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
    plot <- ggpubr::ggarrange(
      dg.mat.plot,
      ev.mat.plot,
      bw.mat.plot,
      cc.mat.long.plot,
      mod.mat.long.plot,
      labels = c("A", "B", "C", "D", "E"),
      ncol = 3,
      nrow = 2
    )
    
    return(plot)
  
}