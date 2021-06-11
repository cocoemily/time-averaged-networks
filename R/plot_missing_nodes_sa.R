plot_missing_nodes_sa <- function(test.BR.rs) {
  
  
  cc.mat.long <- as.data.frame(test.BR.rs[[4]]) %>%
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
    ) %>%
    dplyr::mutate(sample.perc = c(seq(100, 10, by = -10))) %>%
    dplyr::arrange(sample.perc)
  
  mod.mat.long <- as.data.frame(test.BR.rs[[5]]) %>%
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
    ) %>%
    dplyr::mutate(sample.perc = c(seq(100, 10, by = -10))) %>%
    dplyr::arrange(sample.perc)
  
  # Set up for 2 by 3 plotting for the binary network. This will also need to be updated to run on more than one network. Will need to make this a function, then use map to output for each network (or a sample of the networks).
  par(mfrow = c(2, 3))
  boxplot(test.BR.rs[[1]],
          main = "BR - degree",
          xlab = "sampling fraction",
          ylab = "Spearmans rho")
  boxplot(test.BR.rs[[2]], main = "BR - eigenvector", xlab = "sampling fraction")
  boxplot(test.BR.rs[[3]], main = "BR - betweenness", xlab = "sampling fraction")
  plot(
    cc.mat.long$sample.perc,
    cc.mat.long$cc.score,
    type = "l",
    lwd = 5,
    xlab = "Sampling Percentage",
    ylab = "Clustering Coefficient"
  )
  plot(
    mod.mat.long$sample.perc,
    mod.mat.long$mod.score,
    type = "l",
    lwd = 5,
    xlab = "Sampling Percentage",
    ylab = "Modularity Score"
  )
  
}