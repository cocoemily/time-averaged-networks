source("scripts/Chaco-time-average.R")
theme_set(theme_minimal())
library(ggthemes)
library(lme4)
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyverse)

c800 = Chaco_ta_compare(graphs[[1]], 1, graphs, "chaco800")
c825 = Chaco_ta_compare(graphs[[2]], 2, graphs, "chaco825")
c850 = Chaco_ta_compare(graphs[[3]], 3, graphs, "chaco850")
c875 = Chaco_ta_compare(graphs[[4]], 4, graphs, "chaco875")
c900 = Chaco_ta_compare(graphs[[5]], 5, graphs, "chaco900")
c925 = Chaco_ta_compare(graphs[[6]], 6, graphs, "chaco925")
c950 = Chaco_ta_compare(graphs[[7]], 7, graphs, "chaco950")
c975 = Chaco_ta_compare(graphs[[8]], 8, graphs, "chaco975")
c1000 = Chaco_ta_compare(graphs[[9]], 9, graphs, "chaco1000")
c1025 = Chaco_ta_compare(graphs[[10]], 10, graphs, "chaco1025")
c1050 = Chaco_ta_compare(graphs[[11]], 11, graphs, "chaco1050")
c1075 = Chaco_ta_compare(graphs[[12]], 12, graphs, "chaco1075")
c1100 = Chaco_ta_compare(graphs[[13]], 13, graphs, "chaco1100")
c1125 = Chaco_ta_compare(graphs[[14]], 14, graphs, "chaco1125")
c1150 = Chaco_ta_compare(graphs[[15]], 15, graphs, "chaco1150")
c1175 = Chaco_ta_compare(graphs[[16]], 16, graphs, "chaco1175")
c1200 = Chaco_ta_compare(graphs[[17]], 17, graphs, "chaco1200")
c1225 = Chaco_ta_compare(graphs[[18]], 18, graphs, "chaco1225")
c1250 = Chaco_ta_compare(graphs[[19]], 19, graphs, "chaco1250")
#c1275 = Chaco_ta_compare(graphs[[20]], 20, graphs, "chaco1275")

alldata = rbind(c800, c825, c850, c875, 
                  c900, c925, c950, c975,
                  c1000, c1025, c1050, c1075,
                  c1100, c1125, c1150, c1175,
                  c1200, c1225, c1250)

datalist = list(c800, c825, c850, c875, 
                c900, c925, c950, c975,
                c1000, c1025, c1050, c1075,
                c1100, c1125, c1150, c1175,
                c1200, c1225, c1250)

####PCA Analysis####

##comparison to original graph
modelerrors = calculate_model_error(graphs[[1]], c800)
for(g in 2:(length(graphs)-1)) {
  modelerrors = rbind(modelerrors, calculate_model_error(graphs[[g]], datalist[[g]]))
}
#plot(plot_model_errors(modelerrors))
ggsave("figures/null-models/Chaco/me_ta-to-orig.pdf", plot_model_errors(modelerrors), height = 4, width = 7)
##need to implement comparison to time-averaged graphs -- look at Prignano script
