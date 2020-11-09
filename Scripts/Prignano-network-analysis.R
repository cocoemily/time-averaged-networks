#Prignano dataset network-level analysis
source("scripts/Prignano-time-average.R")
source("scripts/network-analysis-functions.R")
theme_set(theme_minimal())
library(ggthemes)
library(lme4)
library(dplyr)
library(factoextra)

####Analysis####
#EIA1E : Early Iron Age 1 Early (950/925 900 BC)
#EIA1L : Early Iron Age 1 Late (900 850/825 BC)
#EIA2 : Early Iron Age 2 (850/825 730/720 BC)
#OA : Orientalizing Age (730/720 580 BC)
#AA : Archaic Period (580-500 BC)
ta5 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups = groups)

ta2_1 = ta_compare(eia1e.edge, eia1l.edge, groups = groups)
ta2_2 = ta_compare(eia1l.edge, eia2.edge, groups = groups)
ta2_3 = ta_compare(eia2.edge, oa.edge, groups = groups)
ta2_4 = ta_compare(oa.edge, aa.edge, groups = groups)

ta3_1 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, groups = groups)
ta3_2 = ta_compare(eia1l.edge, eia2.edge, oa.edge, groups = groups)
ta3_3 = ta_compare(eia2.edge, oa.edge, aa.edge, groups = groups)

ta4_1 = ta_compare(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups = groups)
ta4_2 = ta_compare(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups = groups)

eia1eta = rbind(ta2_1 %>% filter(names != "eia1l"), 
                ta3_1 %>% filter(names == "ta3"),
                ta4_1 %>% filter(names == "ta4"),
                ta5 %>% filter(names == "ta5"))
eia1eta$num.graphs = c(1,2,3,4,5)
eia1eta$network = "EIA1E"
eia1lta = rbind(ta2_1 %>% filter(names != "eia1e"), ta2_2 %>% filter(names == "ta2"), 
                ta3_1 %>% filter(names == "ta3"), ta3_2 %>% filter(names == "ta3"), 
                ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
                ta5 %>% filter(names == "ta5"))
eia1lta$num.graphs = c(1,2,2,3,3,4,4,5)
eia1lta$network = "EIA1L"
eia2ta = rbind(ta2_2 %>% filter(names != "eia1l"), ta2_3 %>% filter(names == "ta2"), 
               ta3_1 %>% filter(names == "ta3"), ta3_2 %>% filter(names == "ta3"), ta3_3 %>% filter(names == "ta3"), 
               ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
               ta5 %>% filter(names == "ta5"))
eia2ta$num.graphs = c(1,2,2,3,3,3,4,4,5)
eia2ta$network = "EIA2"
oata = rbind(ta2_3 %>% filter(names != "eia2"), ta2_4 %>% filter(names == "ta2"), 
             ta3_2 %>% filter(names == "ta3"), ta3_3 %>% filter(names == "ta3"), 
             ta4_1 %>% filter(names == "ta4"), ta4_2 %>% filter(names == "ta4"), 
             ta5 %>% filter(names == "ta5"))
oata$num.graphs = c(1,2,2,3,3,4,4,5)
oata$network = "OA"
aata = rbind(ta2_4 %>% filter(names != "oa"), 
             ta3_3 %>% filter(names == "ta3"), 
             ta4_2 %>% filter(names == "ta4"), 
             ta5 %>% filter(names == "ta5"))
aata$num.graphs = c(1,2,3,4,5)
aata$network = "AA"

alldata = rbind(eia1eta, eia1lta, eia2ta, oata, aata)
alldata$network = factor(alldata$network, levels = c("EIA1E", "EIA1L", 
                                                     "EIA2", "OA", "AA"))

####PCA Analysis####
ggsave("figures/pca/Prignano/pca-biplot.pdf", pca_biplot(alldata, c("btwn", "cc", "eigen", "mod", "mean.deg", "path.length")))

####Model Errors####
modelerrors = rbind(
  calculate_model_error(graph_from_edgelist(as.matrix(eia1e.edge[,1:2])), eia1eta), 
  calculate_model_error(graph_from_edgelist(as.matrix(eia1l.edge[,1:2])), eia1lta), 
  calculate_model_error(graph_from_edgelist(as.matrix(eia2.edge[,1:2])), eia2ta), 
  calculate_model_error(graph_from_edgelist(as.matrix(oa.edge[,1:2])), oata), 
  calculate_model_error(graph_from_edgelist(as.matrix(aa.edge[,1:2])), aata)
)
ggsave("figures/null-models/Prignano/me_ta-to-orig.pdf", plot_model_errors(modelerrors, c("btwn_me", "eigen_me", "deg_me", "cc_me", "mod_me", "pl_me")), height = 4, width = 7)

me_eia1eta = rbind(
  calculate_model_error(average_two(eia1e.edge, eia1l.edge, groups), (eia1eta %>% filter(names == "ta2"))), 
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), (eia1eta %>% filter(names == "ta3"))), 
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), (eia1eta %>% filter(names == "ta4"))), 
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), (eia1eta %>% filter(names == "ta5")))
)
#ggsave("figures/null-models/Prignano/me_eia1e.pdf", plot_model_errors(me_eia1eta, c("btwn_me", "eigen_me", "cc_me", "mod_me")))

me_eia1lta = rbind(
  calculate_model_error(average_two(eia1e.edge, eia1l.edge, groups), eia1lta[2,]),
  calculate_model_error(average_two(eia1l.edge, eia2.edge, groups), eia1lta[3,]),
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), eia1lta[4,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), eia1lta[5,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), eia1lta[6,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia1lta[7,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia1lta[8,])
)
#ggsave("figures/null-models/Prignano/me_eia1l.pdf", plot_model_errors(me_eia1lta, c("btwn_me", "eigen_me", "cc_me", "mod_me")))

me_eia2ta = rbind(
  calculate_model_error(average_two(eia1l.edge, eia2.edge, groups), eia2ta[2,]),
  calculate_model_error(average_two(eia2.edge, oa.edge, groups), eia2ta[3,]),
  calculate_model_error(average_three(eia1e.edge, eia1l.edge, eia2.edge, groups), eia2ta[4,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), eia2ta[5,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), eia2ta[6,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), eia2ta[7,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia2ta[8,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), eia2ta[9,])
)
#ggsave("figures/null-models/Prignano/me_eia2.pdf", plot_model_errors(me_eia2ta, c("btwn_me", "eigen_me", "cc_me", "mod_me")))

me_oata = rbind(
  calculate_model_error(average_two(eia2.edge, oa.edge, groups), oata[2,]),
  calculate_model_error(average_two(oa.edge, aa.edge, groups), oata[3,]),
  calculate_model_error(average_three(eia1l.edge, eia2.edge, oa.edge, groups), oata[4,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), oata[5,]),
  calculate_model_error(average_four(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, groups), oata[6,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), oata[7,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), oata[8,])
)
#ggsave("figures/null-models/Prignano/me_oa.pdf", plot_model_errors(me_oata, c("btwn_me", "eigen_me", "cc_me", "mod_me")))

me_aata = rbind(
  calculate_model_error(average_two(oa.edge, aa.edge, groups), aata[2,]),
  calculate_model_error(average_three(eia2.edge, oa.edge, aa.edge, groups), aata[3,]),
  calculate_model_error(average_four(eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), aata[4,]),
  calculate_model_error(average_five(eia1e.edge, eia1l.edge, eia2.edge, oa.edge, aa.edge, groups), aata[5,])
)
#ggsave("figures/null-models/Prignano/me_aa.pdf", plot_model_errors(me_aata, c("btwn_me", "eigen_me", "cc_me", "mod_me")))

all_me_ta = rbind(me_eia1eta, me_eia1lta, me_eia2ta, me_oata, me_aata)
ggsave("figures/null-models/Prignano/all_ta_me.pdf", plot_model_errors(all_me_ta, c("btwn_me", "eigen_me", "deg_me", "cc_me", "mod_me", "pl_me")), height = 4, width = 7)
