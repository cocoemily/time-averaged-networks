library(tidyverse)
library(igraph)

db = readxl::read_xlsx("Data/ROMURBITAL/Database_ROMURBITAL.xlsx")
sites = db %>% filter(Occ_Date_Cert == "High" | Occ_Date_Cert == "Medium")

port.edge = sites[,c(1,16,23:24)] %>% filter(Port_Name != "-")
depend.edge = sites[,c(1,12,23:24)] %>% filter(Dependent_upon != "-")

##need to determine what the age groups should be 

#time averaging functions