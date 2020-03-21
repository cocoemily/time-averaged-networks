### set correct working directory
setwd("C:/Users/Dries Daems/Documents/Work/Datasets/ICRATES")

### read in data
catalogue <- read.csv("icrates_catalogue.txt")
#deposit = read.csv("icrates_deposit.txt")
#lrp = read.csv("icrates_lrp.txt")
#ock = read.csv("icrates_ock.txt")
sf = read.csv("icrates_standard_form.txt")
loc = read.csv("icrates_location.txt")

### Check dataset
str(catalogue)
unique(catalogue$Fabric)

### Create datasets per main ware
ESA <- subset(catalogue, Fabric == 'ESA')
ESB <- subset(catalogue, grepl("ESB *", catalogue$Fabric)) ## function grepl() --> Groups all ESB subsets
ESC <- subset(catalogue, grepl("ESC *", catalogue$Fabric))
ESD <- subset(catalogue, Fabric == 'ESD')
ARSW <- subset(catalogue, grepl("ARSW*", catalogue$Fabric))
ITS <- subset(catalogue, grepl("ITS*", catalogue$Fabric))
# SRSW <- subset(catalogue, Fabric == 'SRSW') ## not to be included in analysis, just personal interest


#need to decide how to build the network and the time-slices
#network based on shared pottery types

#create node lists by pottery type
get_all_nodes = function(fabric) {
  sites = catalogue %>% filter(Fabric_ID == fabric) %>%
    group_by(Location_ID) %>%
    summarize(site = first(Location_specific), 
              sf.id = first(Standard_Form_ID))
  return(sites)
}
