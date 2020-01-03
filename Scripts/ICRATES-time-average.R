catalogue = read.csv("Data/ICRATES/ICRATES_CATALOGUE.csv")
#deposit = read.csv("Data/ICRATES/ICRATES_DEPOSIT.csv")
#lrp = read.csv("Data/ICRATES/ICRATES_LRP.csv")
#ock = read.csv("Data/ICRATES/ICRATES_OCK.csv")
sf = read.csv("Data/ICRATES/ICRATES_STANDARD_FORM.csv")
#loc = read.csv("Data/ICRATES/ICRATES_LOCATION.csv")

#need to decide how to build the network and the time-slices
#network based on shared pottery types

#create node lists by pottery type
get_all_nodes = function(fabric) {
  sites = catalogue %>% filter(Fabric_ID == fabric) %>%
    group_by(Location_ID) %>%
    summarize(site = first(Location_specific), 
              fabric.id = first(Fabric_ID))
  return(sites)
}



