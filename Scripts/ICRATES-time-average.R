# LOADING SETTINGS

### set correct working directory
setwd("C:/Users/daems/OneDrive/Work/Code/GitHub/time-averaged-networks")

# Preparation
rm(list=ls())
library(dplyr)
library(statnet) #includes the libraries network, sna, and ergm
library(tnet) #includes the library igraph
library(vegan)
library(FastKNN)
library(kableExtra)
library(ggraph)
library(GGally)
library(igraph)
library(tidyverse)
library(censReg)
library(bbmle)
library(MASS)
library(pscl)
library(stargazer)
library(ggthemes)


### read in data
data_catalogue = read.csv("Data/ICRATES/ICRATES_CATALOGUE.csv", header = T, sep = ",")
data_form = read.csv("Data/ICRATES/ICRATES_standard_form.csv", header = T, sep = ",")
loc = read.csv("Data/ICRATES/ICRATES_location.csv")
#deposit = read.csv("Data/ICRATES/ICRATES_deposit.csv")
#lrp = read.csv("Data/ICRATES/ICRATES_lrp.csv")
#ock = read.csv("Data/ICRATES/ICRATES_ock.csv")

#_______________________________________________________________________________________________________________________________________

#DATA PREP

# check data
colnames(data_catalogue)
str(data_catalogue)
unique(data_catalogue$Fabric)
length(unique(data_catalogue$Standard_Form_ID)) # key
length(unique(data_form$Standard_Form_ID)) # use upper and lower date

# check missing date
sum(is.na(data_form$Standard_Typo_chronological_Lower_Date))/dim(data_form)[1] # 20% of standard forms has no date info

# Merge loc and data_catalogue to add coordinates
locations <- merge(data_catalogue, loc, by = 'Location_ID', all=TRUE, sort=FALSE)

## Merge data
merged_data = merge(locations, data_form, by = c("Standard_Form_ID", "Fabric"), all = T) # some mismatches in Fabric of data_form and data_catalogue - investigate further?

# retain only the ones with date info
merged_data_dates = subset(merged_data, !(is.na(merged_data$Standard_Typo_chronological_Lower_Date) | is.na(merged_data$Standard_Typo_chronological_Upper_Date)))

vars = c("Location_ID", "Location_specific", "Standard_Form_ID", "Fabric_ID", "Fabric", "Standard_Typo_chronological_Lower_Date", "Standard_Typo_chronological_Upper_Date", "Longitude", "Latitude")
merged_data_dates = merged_data_dates[,c(colnames(merged_data_dates) %in% vars)]
head(merged_data_dates)
merged_data_dates$Fabric = as.character(merged_data_dates$Fabric) # no factors
str(merged_data_dates)

data_table = as.data.frame(table(merged_data_dates$Standard_Form_ID), stingsAsFactors = F)
colnames(data_table)[1] = "Standard_Form_ID"

final_table = unique(merge(merged_data_dates, data_table, by = "Standard_Form_ID", all = T))
write.csv(final_table, "Data/ICRATES/final_table.csv", row.names = FALSE)

#_______________________________________________________________________________________________________________________________________

#NODE AND EDGE LISTS FOR GEPHI

# Create node lists
## Select relevant columns
nodes <- final_table[,c(3,4,6,7)] # select(final_table, "Location_ID", "Location_specific", "Longitude", "Latitude")
## Remove duplicate locations
node_list <- nodes %>% distinct(Location_ID, .keep_all = TRUE)
## Remove places without coordinates
node_list <- node_list[!(node_list$Longitude == 0.00000 & node_list$Latitude == 0.00000),]
## Remove NA
node_list <- na.omit(node_list)
## Save .csv file
write.csv(node_list, "Data/ICRATES/node_list.csv", row.names = FALSE)

# Create incidence lists
incidence_list = final_table[, which(colnames(final_table) %in% c("Location_ID", 'Standard_Form_ID', 'Freq'))]
## Remove places without coordinates
incidence_list <- incidence_list[!(node_list$Longitude == 0.00000 & node_list$Latitude == 0.00000),]
incidence_list = as.data.frame.matrix(xtabs(Freq ~ Location_ID + Standard_Form_ID, incidence_list))

## Save .csv file
write.csv(incidence_list, "Data/ICRATES/incidence_list.csv", row.names = FALSE)


# Turn incidence matrix into edge list
df_incidence = incidence_list
# rownames(df_incidence)
# colnames(df_incidence)
df_incidence = ifelse(df_incidence > 0, TRUE, FALSE)
df_links = data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_links) = c("source", "target", "weight")
for (i in 1:(dim(df_incidence)[1]-1)) {
  df1 = df_incidence[i,]
  for (j in (i+1):dim(df_incidence)[1]) {  
    df2 = df_incidence[j,]
    df_i = cbind.data.frame(rownames(df_incidence)[i], rownames(df_incidence)[j], sum(df1 & df2))
    colnames(df_i) = c("source", "target", "weight")
    df_links = rbind.data.frame(df_links, df_i)
  }
}
df_links = df_links[df_links$weight > 0,]
df_links$source = as.character(df_links$source)
df_links$target = as.character(df_links$target)
str(df_links)
head(df_links)
summary(df_links)

## Save .csv file
write.csv(df_links, "Data/ICRATES/edge_list.csv", row.names = FALSE)


#_________________________________________________________________________________________________________________________________________________

# OPTIONAL: Sub-networks per pottery ware

## Create datasets per main ware
count(final_table, wt_var = Fabric) # count instances per ware

ESA <- subset(final_table, Fabric == 'ESA')
ESB <- subset(final_table, grepl("ESB *", final_table$Fabric)) ## function grepl() --> Groups all ESB subsets
ESC <- subset(final_table, grepl("ESC *", final_table$Fabric))
ESD <- subset(final_table, Fabric == 'ESD')
ARSW <- subset(final_table, grepl("ARSW*", final_table$Fabric))
ITS <- subset(final_table, grepl("ITS*", final_table$Fabric))
CRSW  <- subset(final_table, Fabric == 'CRSW')
PRSW <- subset(final_table, Fabric == 'PRSW')
# SRSW <- subset(final_table, Fabric == 'SRSW') ## not to be included in analysis, just personal interest

## create node lists by pottery type
# get_all_nodes = function(fabric) {
#   sites = final_table %>% filter(Fabric_ID == fabric) %>%
#     group_by(Location_ID) %>%
#     summarize(site = first(Location_specific), 
#               sf.id = first(Standard_Form_ID))
#   return(sites)
# }


# fabrics <- c("ITS", "ITS Arezzo","ITS Arezzo?", "ITS Pisa", "ITS Pisa?", "ITS Puteoli", "ITS Central Italy?", "ITS Puteoli?", 
#             "ITS Padana", "ITS Central Italy",  "ITS Campania?", "ITS Arezzo-Pisa-Lyon","ITS Po Valley", "ITS Pisa-Lyon",
#             "ITS Etruria?", "ITS Arezzo-Pisa", "ESA", "ESBI", "ESBII", "ESC/Candarli", "ESD", "Candarli", "CRSW", "ARSW",
#             "ARSW-D", "PRSW", "ARSW-A", "ARSW-C", "ARSW-C/E", "ESC", "SRSW","Campana A", "Cypriot", "Pontic Sigillata")


#_____________________________________________________________________________________________________________________________________

# TIME SLICING

step = 20 # years for each slice
slices = (max(final_table$Standard_Typo_chronological_Upper_Date) - min(final_table$Standard_Typo_chronological_Lower_Date))/step # number of slices
# years = seq(min(final_table$Standard_Typo_chronological_Lower_Date), max(final_table$Standard_Typo_chronological_Upper_Date)+1, 1)
brk = seq(min(final_table$Standard_Typo_chronological_Lower_Date), max(final_table$Standard_Typo_chronological_Upper_Date)+1, step); brk

# cut to ranges
Ranges = cut(final_table$Standard_Typo_chronological_Lower_Date, breaks = brk, include.lowest = TRUE, right = TRUE) # Set the cutpoints
# Ranges = cut(years, breaks = brk, include.lowest = TRUE, right = TRUE) # Set the cutpoints
head(Ranges)
levels(Ranges)

# i = 1
# time_span = lapply(final_table, function(x){x$Standard_Typo_chronological_Upper_Date - x$Standard_Typo_chronological_Lower_Date})
# time_span = final_table$Standard_Typo_chronological_Upper_Date - final_table$Standard_Typo_chronological_Lower_Date
# summary(time_span)
# final_table$Standard_Typo_chronological_Lower_Date[1]; final_table$Standard_Typo_chronological_Upper_Date[1]; Ranges[1]; time_span[1]

data_intervals = data.frame("nrow" = 1:length(levels(Ranges)), "Year_Interval" = levels(Ranges))
for (i in 1:dim(final_table)[1]) {
  ts = seq(final_table$Standard_Typo_chronological_Lower_Date[i], final_table$Standard_Typo_chronological_Upper_Date[i], 1)
  freqts = as.data.frame(table(cut(ts, breaks = brk, include.lowest = TRUE, right = TRUE)), stringsAsFactors = F)
  colnames(freqts) = c("Year_Interval", final_table$Standard_Form_ID[i])
  data_intervals = merge(data_intervals, freqts, by = "Year_Interval", all = T)
}

data_intervals = data_intervals[order(data_intervals$nrow, decreasing = FALSE),]
data_intervals$Year_Interval[which(!(data_intervals$Year_Interval %in% Ranges) == TRUE)]
colSums(data_intervals[,-c(1,2)])
data_intervals_estimated = t(data_intervals)

for(i in 3:dim(data_intervals)[2]) {
  data_intervals_estimated[i,] = final_table$Freq[i - 2] * as.numeric(data_intervals_estimated[i,])/sum(as.numeric(data_intervals_estimated[i,]))  
}

data_intervals_estimated_2 = cbind(rownames(data_intervals_estimated), data_intervals_estimated)
data_intervals_estimated_2 = data_intervals_estimated_2[-2,]
colnames(data_intervals_estimated_2) = data_intervals_estimated_2[1,]
colnames(data_intervals_estimated_2)[1] = "Standard_Form_ID"
data_intervals_estimated_2 = data_intervals_estimated_2[-1,]

data_timeslices = merge(final_table, data_intervals_estimated_2, by = "Standard_Form_ID", all = T)
data_timeslices = data_timeslices[,c(2,3,1,4:dim(data_timeslices)[2])]
data_timeslices = data_timeslices[order(data_timeslices$Fabric, decreasing = FALSE),]


# plot all
dataplot_timeslices = data_timeslices[,-c(1:6)]
plot(1:dim(dataplot_timeslices)[2], dataplot_timeslices[1,], type = "l", ylim = c(0,300), ylab = "frequency", xlab = "slice")
for (i in 2:dim(dataplot_timeslices)[1]){
  lines(1:dim(dataplot_timeslices)[2], dataplot_timeslices[i,], type = "l", ylim = c(0,300))  
}

# write data
write.csv(data_timeslices, "data_slices_default.csv", row.names = FALSE)


#_______________________________________________________________________________________________________________________________________

#OTHER CODE

##ALTERNATIVE NODE LIST: turn Location_specific into rownames and drop 2 columns to leave only coordinates --> needed for weighted networks
node_list2 <- node_list
rownames(node_list2) <- node_list2[,2]
node_list2 = subset(node_list2, select = -c(Location_ID, Location_specific))


##Create edge list for Vistorian
vist_columns = c("Location_specific", 'Location_specific', "TO", "TARGET_LOCATION", "Freq", "Fabric")
vist_edge_list = final_table[, which(colnames(final_table) %in% c("Location_ID", 'Standard_Form_ID', "Fabric", 'Freq'))]
##attempt
library(netdiffuseR)
vist_edge_list2 <- adjmat_to_edgelist(
  as.matrix(edge_list),
  undirected = getOption("diffnet.undirected", TRUE),
  keep.isolates = getOption("diffnet.keep.isolates", TRUE)
)


# FUNCTIONS < Matt Peeples tutorial
## co-presence of types based on threshold of 25% (a category must represent 25% of a row to be considered present)
co.p <- function (x,thresh=0.25) {
  #create matrix of proportions from ceramics
  temp <- prop.table(as.matrix(x),1) 
  # define anything with greater than or equal to 0.1 as present (1) 
  temp[temp>=thresh] <- 1 
  # define all other cells as absent (0)
  temp[temp<1] <- 0 
  # matrix algebraic calculation to find co-occurrence (%*% indicates matrix multiplication)
  out <- temp%*%t(temp) 
  return(out)}
### run the function
ceramicP <- co.p(edge_list)
### display the results
kable(ceramicP) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "300px")

##Brainerd-Robinson Similarity
ceramic.p <- prop.table(as.matrix(edge_list), margin = 1) # This line converts the ceramic cluster frequency table to a table of proportions by row
### The following line uses the vegdist function to calculate the Brainard-Robinson similarity score. 
ceramicBR <- (2-as.matrix(vegdist(ceramic.p, method='manhattan')))/2
### display the results
kable(ceramicBR) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "300px")

##x^2 distance
chi.dist <- function(x) {
  rowprof <- x/apply(x,1,sum) # calculates the profile for every row
  avgprof <- apply(x,2,sum)/sum(x) # calculates the average profile
  # creates a distance object of $\chi^{2}$ distances
  chid <- dist(as.matrix(rowprof)%*%diag(1/sqrt(avgprof))) 
  # return the results
  return(as.matrix(chid))} 
### Run the script and then create the rescaled 0-1 version
ceramicX <- chi.dist(edge_list)
ceramicX01 <- ceramicX/max(ceramicX)
### display the results
kable(ceramicX01) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "300px")

##k-nearest neighbors
### Create a distance matrix using Euclidean distances based on the site coordinates in node_list
distMatrix <- as.matrix(dist(node_list))
### set k as 5 and create a function that calculates the 5 nearest neighbors for each node
k <- 5
nrst <- lapply(1:nrow(distMatrix), function(i) k.nearest.neighbors(i, distMatrix, k = k))
### the chunk of code below creates a symmetric matrix of 0s and then fills cells with 1 where two sites are k nearest neighbors
dist.knn <- matrix(nrow = dim(distMatrix), ncol=dim(distMatrix),0) 
for(i in 1:length(nrst)) for(j in nrst[[i]]) dist.knn[i,j] = 1
### set row and column names
row.names(dist.knn) <- row.names(node_list)
colnames(dist.knn) <- row.names(node_list)
### display the results
kable(dist.knn) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "300px")


#Visualize network
## create network object from co-occurrence
Pnet <- network(ceramicP,directed=F)
### Now let's add names for our nodes based on the row names of our original matrix
Pnet %v% 'vertex.names' <- row.names(ceramicP)
### look at the results
Pnet
### plot network
plot(Pnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5,main='co-presence network')
# plot network using geographic coordinates
plot(Pnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5, coord=node_list2)  # does not work

## Define our binary network object from BR similarity
BRnet <- network(event2dichot(ceramicBR,method='absolute',thresh=0.65),directed=F)
### Now let's add names for our nodes based on the row names of our original matrix
BRnet %v% 'vertex.names' <- row.names(ceramicBR)
### look at the results.
BRnet
### plot network
plot(BRnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5,main='BR network')
# plot network using geographic coordinates
plot(BRnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5, coord=node_list2) # does not work

###Define our binary network object from chi squared; Note we use 1 minus ceramicX01 here so to convert a distance to a similarity
Xnet <- network(event2dichot(1-ceramicX01,method='quantile',thresh=0.80),directed=F)
#### Once again add vertex names
Xnet %v% 'vertex.names' <- row.names(ceramicX01)
#### look at the results
Xnet
#### plot network using default layout
plot(Xnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5,main='Chi-squared network')
# plot network using geographic coordinates
plot(Xnet, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5, coord=node_list2) # does not work

###Define our binary network object from k nearest neighbors
#### Create network object with directed ties
dist.net <- network(dist.knn,directed=T)
#### Once again add vertex names
dist.net %v% 'vertex.names' <- row.names(node_list)
#### look at the results
dist.net
#### plot network using default layout
plot(dist.net, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5,main='K nearest neighbors network, k=5')
# plot network using geographic coordinates
plot(dist.net, edge.col='gray', edge.lwd=0.25, vertex.cex=0.5, coord=node_list2)


## Weighted networks


#_________________________________________________________________________________________________________________________________________________

