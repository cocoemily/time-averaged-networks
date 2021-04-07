# ICRATES time slices per fabric and location
rm(list=ls())

setwd("C:/Users/daems/OneDrive/Work/Code/GitHub/time-averaged-networks")
data_catalogue = read.csv("Data/ICRATES/icrates_catalogue.csv", header = T, sep = ",")
data_form = read.csv("Data/ICRATES/icrates_standard_form.csv", header = T, sep = ",")

# check data
colnames(data_catalogue)
length(unique(data_catalogue$Standard_Form_ID)) # key
length(unique(data_form$Standard_Form_ID)) # use upper and lower date

# check missing date
sum(is.na(data_form$Standard_Typo_chronological_Lower_Date))/dim(data_form)[1] # 20% of standard forms has no date info

# merge data
merged_data = merge(data_catalogue, data_form, by = c("Standard_Form_ID", "Fabric"), all = T) # some mismatches in Fabric of data_form and data_catalogue - investigate further? 
# retain only the ones with date info
merged_data_dates = subset(merged_data, !(is.na(merged_data$Standard_Typo_chronological_Lower_Date) | is.na(merged_data$Standard_Typo_chronological_Upper_Date)))

# keeps variables with information for the slicing
vars = c("Standard_Form_ID", "Fabric_ID", "Fabric", "Standard_Typo_chronological_Lower_Date", "Standard_Typo_chronological_Upper_Date") 
merged_data_dates = merged_data_dates[,c(colnames(merged_data_dates) %in% vars)]
merged_data_dates$Fabric = as.character(merged_data_dates$Fabric) # no factors
str(merged_data_dates)

# frequency data per Standard Form
data_table = as.data.frame(table(merged_data_dates$Standard_Form_ID), stingsAsFactors = F)
colnames(data_table)[1] = "Standard_Form_ID"
final_table = unique(merge(merged_data_dates, data_table, by = "Standard_Form_ID", all = T))

# remove data from environment, keep only "merged_data" and "final_table"
rm(data_catalogue, data_form, merged_data_dates, data_table)

# years for each slice
step = 20
# number of slices
nsl = (max(final_table$Standard_Typo_chronological_Upper_Date) - min(final_table$Standard_Typo_chronological_Lower_Date))/step
# breaks between slices
brk = seq(min(final_table$Standard_Typo_chronological_Lower_Date), max(final_table$Standard_Typo_chronological_Upper_Date)+1, step); brk
# cut to ranges
Ranges = cut(final_table$Standard_Typo_chronological_Lower_Date, breaks = brk, include.lowest = TRUE, right = TRUE) # Set the cutpoints
head(Ranges)
levels(Ranges)

# distribute chronological range of each form to the created slices
data_intervals = data.frame("nrow" = 1:length(levels(Ranges)), "Year_Interval" = levels(Ranges))
for (i in 1:dim(final_table)[1]) {
  ts = seq(final_table$Standard_Typo_chronological_Lower_Date[i], final_table$Standard_Typo_chronological_Upper_Date[i], 1)
  freqts = as.data.frame(table(cut(ts, breaks = brk, include.lowest = TRUE, right = TRUE)), stringsAsFactors = F)
  colnames(freqts) = c("Year_Interval", final_table$Standard_Form_ID[i])
  data_intervals = merge(data_intervals, freqts, by = "Year_Interval", all = T)
}
# order data
data_intervals = data_intervals[order(data_intervals$nrow, decreasing = FALSE),]
# transpose data
data_intervals_estimated = t(data_intervals)
# bring in relevant format before merging
data_intervals_estimated = cbind(rownames(data_intervals_estimated), data_intervals_estimated)
data_intervals_estimated = data_intervals_estimated[-2,]
colnames(data_intervals_estimated) = data_intervals_estimated[1,]
colnames(data_intervals_estimated)[1] = "Standard_Form_ID"
data_intervals_estimated = data_intervals_estimated[-1,]

# data table to include the location and fabric
merged_data_dates = subset(merged_data, !(is.na(merged_data$Standard_Typo_chronological_Lower_Date) | is.na(merged_data$Standard_Typo_chronological_Upper_Date)))
vars = c("Location_ID", "Location_specific", "Standard_Form_ID", "Fabric_ID", "Fabric", "Standard_Typo_chronological_Lower_Date", "Standard_Typo_chronological_Upper_Date", "Longitude", "Latitude")
merged_data_dates = merged_data_dates[,c(colnames(merged_data_dates) %in% vars)]
merged_data_dates$Fabric = as.character(merged_data_dates$Fabric) # no factors
str(merged_data_dates)
# frequency data, per form and location
data_table = as.data.frame(table(merged_data_dates$Standard_Form_ID, merged_data_dates$Location_ID), stingsAsFactors = F)
colnames(data_table)[1:2] = c("Standard_Form_ID", "Location_ID")
# include the fabric
data_fabric = unique(cbind.data.frame(merged_data_dates$Standard_Form_ID, merged_data_dates$Fabric_ID))
colnames(data_fabric) = c("Standard_Form_ID", "Fabric_ID")
data_table = merge(data_table, data_fabric, by = "Standard_Form_ID")

# dates data
vars = c("Standard_Form_ID", "Standard_Typo_chronological_Lower_Date", "Standard_Typo_chronological_Upper_Date")
data_dates = unique(merged_data_dates[,c(colnames(merged_data_dates) %in% vars)])
data_dates$Lifetime = data_dates$Standard_Typo_chronological_Upper_Date - data_dates$Standard_Typo_chronological_Lower_Date + 1

# merge frequency data and date data
final_table = merge(data_table, data_dates, by = "Standard_Form_ID")
final_table$Standard_Form_ID = as.character(final_table$Standard_Form_ID)
final_table$Location_ID = as.character(final_table$Location_ID)

# merge data created above with estimated time slices
df_form_loc = merge(final_table, data_intervals_estimated, by = "Standard_Form_ID")
df_form_loc[,dim(final_table)[2]:(dim(final_table)[2]+nsl)] = unlist(lapply(df_form_loc[,dim(final_table)[2]:(dim(final_table)[2]+nsl)], as.numeric))

# split the actual frequencies per location and form 
df_form_loc[,(dim(final_table)[2]+1):(dim(final_table)[2]+nsl)] = df_form_loc$Freq*df_form_loc[,(dim(final_table)[2]+1):(dim(final_table)[2]+nsl)]/df_form_loc$Lifetime

# remove data from environment, keep only "df_form_loc"
rm(freqts, merged_data, data_intervals, data_intervals_estimated, merged_data_dates, data_table, data_fabric, data_dates, final_table)

# aggregate form frequencies per fabric for each location
df_fabric_loc = df_form_loc
vars = c("Standard_Form_ID", "Standard_Typo_chronological_Lower_Date", "Standard_Typo_chronological_Upper_Date", "Lifetime" )
df_fabric_loc = df_fabric_loc[, -which(colnames(df_fabric_loc) %in% vars)]
df_agg_fabric_loc = aggregate(. ~ Fabric_ID + Location_ID, data = df_fabric_loc, FUN = sum)
dim(df_agg_fabric_loc)[1] == sum(df_agg_fabric_loc$Freq == round(rowSums(df_agg_fabric_loc[,4:53]))) # check result

# remove data from environment, keep only "df_agg_fabric_loc"
rm(df_form_loc, df_fabric_loc)

# make one df per slice and save
setwd("C:/Users/daems/OneDrive/Work/Code/GitHub/time-averaged-networks/Data/ICRATES/time-slices")
for (i in 4:dim(df_agg_fabric_loc)[2]) {
  df_slice_i = as.data.frame.matrix(xtabs(df_agg_fabric_loc[,i] ~ Location_ID + Fabric_ID, data = df_agg_fabric_loc))
  filename = paste0("ICRATES_slice_", i-3, "_range_" ,gsub(",", "to", substr(colnames(df_agg_fabric_loc)[i], 2, nchar(colnames(df_agg_fabric_loc)[i])-1)), ".csv")
  write.csv(df_slice_i, filename, row.names = TRUE)
  # rownames are Location_IDs colnames are Fabric_IDs.
}




