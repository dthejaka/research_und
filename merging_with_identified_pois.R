library(stringr)
library(tidyverse)
library(readxl)
############ MAKING THE FINAL DATASET ###########
final_poi_all_df = data.frame()

big_df_file_list <- list.files(path = "D:/UNI STUFF/Stat/4th year/Research/Flickr R/Clusteing Results_R/All_big_dfs", pattern = "*.csv")

for (csv in big_df_file_list) {
  ## reading one district csv
  big_df_district = read.csv(sprintf("D:/UNI STUFF/Stat/4th year/Research/Flickr R/Clusteing Results_R/All_big_dfs/%s",csv))
  
  ## reading the corresponding POI xls
  districts_name = str_to_title(tolower(unique(big_df_district$DISTRICT_N)))
  df_poi = read_excel(sprintf("D:/UNI STUFF/Stat/4th year/Research/Flickr R/Clusteing Results_R/All_POI/POI_%s.xlsx", districts_name))
  
  ## chanigng the column names in order to merge properly
  columns_poi = c("Clusters_0.0005_5", colnames(df_poi)[-1])
  colnames(df_poi) = columns_poi
  
  ## merging with POI df
  df_merged = merge(big_df_district, df_poi, by = "Clusters_0.0005_5")
  
  cleaned_merged = subset(df_merged, df_merged$`Can Take` == 1)
  
  
  
  cleaned_district_df = subset(cleaned_merged, select = c("id", "owner", "ownername", "date", "time", "DISTRICT_N", "POI",
                                                          "latitude_poi", "longitude_poi"))
  
  
  final_poi_all_df = rbind(final_poi_all_df, cleaned_district_df)
  #break
}


############### FILTER FOR INBOUND TOURISTS ###############
user_cat_df = read.csv("D:/UNI STUFF/Stat/4th year/Research/Flickr R/just_takin_last_first.csv")

final_poi_all_df = merge(final_poi_all_df, user_cat_df, by = "ownername")


########### MAKING THE WHOLE COUNTRY FLOW ###########

final_poi_all_df$date = as.Date(final_poi_all_df$date)
## removing records with same owner, same date at same POI
final_poi_all_df  <- final_poi_all_df[!duplicated(final_poi_all_df [, c("owner", "date", "POI")]), ]

## sorting by user, date and time
final_poi_all_df  <- final_poi_all_df[order(final_poi_all_df$ownername, final_poi_all_df$date, final_poi_all_df$time), ]


## filtering for inbound tourists
final_poi_all_df = subset(final_poi_all_df, user_type == "Inbound")
length(unique(final_poi_all_df$ownername))
## 947 inbound tourists




################ Adding Airport for once who do not have airport as the first POI #############
## for each user if the minimum date is not "Bandaranayaka International Airport" add it for the minimum date

final_poi_all_df_adjusted = data.frame()

for (user in unique(final_poi_all_df$ownername)) {
  user_df = subset(final_poi_all_df, ownername == user)
  
  if(user_df[1,]$POI != "Bandaranayaka International Airport"){
    add_df = user_df[1,]
    add_df['DISTRICT_N'] = "GAMPAHA"
    add_df['POI'] = "Bandaranayaka International Airport"
    add_df['date'] = add_df['date'] - 1
    add_df['latitude_poi'] = 7.1753
    add_df['longitude_poi'] = 79.8835
    
    user_df = rbind(add_df, user_df)
  }
  
  final_poi_all_df_adjusted = rbind(final_poi_all_df_adjusted, user_df)
}
