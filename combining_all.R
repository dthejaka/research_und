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

length(unique(final_poi_all_df$ownername))

############### FILTER FOR INBOUND TOURISTS ###############
user_cat_df = read.csv("D:/UNI STUFF/Stat/4th year/Research/Flickr R/just_takin_last_first.csv")

final_poi_all_df = merge(final_poi_all_df, user_cat_df, by = "ownername")

length(unique(final_poi_all_df$POI))

########### MAKING THE WHOLE COUNTRY FLOW ###########

final_poi_all_df$date = as.Date(final_poi_all_df$date)
## removing records with same owner, same date at same POI
final_poi_all_df  <- final_poi_all_df[!duplicated(final_poi_all_df [, c("owner", "date", "POI")]), ]

## sorting by user, date and time
final_poi_all_df  <- final_poi_all_df[order(final_poi_all_df$ownername, final_poi_all_df$date, final_poi_all_df$time), ]

length(unique(final_poi_all_df$POI))

## 208

## filtering for inbound tourists
final_poi_all_df = subset(final_poi_all_df, user_type == "Inbound")
length(unique(final_poi_all_df$ownername))
## 975 to 963 inbound tourists after removing the repeated POIs




###### district to district flow ######

final_poi_all_df_adjusted = data.frame()

##### Adding Airport for once who do not have airport as the first POI #############
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



user_dfs = data.frame()
for (i in users) {
  ## extracting user df
  user_df = subset(final_poi_all_df_adjusted , final_poi_all_df_adjusted$ownername == i)
  
  
  ## 1st argument - more than one district visit
  if(length(unique(user_df$DISTRICT_N)) > 1){
    
    district_vec = user_df$DISTRICT_N
    user_df["FROM"] = c(NA, district_vec[-length(district_vec)])
    user_df["TO"] = district_vec
    user_df = user_df[c("ownername", "FROM", "TO")]
    user_dfs = rbind(user_dfs, user_df)
  }else{
    abc = c(abc,i)
    FROM = rep("GAMPAHA", nrow(user_df))
    TO = rep(NA, nrow(user_df))
    user_df = data.frame(ownername = user_df$ownername, FROM, TO)
    user_dfs = rbind(user_dfs, user_df)
  }
}

final_district_all_df_na_drop = drop_na(user_dfs)
final_district_all_df_na_drop <- final_district_all_df_na_drop %>% filter(FROM!= TO)

length(unique(final_district_all_df_na_drop$ownername))
#clean_kandy_na_drop["combined"] = paste(clean_kandy_na_drop$FROM,clean_kandy_na_drop$TO,sep=",")
#length(unique(clean_kandy_na_drop$combined))


df_count <- final_district_all_df_na_drop %>% group_by(FROM, TO) %>% summarise(count = n_distinct(ownername))
df_sum <- final_district_all_df_na_drop %>% group_by(FROM) %>% summarise(sum = n_distinct(ownername))
df_count = merge(df_count, df_sum, by = "FROM", all.x = TRUE)

df_count["prob"] = df_count$count/df_count$sum
df_count = df_count %>% filter(sum > 5)
df_prob = df_count %>% select(c("FROM", "TO", "prob"))
#df_prob = df_count



## the reason why some FROMs have one is because the rest is classfied s local tourists
write.csv(df_count, "District_to_district_flow.csv")
## TAKING ATLEAST 5 USERS DISTRICTS



##Rounding off to 4 decimal places
df_count$prob = round(df_count$prob, digits = 4)



write.csv(df_count, "1district_todistrict.csv")


### district to didstrict matrix
matrix <- xtabs(prob ~ FROM + TO, df_count)
# Convert the matrix to a data frame
df <- as.data.frame(matrix)

# Convert the "from" and "to" columns to character or factor
df$FROM <- as.character(df$FROM)
df$TO <- as.character(df$TO)

# Convert the data frame to long format
df_long <- pivot_longer(df, cols = -c(FROM, TO), names_to = "count", values_to = "prob")
df_long <- df_long[order(df_long$TO), ]
df_long = df_long[order(df_long$FROM), ]

# Plot the heatmap using ggplot2
library(ggplot2)

ggplot(df_long, aes(x = TO, y = FROM, fill = prob)) +
  geom_tile() +
  scale_fill_gradient(high = "#003477", low = "#9AC0F2") +
  geom_text(aes(label = round(prob, 4)),color = "black", size = 3) +
  labs(x = "TO", y = "FROM") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 0))



###### Finding POI to POI whole country ######
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

## finding number of people visited from one attraction to another

users = unique(final_poi_all_df_adjusted$ownername)


user_dfs = data.frame()
for (i in users) {
  ## extracting user df
  user_df = subset(final_poi_all_df_adjusted, final_poi_all_df_adjusted$ownername == i)
  
  
  ## 1st argument - more than one place visit
  if(length(unique(user_df$POI)) > 1){
    
    poi_vec = user_df$POI
    user_df["FROM"] = c("Bandaranayaka International Airport", poi_vec[-length(poi_vec)])
    user_df["TO"] = poi_vec
    user_df = user_df[c("ownername", "FROM", "TO", "DISTRICT_N", "latitude_poi", "longitude_poi")]
    user_dfs = rbind(user_dfs, user_df)
  }else{
    FROM = rep("Bandaranayaka International Airport", nrow(user_df))
    TO = rep(NA, nrow(user_df))
    user_df = data.frame(ownername = user_df$ownername, FROM, TO)
    user_dfs = rbind(user_dfs, user_df)
  }
}



final_poi_all_df_adjusted["FROM"] = user_dfs["FROM"]
final_poi_all_df_adjusted["TO"] = user_dfs["TO"]



final_poi_all_df_na_drop = drop_na(final_poi_all_df_adjusted)
final_poi_all_df_na_drop = final_poi_all_df_na_drop %>% filter(FROM != TO)


df_count <- final_poi_all_df_na_drop %>% group_by(FROM, TO) %>% summarise(count = n_distinct(ownername))
df_sum <- df_count %>% group_by(FROM) %>% summarise(sum = sum(count))
df_count = merge(df_count, df_sum, by = "FROM")


df_count["prob"] = df_count$count/df_count$sum

df_count = df_count %>% filter(sum>10)

df_count$prob = round(df_count$prob, digits = 4)


user_dfs_matale = user_dfs %>% filter(DISTRICT_N == "MATALE")


################ Adding Airport for once who do n`ot have airport as the first POI #############
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

final_poi_all_df_adjusted = subset(final_poi_all_df_adjusted, DISTRICT_N == "KANDY")

## finding number of people visited from one attraction to another


users = unique(final_poi_all_df_adjusted$ownername)




user_dfs = data.frame()
for (i in users) {
  ## extracting user df
  user_df = subset(final_poi_all_df_adjusted, final_poi_all_df_adjusted$ownername == i)
  
  
  ## 1st argument - more than one place visit
  if(length(unique(user_df$POI)) > 1){
    
    poi_vec = user_df$POI
    user_df["FROM"] = c(NA, poi_vec[-length(poi_vec)])
    user_df["TO"] = poi_vec
    user_df = user_df[c("ownername", "FROM", "TO")]
    user_dfs = rbind(user_dfs, user_df)
  }else{
    FROM = rep(NA, nrow(user_df))
    TO = rep(NA, nrow(user_df))
    user_df = data.frame(ownername = user_df$ownername, FROM, TO)
    user_dfs = rbind(user_dfs, user_df)
  }
}



final_poi_all_df_adjusted["FROM"] = user_dfs["FROM"]
final_poi_all_df_adjusted["TO"] = user_dfs["TO"]

final_poi_all_df_na_drop = drop_na(final_poi_all_df_adjusted)
final_poi_all_df_na_drop = final_poi_all_df_na_drop %>% filter(FROM != TO)


df_count <- final_poi_all_df_na_drop %>% group_by(FROM, TO) %>% summarise(count = n_distinct(ownername))
df_sum <- df_count %>% group_by(FROM) %>% summarise(sum = sum(count))
df_count = merge(df_count, df_sum, by = "FROM")


df_count["prob"] = df_count$count/df_count$sum

