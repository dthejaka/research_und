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
    
    FROM = rep(NA, nrow(user_df))
    TO = rep(NA, nrow(user_df))
    user_df = data.frame(ownername = user_df$ownername, FROM, TO)
    user_dfs = rbind(user_dfs, user_df)
  }
}



final_district_all_df_na_drop = drop_na(user_dfs)
final_district_all_df_na_drop <- final_district_all_df_na_drop %>% filter(FROM!= TO)


#clean_kandy_na_drop["combined"] = paste(clean_kandy_na_drop$FROM,clean_kandy_na_drop$TO,sep=",")
#length(unique(clean_kandy_na_drop$combined))
df_count <- final_district_all_df_na_drop %>% group_by(FROM, TO) %>% summarise(count = n_distinct(ownername))
df_sum <- final_district_all_df_na_drop %>% group_by(FROM) %>% summarise(sum = n_distinct(ownername))
df_count = merge(df_count, df_sum, by = "FROM", all.x = TRUE)

df_count["prob"] = df_count$count/df_count$sum
df_count = df_count %>% filter(sum > 5)
df_prob = df_count %>% select(c("FROM", "TO", "prob"))