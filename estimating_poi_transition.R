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

#final_poi_all_df_adjusted = subset(final_poi_all_df_adjusted, DISTRICT_N == "KANDY")

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