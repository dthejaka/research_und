usernames = unique(df$ownername)
user_type = c()
df$date = as.Date(df$date)
for (user in usernames) {
  df_user = df %>% filter(ownername == user)
  first_date = min(df_user$date)
  #df_user_30 = df_user %>% filter(date < first_date +31)
  last_date = max(df_user$date)
  if(last_date-first_date <= 31 ){
    user_type = c(user_type, "Inbound")
  }else{
    user_type = c(user_type, "Local")
  }
}