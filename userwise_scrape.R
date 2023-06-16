library(FlickrAPI)
library(dplyr)
library(readxl)

setFlickrAPIKey(api_key = "3186081d7528ee513480e82498d9366b", overwrite = TRUE)


i = 1
k = 1
df = data.frame()

user_df1 = read.csv("final_full.csv")
IDs = user_df1$owner

IDs = unique(IDs)
length(IDs) 

not_scraped = c()

for (userID in IDs){
  i = 1
  while (TRUE) {
    sub_df = getPhotoSearch(
      api_key = "3186081d7528ee513480e82498d9366b",
      user_id = userID,
      #tags = c('Benthota'),
      bbox = c(79.561244, 5.9, 81.9, 9.9),
      extras = c("date_taken", "owner_name", "geo", 'url_c'),
      sort = "date-taken-desc",
      per_page = 100,
      page = i
    )
    #i = i+1
    
    if(length(sub_df != 0)){
      if("url_c" %in% colnames(sub_df)){
        sub_df = sub_df %>% select('id', 'owner', 'datetaken', 'ownername', 'latitude',
                                   'longitude', 'url_c')
        df = rbind(df, sub_df)
        i = i + 1
      }else{
        sub_df = sub_df %>% select('id', 'owner', 'datetaken', 'ownername', 'latitude',
                                   'longitude')
        url_c = rep(NA, nrow(sub_df))
        sub_df['url_c'] = url_c 
        df = rbind(df, sub_df)
        i = i + 1
      }
    }else{
      if(i == 1){ ## checking whether the user has been scraped or not
        x1 = sprintf("%s user not scraped",k)
        print(x1)
        not_scraped = append(not_scraped, userID)
      }
      break
    }
  }
  x2 = sprintf("%s users done",k)
  print(x2)
  k = k + 1
}


write.csv(df, "my_full_userwise_scrape.csv")

write.csv(as.data.frame(not_scraped), "not_scraped_users.csv")