# Defining the dimensions of the large rectangle that covers whole SL
x_left_bottom <- 5.9
y_left_bottom <- 79.561244
x_right_top <- 9.9
y_right_top <- 81.9

## Calculating the dimensions of each smaller rectangle
rect_width <- (x_right_top - x_left_bottom) / 250
rect_height <- (y_right_top - y_left_bottom) / 400



x_lefts = c()
x_rights = c()
y_bottoms = c()
y_tops = c()


## Loop through each row and column
for (row in 1:400) {
  for (col in 1:250) {
    ## Calculate the coordinates of the current smaller rectangle
    x_left <- x_left_bottom + (col - 1) * rect_width
    x_right <- x_left_bottom + col * rect_width
    y_bottom <- y_left_bottom + (row - 1) * rect_height
    y_top <- y_left_bottom + row * rect_height
    
    ## Store the values
    x_lefts = c(x_lefts, x_left)
    x_rights = c(x_rights, x_right)
    y_bottoms = c(y_bottoms, y_bottom)
    y_tops = c(y_tops, y_top)
  }
}



lens =c()
df = data.frame()
for (i in 1:100000) {
  sub_df = getPhotoSearch(
    api_key = "3186081d7528ee513480e82498d9366b",
    bbox = c(y_bottoms[i], x_lefts[i], y_tops[i], x_rights[i]),
    extras = c("date_taken", "owner_name", "geo", 'url_c'),
    sort = "date-taken-desc",
    per_page = 250,
    page = 1
  )
  
  if(nrow(sub_df) != 0){
    sub_df = select(sub_df, c("owner", "datetaken", "ownername",  "longitude",  "latitude"))
    
    df = rbind(df, sub_df)
  }
  
  print(nrow(sub_df))
  print(i)
  
  while(nrow(sub_df) == 250){
    k = 1
    sub_df = getPhotoSearch(
      api_key = "3186081d7528ee513480e82498d9366b",
      bbox = c(y_bottoms[i], x_lefts[i], y_tops[i], x_rights[i]),
      extras = c("date_taken", "owner_name", "geo", 'url_c'),
      sort = "date-taken-desc",
      per_page = 250,
      page = 1 + k
    )
    
    if(nrow(sub_df) != 0){
      sub_df = select(sub_df, c("owner", "datetaken", "ownername", "longitude",  "latitude"))
      
      df = rbind(df, sub_df)
    }
    
    k = k + 1
    print("##########")
    print(k)
  }
  print(nrow(sub_df))
  print(i)
}