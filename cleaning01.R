## making the column names correctly
column_names = colnames(df)
column_names = c(column_names[c(-1, -10)], c("longitude", "latitude"))
colnames(df) = column_names

## regex
df$longitude <- sapply(df$longitude, function(x) as.numeric(sub("c\\((.*)", "\\1", x)))
df$latitude <- sapply(df$latitude, function(x) as.numeric(sub("\\)", "", as.character(x))))


## split the datetime column into date and time
## split the datetime column into date and time
df$date <- sapply(strsplit(df$datetaken, " "), function(x) x[1])
df$time <- sapply(strsplit(df$datetaken, " "), function(x) x[2])

## removing photos from same date, same owner, same lon and lat
df$latitude = signif(df$latitude, digits = 5)
df$longitude = signif(df$longitude, digits = 6)

df <- df[!duplicated(subset(df, select = c("date","owner", "latitude", "longitude"))), ]