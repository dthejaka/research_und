library(sf)
library(ggplot2)

# defining the shapefile
districts = read_sf(dsn = "D:/UNI STUFF/Stat/4th year/Research/Flickr R/ShapeFiles/Districs_SL.shp") 

ggplot() + 
  geom_sf(data = districts)

## reading the csv file
df = read.csv("D:/UNI STUFF/Stat/4th year/Research/Flickr R/my_full_userwise_scrape.csv")

## converting the df in to an sf object with crs as crs of loaded shapefile
data_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = st_crs(districts))

## transforming the CRS of data_sf to match the district shapefile (just to make sure again)
data_sf <- st_transform(data_sf, crs = st_crs(districts))

## joining data_sf and districts
joined_sf <- st_join(data_sf, districts, join = st_intersects)

## extracting necessary columns
joined_sf_cleaned = joined_sf[,c(2:6, 9, 13, 15)]


## saving the csvs
write.csv(joined_sf_cleaned, "with_provinces_shapefiles.csv")
