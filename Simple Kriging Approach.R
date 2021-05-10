rm(list=ls())
require(rgdal)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(gstat)  
library(automap) 
library(patchwork)

#####
results.total <- read.csv("/home/stathis/Desktop/Scripts/results.rnn.gstat.10.may.1500.csv")
results.2hrs <- read.csv("/home/stathis/Desktop/Scripts/results.2hrs.rnn.gstat.10may1502.csv")
results.6hrs <- read.csv("/home/stathis/Desktop/Scripts/results.6hrs.rnn.gstat.10may1502.csv")
results.24hrs <- read.csv("/home/stathis/Desktop/Scripts/results.24hrs.rnn.gstat.10may1502.csv")

results <- results.2hrs
results <- results[ , c("x.y", "y.y", "value")]
results2 <- SpatialPointsDataFrame(coords = results[ , c("x.y", "y.y")], data = results)
t <- st_make_grid(results2, cellsize = 0.02, what = "polygons")
t <- as_Spatial(t)
t.data <- as.data.frame(matrix(nrow=NROW(t), ncol=1))
names(t.data) <- c("id")
t.data$id <- 1:NROW(t)
row.names(t.data) <-row.names(t)
t.sp <- SpatialPolygonsDataFrame(t, data = t.data)
t.sp@data$mean <- NA
t.sp@data$mean <- over(t.sp, results2, fn = mean)[ c("value")]

data.total <- cbind(coordinates(t.sp), t.sp$mean)
data.total <- subset(data.total, !is.na(data.total$value))
write.csv(data.total, "/home/stathis/Desktop/Scripts/gstat/gridded.2hrs.csv")
#data import

results.total <- read.csv("/home/stathis/Desktop/Scripts/gstat/gridded.total.csv")
results.2hrs <- read.csv("/home/stathis/Desktop/Scripts/gstat/gridded.2hrs.csv")
results.6hrs <- read.csv("/home/stathis/Desktop/Scripts/gstat/gridded.6hrs.csv")
results.24hrs <- read.csv("/home/stathis/Desktop/Scripts/gstat/gridded.24hrs.csv")

  
#2hrs

results <- results.2hrs
names(results)

results <- results[ ,c("X1", 
                       "X2", 
                       "value")]
names(results) <- c("X", "Y", "value")
results$value <- as.integer(results$value)
results <- SpatialPointsDataFrame(coords = results[ ,c("X", "Y")], data = results)
results@data$value <- as.numeric(results@data$value)

variogram <- automap::autofitVariogram(results@data$value~1, results, model="Sph")$var_model
plot(automap::autofitVariogram(results@data$value~1, results, model="Sph"), title = "Total")


#prediction grid common for all subsets
grd_100_sf <- results %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_make_grid(
    cellsize = c(0.02, 0.02), # 100m pixel size
    what = "centers"
  ) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.))


grd_100_sp <- as(grd_100_sf, "Spatial")
gridded(grd_100_sp) <- TRUE    
grd_100_sp <- as(grd_100_sp, "SpatialPixels") # 


#simple kriging total
krige.gstat <- krige(
  results@data$value~1,                     
  beta = mean(results$value),
  results, 
  grd_100_sp,        
  model = variogram
)

raster.total <- rasterFromXYZ(krige.gstat)
shape <- readOGR("/home/stathis/Desktop/SeismicIntensityArticle-master/shapefile/area-of-interest.shp", layer="area-of-interest")
raster.total <- mask(raster.total, shape)

plot(raster.total)
writeRaster(raster.total, "/home/stathis/Desktop/Scripts/gstat/raster.2hrs.10.may.1516.gstat.tif", overwrite=TRUE)
