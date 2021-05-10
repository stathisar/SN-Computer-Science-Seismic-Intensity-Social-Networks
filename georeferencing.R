rm(list = ls())
#required libraries
require("splitstackshape")
require("lubridate")
require("spdep")
require("splancs")
require("rgdal")
require("plyr")
require("rgeos")
require("rgdal")
require("raster")
require("geoR")
setwd("/home/stathis/Desktop/SeismicIntensityArticle-master/")

tweets.intensity.rnn <- read.csv("/home/stathis/Desktop/SeismicIntensityArticle-master/data.final.rnn.27040049.csv")

#import of classified dataset
tweets.intensity.rnn$vamb = NA
for(i in 1:NROW(tweets.intensity.rnn)){
  if(as.numeric(tweets.intensity.rnn$p[i]) > as.numeric(tweets.intensity.rnn$n[i])){
    tweets.intensity.rnn$vamp[i] = c('n')
  }else{
    tweets.intensity.rnn$vamp[i] = c('p')
  }
  
}
#removing ambiguity from classified data
tweets.intensity.final <- subset(tweets.intensity.rnn, 
                                 tweets.intensity.rnn$vamp == 'p')


#import of geolocations
geolocations <- read.csv("./geolocations.l.csv")
geolocations$name <- as.character(tolower(geolocations$name))
geolocations$Lat <- as.numeric(geolocations$Lat)
geolocations$Long <- as.numeric(geolocations$Long)
tweets.intensity.final$reference <- 0
tweets.intensity.final$x <- 0
tweets.intensity.final$y <- 0
tweets.intensity.final$precision <- 0
tweets.intensity.final$comment <- NA

#creation of dataframe in which the georeferencing output will be stored
tweets.georeferenced <- data.frame(matrix(nrow = 0, 
                                          ncol = NCOL(tweets.intensity.final)))

#loop that adds lat lon information and replicates tweets according to geolocation word detection
for (i in 1:NROW(geolocations)){
  tweets.test <- tweets.intensity.final[ grepl(geolocations$name[i], 
                                               tweets.intensity.final$Text), ]
  if(NROW(tweets.test) == 0){
  }else{
    tweets.test$x <- geolocations$Long[i]
    tweets.test$y <- geolocations$Lat[i]
    tweets.test$detected <- geolocations$name[i]
    tweets.test$precision <- geolocations$Precision[i]
    tweets.test$comment <- geolocations$duplicates[i]
    tweets.georeferenced <- rbind(tweets.georeferenced, tweets.test)
  }
}


#export geo-referenced prerandomized dataset
write.csv(tweets.georeferenced, 
          "./georeferenced.prerandomized.apr.17.2021.csv")

#start of randomization part
final.results.prerandomized <- tweets.georeferenced
#removal of non needed dataframes

rm(tweets.test, tweets.georeferenced)

#keeping only data with coordinates
final.results.prerandomized <- subset(final.results.prerandomized, y > 0)

#import of area.map shapefile
area.map <- readOGR(dsn = "./shapefile", 
                    layer = "area-of-interest")
#creation of unique id to area.map
area.map@data$id <- as.numeric(row.names(area.map@data)) + 1


#estimation of centroid of each area
area.map@data$centroid.x <- NA
area.map@data$centroid.y <- NA
for (i in 1:NROW(area.map)){
  area.map@data$centroid.x[i] <- paste(as.numeric(gCentroid(area.map[i, ])$x))
  area.map@data$centroid.y[i] <- paste(as.numeric(gCentroid(area.map[i, ])$y))
}

#storage to centroids dataframe
centroids <- as.data.frame(area.map@data[ , c("NAME", "id", "centroid.x", "centroid.y")])
centroids$centroid.x <- as.numeric(centroids$centroid.x)
centroids[ ,4] <- as.numeric(centroids[ ,4])

#calculating distance from earthquake's epicenter
area.map@data$distance.from.epic <- spDistsN1(as.matrix(centroids[ ,3:4]), 
                                              c(as.numeric("26.36"), as.numeric("38.84")))

#creation of spatial points dataframe
final.results.prerandomized$x <- gsub(",", ".", final.results.prerandomized$x)
final.results.prerandomized$y <- gsub(",", ".", final.results.prerandomized$y)
final.results.prerandomized$x = as.numeric(final.results.prerandomized$x)
final.results.prerandomized$y = as.numeric(final.results.prerandomized$y)

spdf.prerandomize <- SpatialPointsDataFrame(coords = final.results.prerandomized[ ,c("x", "y")], 
                                            data = final.results.prerandomized, 
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#spatial join of municipality name on spdfprerandomize
spdf.prerandomize@data$NAME <- NA
spdf.prerandomize@data$id <- NA
spdf.prerandomize@data$distance.from.epic <- NA
spdf.prerandomize@data[, c("NAME", "id", "distance.from.epic")]

proj4string(area.map) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf.prerandomize) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

spdf.prerandomize@data[, c("NAME", "id", "distance.from.epic")] = sp::over(spdf.prerandomize, 
                                                                           area.map)[, c("NAME", "id", "distance.from.epic")]

point.count <- summary(spdf.prerandomize@data$NAME)
point.count <- as.data.frame(point.count)
point.count$NAME <- row.names(point.count)

map.count <- merge(area.map, point.count, by.area.map = NAME, 
                   by.point.count = NAME, 
                   all.area.map = TRUE)
#aligning ids
map.count@data$id <- as.numeric(map.count@data$id)
map.count@data$id <- as.numeric(map.count@data$id + 1)
rownames(map.count@data) <- map.count@data$id
map.count@data$point.count[is.na(map.count@data$point.count)] <- 0 
#create randompoints and spdf in which randompoints will be stored

generated.random.points <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]
map.count@data$point.count  <- as.numeric(map.count@data$point.count)

for (i in 1:nrow(map.count)) {
  if (map.count@data$point.count[i] == 0){
    i = i+1}
  else {
    generated.random.points <- append(generated.random.points, 
                                      spsample(map.count[i, ], 
                                               n=map.count@data$point.count[i], 
                                               "random"))
    
  }
}

#put all random coords in a dataframe

random.points <- data.frame()
random.points$x <- as.numeric()
random.points$y <- as.numeric()

#add Counter by column
#table$Counter <- with(table, ave(seq_along(NAME), NAME, FUN = seq_along))

for (i in 1:length(generated.random.points)){
  random.points <- rbind(random.points, generated.random.points[[i]]@coords)
  i <- i+1
}
write.csv(random.points, file = "./randompointswithoutvalues.17.apr.2021.csv")

#remove data that are not used any more
rm(generated.random.points, point.count, spdf.prerandomize)
#creating a uniqueid of finalresultsprerandomize
final.results.prerandomized$munname <- NA
final.results.prerandomized$counter <- 0
final.results.prerandomized$uniqueid <- NA
spdf.prerandomized <- SpatialPointsDataFrame(coords = final.results.prerandomized[ , c("x", "y")], 
                                             data = final.results.prerandomized, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

final.results.prerandomized$munname <- over(spdf.prerandomized, area.map[, "NAME"])
final.results.prerandomized$municipality <- paste(as.character(unlist(final.results.prerandomized$munname)))
final.results.prerandomized$counter <-  with(final.results.prerandomized, 
                                             ave(seq_along(municipality), 
                                                 municipality, 
                                                 FUN=seq_along))
final.results.prerandomized$uniqueid <- paste(final.results.prerandomized$municipality, 
                                              final.results.prerandomized$counter, 
                                              sep = "")
rm(spdf.prerandomized)

#creating unique id of randompoints
random.points$munname <- NA
random.points$counter <- 0
random.points$uniqueid <- NA
random.points$distance.from.epic <- NA
spdf.random.points <- SpatialPointsDataFrame(coords = random.points[ ,c("x", "y")], 
                                             data = random.points, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
random.points$munname <- over(spdf.random.points, area.map[ , "NAME"])
random.points$distance.from.epic <- over(spdf.random.points, area.map[ , "distance.from.epic"])
random.points$municipality <- paste(as.character(unlist(random.points$munname)))
random.points$counter <- with(random.points, 
                              ave(seq_along(municipality), 
                                  municipality, 
                                  FUN = seq_along))
random.points$uniqueid <- paste(random.points$municipality, random.points$counter, sep = "")
rm(spdf.random.points)
#merge finalresultsprerandomized and randompoints
final.results.prerandomized.2 <- final.results.prerandomized
final.results.prerandomized <- merge(final.results.prerandomized, 
                                     random.points, 
                                     by = "uniqueid", 
                                     all.final.results.prerandomized = TRUE)


#unlist results
for (i in 1:length(final.results.prerandomized)){
  final.results.prerandomized[ , i] <- paste(unlist(final.results.prerandomized[ ,i]))
  i <- i + 1
}


tweets.randomized.final <- subset(final.results.prerandomized, 
                                  final.results.prerandomized$precision == 0 | 
                                    final.results.prerandomized$precision == 3 |
                                    final.results.prerandomized$precision == 2 | 
                                    final.results.prerandomized$precision == 4)


write.csv(tweets.randomized.final, file = "./tweets.randomized.final.april.17.2021.csv")

#VALIDATION RULE 1
#eliminate values that are > than X and have distance from epicenter greater than Y
#add the epicenter coordinates
epicenter <- data.frame(1:2)
epicenter$y <- 38.84
epicenter$x <- 26.36
#setting primary zone
epicenter$ymax[1] <- epicenter$x[1] + 0.3
epicenter$ymin[1] <- epicenter$x[1] - 0.3
epicenter$xmax[1] <- epicenter$y[1] + 0.3
epicenter$xmin[1] <- epicenter$y[1] - 0.3

epicenter$earthquake <- "lesvos"

#setting secondary zone
epicenter$xmax[2] <- epicenter$x[2] + 0.65
epicenter$xmin[2] <- epicenter$x[2] - 0.65
epicenter$ymax[2] <- epicenter$y[2] + 0.65
epicenter$ymin[2] <- epicenter$y[2] - 0.65

results <- tweets.randomized.final

#selecting wrong values outside primary zone
wrong.values <- subset(results, ((results$x.y > epicenter$xmax[1] | 
                                    results$x.y < epicenter$xmin[1] | 
                                    results$y.y > epicenter$ymax[1] | 
                                    results$y.y < epicenter$ymin[1])) & (results$value > 6))
#selecting wrong values outside secondary zone
wrong.values.2 <- subset(results, (results$x.y > epicenter$xmax[2] | 
                                     results$x.y < epicenter$xmin[2] | 
                                     results$y.y > epicenter$ymax[2] | 
                                     results$y.y < epicenter$ymin[2]) & (results$value > 4))

#removing wrong values
results <- tweets.randomized.final
results <- results[!(results$uniqueid) %in% wrong.values$uniqueid, ]
results <- results[!(results$uniqueid) %in% wrong.values.2$uniqueid, ]


write.csv(results, file = "./validation.1.geo.filtered.apr.17.2021.csv")


#remove data that are not used any more
rm(map.count, random.points, final.results.prerandomized, final.results.prerandomized.2, geolocations,
   tweets.randomized.final, tweets.intensity.final, wrong.values, wrong.values.2)

#Validation rule 2:
names(results)
results <- results[ ,c("uniqueid", "id", "X.M..object_posted_time..", "Text", "value", "x.x", "y.x", 
                       "x.y", "y.y", "distance.from.epic", "municipality.y",
                       "precision")]

names(results) <-c("uniqueid", "id", "posted.time", "Text", "value", "x.x", "y.x", 
                   "x.y", "y.y", "distance.from.epic", "municipality",
                   "precision")


#list of municipalities that contain macroseismic obs
#unique(results$municipality)
results$value <- as.numeric(results$value)

#definition of 3 zones accordinf to distance from epicenter
min.value <- as.numeric(range(results$distance.from.epic)[1])
max.value <- as.numeric(range(results$distance.from.epic)[2])
list <- c(min.value, max.value)
med.value <- median(list)

#quarters(list)
results.zone1 <- subset(results, results$distance.from.epic <= 0.5)
results.zone2 <- subset(results, results$distance.from.epic > 0.5)

#remove wrong values from zone1
for (i in 1:as.numeric(range(results.zone1$value)[2]) - as.numeric(range(results.zone1$value)[1])){
  a.value <- as.numeric(range(results.zone1$value)[2]) - as.numeric(range(results.zone1$value)[1])
  if (a.value > 4){
    results.zone1 <- subset(results.zone1, results.zone1$value != min(results.zone1$value))
  }else{
  }
}

#remove wrong values from zone2
unique(results.zone2$municipality)
for (i in 1:as.numeric(range(results.zone2$value)[2]) - as.numeric(range(results.zone2$value)[1])){
  a.value <- as.numeric(range(results.zone2$value)[2]) - as.numeric(range(results.zone2$value)[1])
  
  if (a.value > 4){
    results.zone2 <- subset(results.zone2, results.zone2$value != max(results.zone2$value))
  }else{
  }
}


#create subsets with publication time within 2hrs, 6hrs, 24hrs
results <- read.csv("/home/stathis/Desktop/results.gstat.8.may.1502.csv")
results$unixtime <- lubridate::parse_date_time(results$posted.time, '%m/%d/%y %I:%M:%S %p')
results$unixtime <- as.numeric(as.POSIXct(results$unixtime))
hrs.2 <- min(results$unixtime) + (3600 * 2 + 1)
hrs.6 <- min(results$unixtime) + (3600 * 6 + 1)
hrs.24 <- min(results$unixtime) + (3600 * 24 + 1)

results.2hrs <- subset(results, results$unixtime < hrs.2)
results.6hrs <- subset(results, results$unixtime < hrs.6)
results.24.hrs <- subset(results, results$unixtime < hrs.24)
results.total <- results
write.csv(results.2hrs, "/home/stathis/Desktop/results.2hrs.svm.gstat.8may.lstm.csv")
write.csv(results.6hrs, "/home/stathis/Desktop/results.6hrs.gstat.8may.svm.csv")
write.csv(results.24.hrs, "/home/stathis/Desktop/results.24hrs.gstat.8may.svm.csv")
write.csv(results.total, "/home/stathis/Desktop/results.totalhrs.gstat.8may.svm.csv")
