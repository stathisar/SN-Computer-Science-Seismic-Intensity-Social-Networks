          rm(list = ls())
          #required libraries
          require("RTextTools")
          require("splitstackshape")
          require("lubridate")
          require("spdep")
          require("splancs")
          require("rgdal")
          require("plyr")
          require("rgeos")
          require("kriging")
          require("rgdal")
          require("raster")
          #require("ggplot2")
          #require("GISTools")
          #install.packages("geoR")
          require("geoR")
          #require("OpenStreetMap")
          #trace("create_matrix", edit=T)     #change Acronym to acronym in line42
          setwd("~/Desktop/")
          source("./create_matrix2.R")
          removeURL <- function(data){
            gsub('http.*\\s*', '', data)
          }
          start.time <- Sys.time()
          
          
          #load of data
          tweets <- read.csv("./lesvosdataset.csv")
    
          word.list <- read.csv("./word_patterns.csv")
          #select tweets written in greek and english
          tweets <- subset(tweets, 
                           tweets$X.M..language.. == "el" | tweets$X.M..language.. == "en")
          tweets$Text <- removeURL(tweets$Text)
          rownames(tweets) <- 1:62011
          
          #word.list$value <- as.numeric(word.list$value)
          
          word.list$Word.patterns <- as.character(word.list$Word.patterns)
          word.list$value <- gsub(",", ".", word.list$value)
          word.list <- word.list[order(word.list$value), ]  
          names(word.list)
          
          tweets$detected <- NA
          tweets$value <- NA
          tweets$Text <- as.character(tolower(tweets$Text))
          
          #creation of dataframe in which classified values will be stored
          tweets.classified <- data.frame(matrix(nrow = 0, 
                                                 ncol = NCOL(tweets)))
          names(tweets.classified) <- names(tweets)
          
          #loop that detects word match, classifies and replicates according to word match
          for (i in 1:NROW(word.list)){
            tweets.test <- tweets[ grepl(word.list$Word.patterns[i], 
                                         tweets$Text), ]
            if(NROW(tweets.test) == 0){
            }else{
              tweets.test$value <- word.list$value[i]
              tweets.classified <- rbind(tweets.classified, 
                                         tweets.test)
            }
          }
          
          #storing output to tweets.with.int and removing dataframes that are no longer used
          tweets.with.int <- tweets.classified
          rm(tweets.test, tweets.classified)
          
          #addition of a sequential id to tweets.with.int
          rownames(tweets.with.int) <- 1:NROW(tweets.with.int)
          
          
          #creation of prediction.data dataframe that will be used for SVM
          prediction.data <- tweets.with.int
          #import of training dataset for svm classification
          training.dataset <- read.csv("./svm.improvements.training.dataset.csv")
          #creation of dtMatrix based on Text of training dataset
          dt.matrix <- create_matrix(training.dataset["Text"])
          
          #creation of container
          container <- create_container(dt.matrix, 
                                        training.dataset$ambiguity, 
                                        trainSize = 1:NROW(training.dataset), 
                                        virgin = FALSE)
          
          #creation of SVM Model
          model <- train_model(container, "SVM", 
                               kernel="linear", 
                               cost=1)
          
          #creation of prediction matrix
          pred.matrix <- create_matrix(prediction.data, 
                                       originalMatrix = dt.matrix)
          
          #creation of prediction container
          prediction.container <- create_container(pred.matrix, 
                                                   labels = rep(0, NROW(prediction.data)), 
                                                   testSize = 1:NROW(prediction.data), 
                                                   virgin = FALSE)
          
          #prediction of results
          tweets.intensity.svm <- classify_model(prediction.container, 
                                                 model)
          
          
          #creation of a logical structure for the prediction output
          tweets.intensity.svm$id <- row.names(tweets.intensity.svm)
          prediction.data$id <- row.names(prediction.data)
          tweets.intensity.svm.filtered <- merge(tweets.intensity.svm, 
                                                 prediction.data, by = "id")
          
          #export data
          write.csv(tweets.intensity.svm.filtered, 
                    "./tweets.intensity.svm.filtered.20.oct.csv")
          
          
          #removing ambiguity from classified data
          tweets.intensity.final <- subset(tweets.intensity.svm.filtered, 
                                           tweets.intensity.svm.filtered$SVM_LABEL != 1)
          rm(tweets.with.int, 
             tweets.intensity.svm.filtered, 
             pred.matrix, 
             container, 
             prediction.container, training.dataset,
             model, prediction.data, dt.matrix,
             tweets.intensity.svm)
          
          #georeferencing of classified tweets
          
          #import of geolocations
              geolocations <- read.csv("./geolocations.csv")
              geolocations$name <- as.character(tolower(geolocations$name))
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
                        "./georeferenced.prerandomized.oct.16.csv")
              
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
            spdf.prerandomize <- SpatialPointsDataFrame(coords = final.results.prerandomized[ ,c("x", "y")], 
                                                       data = final.results.prerandomized, 
                                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
            
            
            #spatial join of municipality name on spdfprerandomize
            spdf.prerandomize@data$NAME <- NA
            spdf.prerandomize@data$id <- NA
            spdf.prerandomize@data$distance.from.epic <- NA
            spdf.prerandomize@data[, c("NAME", "id", "distance.from.epic")] = over(spdf.prerandomize, 
                                                                                   area.map[, c("NAME", "id", "distance.from.epic")])
            
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
                i <- i+1
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
            write.csv(random.points, file = "./randompointswithoutvalues.csv")
            
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
            #unique(final.results.prerandomized$precision)
            #unique(subset(final.results.prerandomized$detected, final.results.prerandomized$precision == 0))
            
            #keeping initial coordinates for xy out of Greece and tw$precision == 2    !!!!!!!!!!!!!!!!!!!!!!!!!!! check about smirni,klp
            #precision.2 <- subset(final.results.prerandomized, final.results.prerandomized$precision == 2)
            #precision.2$x.y <- precision.2$x.x
            #precision.2$y.y <- precision.2$y.x
            tweets.randomized.final <- subset(final.results.prerandomized, 
                                              final.results.prerandomized$precision == 0 | 
                                              final.results.prerandomized$precision == 3 |
                                              final.results.prerandomized$precision == 2 | 
                                              final.results.prerandomized$precision == 4)
            
            #tweets.randomized.final <- subset(final.results.prerandomized, 
            #                                  final.results.prerandomized$precision == 2 | 
            #                                    final.results.prerandomized$precision == 3 | 
            #                                    final.results.prerandomized$precision == 4)
            
            write.csv(tweets.randomized.final, file = "./tweets.randomized.final.oct.16.csv")
            
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
            
            
            
            write.csv(results, file = "./validation.1.geo.filtered.oct.16.csv")
            #results <- read.csv("./final.geo.filtered.oct.5.csv")
            #remove data that are not used any more
            rm(map.count, random.points, final.results.prerandomized, final.results.prerandomized.2, geolocations,
               tweets.randomized.final, tweets.intensity.final, wrong.values, wrong.values.2)
         #   View(results[1:10, ])
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
            min.value
            
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
            
            unique(results.zone1$municipality)
            
            #remove wrong values from zone2
            unique(results.zone2$municipality)
            for (i in 1:as.numeric(range(results.zone2$value)[2]) - as.numeric(range(results.zone2$value)[1])){
              a.value <- as.numeric(range(results.zone2$value)[2]) - as.numeric(range(results.zone2$value)[1])
              
              if (a.value > 4){
                results.zone2 <- subset(results.zone2, results.zone2$value != max(results.zone2$value))
              }else{
              }
            }
            
            
            #rbind validated data
            results <- rbind(results.zone1, results.zone2)
            rm(results.zone1, results.zone2)
            
            #addition of 8 0-valued points
            results.8.rows <- results[1:8, ]
            results.8.rows$value <- 0
            #point down left
            results.8.rows$x.y[1] <- as.numeric(min(results$x.y)) - 0.2
            results.8.rows$y.y[1] <- as.numeric(min(results$y.y)) - 0.2
            #point upper left
            results.8.rows$x.y[2] <- as.numeric(min(results$x.y)) - 0.2
            results.8.rows$y.y[2] <- as.numeric(max(results$y.y)) + 0.2
            #point upper right
            results.8.rows$x.y[3] <- as.numeric(max(results$x.y)) + 0.2
            results.8.rows$y.y[3] <- as.numeric(max(results$y.y)) + 0.2
            #point down right
            results.8.rows$x.y[4] <- as.numeric(max(results$x.y)) + 0.2
            results.8.rows$y.y[4] <- as.numeric(min(results$y.y)) - 0.2
            #point mid-x max y (upper middle)
            results.8.rows$x.y[5] <- (as.numeric(min(results$x.y)) - 0.2 ) + (as.numeric(max(results$x.y)) + 0.2) / 2
            results.8.rows$y.y[5] <- as.numeric(max(results$y.y)) + 0.2
            #point mid-x min y (down middle)
            results.8.rows$x.y[6] <- (as.numeric(min(results$x.y)) - 0.2 ) + (as.numeric(max(results$x.y)) + 0.2) / 2
            results.8.rows$y.y[6] <- as.numeric(min(results$y.y)) - 0.2
            #point left mid-y and x min
            results.8.rows$x.y[7] <- as.numeric(min(results$x.y)) - 0.2
            results.8.rows$y.y[7] <- (as.numeric(min(results$y.y)) -0.2 ) + (as.numeric(max(results$y.y)) + 0.2) / 2
            #point right mid-y and x max
            results.8.rows$x.y[8] <- as.numeric(max(results$x.y)) + 0.2
            results.8.rows$y.y[8] <- (as.numeric(min(results$y.y)) - 0.2 ) + (as.numeric(max(results$y.y)) + 0.2) / 2
            
            #addition of 8 zero valued point to results
            results <- rbind(results, results.8.rows)
            #kriging interpolatio n
            names(results)
        results$posted.time[1]
            #create subsets with publication time within 2hrs, 6hrs, 24hrs
  
            results$unixtime <- lubridate::parse_date_time(results$posted.time, '%m/%d/%y %I:%M:%S %p')
            results$unixtime <- as.numeric(as.POSIXct(results$unixtime))
            hrs.2 <- min(results$unixtime) + (3600 * 2 + 1)
            hrs.6 <- min(results$unixtime) + (3600 * 6 + 1)
            hrs.24 <- min(results$unixtime) + (3600 * 24 + 1)
            
            results.2hrs <- subset(results, results$unixtime < hrs.2)
            results.6hrs <- subset(results, results$unixtime < hrs.6)
            results.24.hrs <- subset(results, results$unixtime < hrs.24)
            results.total <- results
           
             #select the necessary columns of results dataframe
         
               #kriging 2hrs
              results <- results.2hrs
              rm(results.2hrs)
              results <- results[ ,c("uniqueid", 
                                     "id", 
                                     "value", 
                                     "x.y", 
                                     "y.y")]
              #convert data type of coordinates to numeric
              results$x.y <- as.numeric(results$x.y)
              results$y.y <- as.numeric(results$y.y)
              #results <- read.csv("/home/stathis/Desktop/to.check.corrected.csv")
              #convert data frame to spatial points data frame
              results <-SpatialPointsDataFrame(coords = results[ , c("x.y", "y.y")], 
                                               data = results, 
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
              
              #creation of variogram in order to determine the kriging model
              v <- variog(coords =  results@coords, 
                          data = results@data$value)
              #plot variogram
              plot(v)
              
              #perform kriging interpolation
              #Sys.time()
              kriging <- kriging(x = results@coords[ ,1], 
                                 y = results@coords[ ,2], 
                                 response = results@data$value, 
                                 #range = 0.5,
                                 lags = 10,
                                 pixels = 600)
            #write.csv(results@data, "~/Desktop/to.look.carefully.csv")
            #create raster
            krig.raster = rasterFromXYZ(kriging$map)
            
            #mask according to area.map
            r.masked.to.greece <- mask(krig.raster, 
                                       area.map)
            #plot final raster
            plot(r.masked.to.greece)
            
            
            #create subsets with data published within 2h, 6h and 24h respectively
  
  
  writeRaster(r.masked.to.greece, '~/Desktop/intensity.raster.2hrs.oct.22.600.pixels.tif')
  
  #kriging 6hrs
  results <- results.6hrs
  rm(results.6hrs)
  results <- results[ ,c("uniqueid", 
                         "id", 
                         "value", 
                         "x.y", 
                         "y.y")]
  #convert data type of coordinates to numeric
  results$x.y <- as.numeric(results$x.y)
  results$y.y <- as.numeric(results$y.y)
  #results <- read.csv("/home/stathis/Desktop/to.check.corrected.csv")
  #convert data frame to spatial points data frame
  results <-SpatialPointsDataFrame(coords = results[ , c("x.y", "y.y")], 
                                   data = results, 
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #creation of variogram in order to determine the kriging model
  v <- variog(coords =  results@coords, 
              data = results@data$value)
  #plot variogram
  plot(v)
  
  #perform kriging interpolation
  #Sys.time()
  kriging <- kriging(x = results@coords[ ,1], 
                     y = results@coords[ ,2], 
                     response = results@data$value, 
                     #range = 0.5,
                     lags = 10,
                     pixels = 600)
  
  #create raster
  krig.raster = rasterFromXYZ(kriging$map)
  
  #mask according to area.map
  r.masked.to.greece <- mask(krig.raster, 
                             area.map)
  #plot final raster
  plot(r.masked.to.greece)
  
  
  writeRaster(r.masked.to.greece, '~/Desktop/intensity.raster.6.hrs.oct.22.600.pixels.tif')
  
  #kriging 24hrs
  results <- results.24.hrs
  rm(results.24.hrs)
  results <- results[ ,c("uniqueid", 
                         "id", 
                         "value", 
                         "x.y", 
                         "y.y")]
  #convert data type of coordinates to numeric
  results$x.y <- as.numeric(results$x.y)
  results$y.y <- as.numeric(results$y.y)
  #results <- read.csv("/home/stathis/Desktop/to.check.corrected.csv")
  #convert data frame to spatial points data frame
  results <-SpatialPointsDataFrame(coords = results[ , c("x.y", "y.y")], 
                                   data = results, 
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #creation of variogram in order to determine the kriging model
  v <- variog(coords =  results@coords, 
              data = results@data$value)
  #plot variogram
  plot(v)
  
  #perform kriging interpolation
  #Sys.time()
  kriging <- kriging(x = results@coords[ ,1], 
                     y = results@coords[ ,2], 
                     response = results@data$value, 
                     #range = 0.5,
                     lags = 10,
                     pixels = 600)
  #create raster
  krig.raster = rasterFromXYZ(kriging$map)
  
  #mask according to area.map
  r.masked.to.greece <- mask(krig.raster, 
                             area.map)
  
  plot(r.masked.to.greece)
  
  
  writeRaster(r.masked.to.greece, '~/Desktop/intensity.raster.24.hrs.oct.22.600.pixels.tif')
  
  
  #kriging total
  results <- results.total
  rm(results.total)
  results <- results[ ,c("uniqueid", 
                         "id", 
                         "value", 
                         "x.y", 
                         "y.y")]
  #convert data type of coordinates to numeric
  results$x.y <- as.numeric(results$x.y)
  results$y.y <- as.numeric(results$y.y)
  #results <- read.csv("/home/stathis/Desktop/to.check.corrected.csv")
  #convert data frame to spatial points data frame
  results <-SpatialPointsDataFrame(coords = results[ , c("x.y", "y.y")], 
                                   data = results, 
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #creation of variogram in order to determine the kriging model
  v <- variog(coords =  results@coords, 
              data = results@data$value)
  #plot variogram
  plot(v)
  
  #perform kriging interpolation
  #Sys.time()
  kriging <- kriging(x = results@coords[ ,1], 
                     y = results@coords[ ,2], 
                     response = results@data$value, 
                     #range = 0.5,
                     lags = 10,
                     pixels = 600)
  #  ?kriging
  #write.csv(results@data, "~/Desktop/to.look.carefully.csv")
  #create raster
  krig.raster = rasterFromXYZ(kriging$map)
  
  #mask according to area.map
  r.masked.to.greece <- mask(krig.raster, 
                             area.map)
  #          write.csv(results@data, "~/Desktop/check.oct.18.csv")
  #plot final raster
  plot(r.masked.to.greece)
  
  
  #create subsets with data published within 2h, 6h and 24h respectively
  #re-execute the process
  
  #         end.time <- Sys.time()
  #        time.taken <- end.time - start.time
  #       time.taken
  #map creation
  
  writeRaster(r.masked.to.greece, '~/Desktop/intensity.raster.total.oct.22.600.pixels.tif')
  


 map <- function(){
  plot(r.masked.to.greece, main = "Seismic Intensity Map, created, \n from tweet-text corpus", 
       xlab = "Long", 
       ylab = "Lat")
  plot(area.map, add = TRUE)
#  plot(results, add = TRUE)
  
}
map()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
