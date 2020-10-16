
#Sep 2020:
Added removeURL() function which removes urls from texts.

#29 - DEC:

uploaded: word_list.csv and area shape file.
The uploaded scripted along with the data was tested during december 2019 and worked 100%. The O/S used was an ubuntu 18.04 LTS. The R-version along with the packages were up-to-date. According to O/S and updates in versions of both libraries and R, the script might needs minor adjustments in order to be compatible. As the research is on-going, it will be constantly updated almost every 1 - 1.5 years.


#Quick instructions

Modify the path in line 20, according to where you have stored all the files:
          setwd("/path-to-your-local-drive/")
Ensure that you have the following libraries* installed:

splitstackshape, lubridate, spdep, splancs, rgdal,
plyr,rgeos,kriging,rgdal,raster,ggplot2,GISTools,geoR


Run the whole tweets.R script at once

#for any problems contact: e.arapostathis@gmail.com, sarapos@hua.gr


*According to the R version, and operating system used, some dependencies of the libraries might need to be installed manually, by using the official R repository.

