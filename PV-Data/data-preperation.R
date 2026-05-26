################################################################################
#### Import and edit data
################################################################################

## Autor: Leia Betting

setwd("PV-Data/Solardaten")
##  Set the time and date settings to English
Sys.setlocale("LC_TIME", "C")

## load packages
library(matrixStats)
library(sf)
library(ggplot2)
library(dplyr)

##############
## Import Data

# 2010-2011
data10 <- read.csv("2010-2011 Solar home electricity data.csv", skip=1)
# add column Row.Quality
data10$Row.Quality <- NA
data10$date <- as.Date(data10$date, format="%d-%b-%y")

# 2011-2012
data11 <- read.csv("2011-2012 Solar home electricity data v2.csv", skip=1)
data11$date <- as.Date(data11$date, format="%d/%m/%Y")

# 2012-20113
data12 <- read.csv("2012-2013 Solar home electricity data v2.csv", skip=1)
data12$date <- as.Date(data12$date, format="%d/%m/%Y")

# complete data set
data <- rbind(data10, data11, data12)
data_all <- data

# Rename "0 a.m." to "12 a.m." to make it easier to convert to the date format
colnames(data)[colnames(data) == "X0.00"] <- "X24.00"

##############
## Add Columns

times <- grep("^X",colnames(data), value=TRUE)
mornings <- times[1:10]
IDs <- unique(data$Customer)

# daily maxima
data$day_max <- rowMaxs(as.matrix(data[,times]))

# daily sum
data$day_total <- rowSums(data[,times])

# sum of morning hours
data$morning_total <- rowSums(data[,mornings])


##################
## Filter Columns

## Row Quality: contians only NAs
any(!is.na(data$Row.Quality))
data <- data[,!(colnames(data) %in% c("Row.Quality"))]


########################
## Add further variables

## Weekday
data$weekday <- factor(weekdays(data$date),
                       levels = c("Monday","Tuesday","Wednesday","Thursday",
                                  "Friday","Saturday","Sunday"))
## Workday vs. Weekend
data$workday <- NA

data$workday[data$weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday")] <- "Workday"
data$workday[data$weekday %in% c("Friday", "Saturday", "Sunday")] <- "Weekend"

data$workday <- factor(data$workday)

## Month
data$month <- factor(months(data$date),
                     levels = c("January","February","March","April","May",
                                "June","July","August","September",
                                "October","November","December"))

## Season
data$season <- NA
data$season[data$month %in% c("December", "January", "February")] <- "Summer"
data$season[data$month %in% c("March", "April", "May")] <- "Autumn"
data$season[data$month %in% c("June", "July", "August")] <- "Winter"
data$season[data$month %in% c("September", "October", "November")] <- "Spring"

data$season <- factor(data$season, levels = c("Spring","Summer","Autumn","Winter"))

## Region
data$Region <- NA
data$Region[data$Postcode %in% c(2051:2129, 2141:2161)] <- "Sydney North"
data$Region[data$Postcode %in% c(2008:2050, 2130:2140, 2191:2196, 2201:2204, 2206)] <- "Sydney Center"
data$Region[data$Postcode %in% c(2162:2190, 2197:2200, 2205, 2207:2249)] <- "Sydney South"
data$Region[data$Postcode %in% c(2250:2263)] <- "Central Coast"
data$Region[data$Postcode %in% c(2264:2277, 2306:2330)] <- "Hunter Region"
data$Region[data$Postcode %in% c(2278:2305)] <- "Newcastle"

data$Region <- factor(data$Region, levels = c("Sydney North", "Sydney Center",
                                              "Sydney South", "Central Coast",
                                              "Hunter Region", "Newcastle"))

## only GG of interest
data <- data[which(data[,"Consumption.Category"] == "GG"),]

## quality of the data:
# 1: good quality
# 0: bad quailty: maximum generation < 0.03kwh or early moring > 0.02 kwh
data$quality <- NA
for(i in 1:nrow(data)){
  if(data$day_max[i] < 0.03 | data$morning_total[i] > 0.02){
    data$quality[i] <- 0
  } else{
    data$quality[i] <- 1
  }
}


#######################################################
## Add coordinates of the middle point of the postcodes
#######################################################

## Author: Jana Gierse

#################
## Pfad Shapefile
unzip_dir <- "POA_2021_AUST_GDA2020_SHP"
unzip("POA_2021_AUST_GDA2020_SHP.zip", exdir = unzip_dir)

##################
## load shape file
shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
poa <- sf::st_read(shp_files[1], quiet = FALSE)
save(poa, file = "Shape_Australia.RData")

#####################################
## Filter by Postcodes in the dataset
data.postcode <- split(data, data[,"Postcode"])
poa.postcodes <- poa[is.element(poa$POA_CODE21, names(data.postcode)),]
save(poa.postcodes, file = "postcodes.RData")

###################################
## distances between the postcodes
poa_centroids <- st_centroid(poa.postcodes)

# matrix with distances (in kilometers)
dist_matrix <- st_distance(poa_centroids)/1000
rownames(dist_matrix) <- poa_centroids$POA_CODE21
colnames(dist_matrix) <- poa_centroids$POA_CODE21
dist_matrix_round <- floor(dist_matrix)
save(dist_matrix_round, file = "Distanzmatrix")

# all possible distances
distanzen <- as.vector(dist_matrix)
distanzen <- sort(distanzen[which(distanzen > 0)])
distanzen_round <- floor(distanzen)

# For each distance: Determine pairs of postal codes
dist_matrix_round <- matrix(as.numeric(dist_matrix_round), nrow = nrow(dist_matrix_round))
rownames(dist_matrix_round) <- poa_centroids$POA_CODE21
colnames(dist_matrix_round) <- poa_centroids$POA_CODE21
postcods_dists <- list() # Postcodes  at the respective distance
j <- 1
for(i in unique(distanzen_round)[-1]){
  inds <- which(dist_matrix_round == i, arr.ind = TRUE)
  postcods_dists[[j]] <- apply(inds, 1, function(x){res <- c(rownames(dist_matrix_round)[x[1]], colnames(dist_matrix_round)[x[2]])})
  colnames(postcods_dists[[j]]) <- NULL
  j <- j + 1
}
names(postcods_dists) <- unique(distanzen_round)[-1]
save(postcods_dists, file = "Postcode_Distances.RData")


###################
## Distance vectors
coordtrans <- st_transform(poa_centroids, 32755)
coordtrans <- st_coordinates(coordtrans)
inds <- t(combn(1:nrow(coordtrans), 2))
inds.names <- c()
distvector <-  c()
for(i in 1:nrow(inds)){
  distvector <- rbind(distvector, coordtrans[inds[i,1],]-  coordtrans[inds[i,2],])
  inds.names <- rbind(inds.names, c(poa_centroids$POA_NAME21[inds[i,1]], poa_centroids$POA_NAME21[inds[i,2]]))
}
rownames(distvector) <- paste(inds.names[,1] , inds.names[,2])
# in km
distvector <- distvector/1000
# round
distvector <- round(distvector, 2)
save(distvector, file = "distvectors.RData")


################################
## add centroids to the data set
data$Centroid <- NA
for(i in 1:nrow(data)){
  ind <- which(poa.postcodes$POA_CODE21 == data$Postcode[i])
  data$Centroid[i] <- poa_centroids$geometry[ind]

}


############
## Save data
save(data,  file = "Clean_Data.Rdata")

