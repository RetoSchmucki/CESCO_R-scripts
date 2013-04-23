rm(list=ls())

library(raster)


setwd("/path/to/directory")


# load raster *ascii files
#==================================================
asp_median <-raster("asp_median.asc")
masq_hydro <-raster("masq_hydro1k.asc")


# make a copy for the original file, just in case
#==================================================
asp_median_ori <- asp_median


# get x and y coordinates for missing values
#==================================================
x<-xFromCell(masq_hydro,which(is.na(masq_hydro[,])))
y<-yFromCell(masq_hydro,which(is.na(masq_hydro[,])))


# find nearest neighbor(s) with a value and replace
# the missing with the value or the median of the neighbor(s)
#=============================================================

# Step 1. get the furthest point first

min_dist <- NULL

for (i in 1:length(x)){
minimum_dist <- min(distanceFromPoints(asp_median,c(x[i],y[i]))[,][which(!is.na(asp_median[,]))])
min_dist <- c(min_dist,minimum_dist)
}

# Step 2. oder to compute the furthest value first
# (The rational behind this is that we limit the second hand 
# estimation and maximize the use of known values)

ord_serie <- order(min_dist,decreasing=TRUE)


# Step 3. proceed with replacement of missing value, starting with
# the furthest

for (i in ord_serie){
minimum_dist <- min(distanceFromPoints(asp_median,c(x[i],y[i]))[,][which(!is.na(asp_median[,]))])
nn_index <- which(distanceFromPoints(asp_median,c(x[i],y[i]))[,]==minimum_dist)[which(distanceFromPoints(asp_median,c(x[1],y[1]))[,]==minimum_dist) %in% which(!is.na(asp_median[,]))]
asp_median[which(is.na(masq_hydro[,]))[i]] <- median(getValues(asp_median)[nn_index])
}

writeRaster(asp_median,file="asp_median_corr.asc",format="ascii")
