library(shiny)
library(leaflet)
library(tidyverse)
library(rgbif)
library(taxize)
library(rgdal)




#################################
##Getting data from GBIF for app##
##################################
Amphibiakey <- get_gbifid(sci='Amphibia')
Reptiliakey <- get_gbifid(sci='Reptilia')
Mammaliakey <- get_gbifid(sci='Mammalia')
Aveskey <- get_gbifid(sci='Aves')


# fill in your gbif.org credentials 
user <- "" # your gbif.org username 
pwd <- "" # your gbif.org password
email <- "" # your email 

############
###amphibia###

# use matched gbif_taxon_keys from above 
amph <- occ_download(
  pred("taxonKey", 131),
  pred_within("POLYGON((-75.92651 2.21924,-78.59619 0.40649,-77.21191 -0.87891,-73.02612 -3.74634,
      -70.78491 -4.50439,-69.64233 -4.80103,-66.74194 -4.35059,-67.30225 -1.90063,-75.92651 2.21924))"),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

#get from GBIF
amph_dat <- occ_download_get("0090739-200613084148143")
amph_dat <-occ_download_import(amph_dat)

amph_citation <- occ_download_meta(amph) %>% gbif_citation()

###############
###reptiles###

reptilia <- occ_download(
  pred("taxonKey", 358),
  pred_within("POLYGON((-75.92651 2.21924,-78.59619 0.40649,-77.21191 -0.87891,-73.02612 -3.74634,
              -70.78491 -4.50439,-69.64233 -4.80103,-66.74194 -4.35059,-67.30225 -1.90063,-75.92651 2.21924))"),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
  )

#get from GBIF
Reptilia_dat <- occ_download_get("0090745-200613084148143")
Reptilia_dat<-occ_download_import(Reptilia_dat)

reptilia_citation <- occ_download_meta(reptilia) %>% gbif_citation()

##################
###mammals#####

mammalia <- occ_download(
  pred("taxonKey", 359),
  pred_within("POLYGON((-75.92651 2.21924,-78.59619 0.40649,-77.21191 -0.87891,-73.02612 -3.74634,
              -70.78491 -4.50439,-69.64233 -4.80103,-66.74194 -4.35059,-67.30225 -1.90063,-75.92651 2.21924))"),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
  )

#get from GBIF
mammalia_dat <- occ_download_get("0091071-200613084148143")
mammalia_dat<-occ_download_import(mammalia_dat)

mammalia_citation <- occ_download_meta(mammalia) %>% gbif_citation()

##################
###mammals#####

aves <- occ_download(
  pred("taxonKey", 212),
  pred_within("POLYGON((-75.92651 2.21924,-78.59619 0.40649,-77.21191 -0.87891,-73.02612 -3.74634,
              -70.78491 -4.50439,-69.64233 -4.80103,-66.74194 -4.35059,-67.30225 -1.90063,-75.92651 2.21924))"),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

#get from GBIF

aves_dat <- occ_download_get("0091144-200613084148143")
aves_dat<-occ_download_import(aves_dat)

aves_citation <- occ_download_meta(aves) %>% gbif_citation()

###################
###cropto put poly###
#Load shapefiles
Putbasin <- readOGR("Geo/Putmayo_WaterShed.shp")
plot(Putbasin)

#load amph data nd change column names
Amph_occurrence <- dplyr::rename(amph_dat, latitude = decimalLatitude, 
                                 longitude = decimalLongitude)

# convert data frame to SpatialPoints class to shapefile and save as shapefile
lon<-Amph_occurrence[,c('longitude')]
lat<-Amph_occurrence[,c('latitude')]
coords<-SpatialPoints(cbind(lon,lat))
Amph_occurrence <- SpatialPointsDataFrame(coords,Amph_occurrence)
proj4string(Amph_occurrence) = CRS("+proj=longlat +datum=WGS84 +no_defs")

Amph_Put <- !is.na(over(Amph_occurrence, as(Putbasin, "SpatialPolygons"))) # identify points that overlaps with the study area
Amph_Put<- Amph_occurrence[which(Amph_Put),] # select points that overlaps with the study area
head(Amph_Put)
#convert to dataframe
Amph_Put<-as.data.frame(Amph_Put)
write.csv (Amph_Put, file="Amph_Put.csv")

##############
##Reptiles
# convert data frame to SpatialPoints class to shapefile and save as shapefile
lon<-Reptilia_dat[,c('decimalLongitude')]
lat<-Reptilia_dat[,c('decimalLatitude')]
coords<-SpatialPoints(cbind(lon,lat))
Reptilia_dat <- SpatialPointsDataFrame(coords,Reptilia_dat)
proj4string(Reptilia_dat) = CRS("+proj=longlat +datum=WGS84 +no_defs")

Rep_Put <- !is.na(over(Reptilia_dat, as(Putbasin, "SpatialPolygons"))) # identify points that overlaps with the study area
Rep_Put<- Reptilia_dat[which(Rep_Put),] # select points that overlaps with the study area
head(Rep_Put)
#convert to dataframe
Rep_Put<-as.data.frame(Rep_Put)
write.csv (Rep_Put, file="Rep_Put.csv")

############
##mammals

# convert data frame to SpatialPoints class to shapefile and save as shapefile
lon<-mammalia_dat[,c('decimalLongitude')]
lat<-mammalia_dat[,c('decimalLatitude')]
coords<-SpatialPoints(cbind(lon,lat))
mammalia_dat <- SpatialPointsDataFrame(coords,mammalia_dat)
proj4string(mammalia_dat) = CRS("+proj=longlat +datum=WGS84 +no_defs")

mammal_Put <- !is.na(over(mammalia_dat, as(Putbasin, "SpatialPolygons"))) # identify points that overlaps with the study area
mammal_Put<- mammalia_dat[which(mammal_Put),] # select points that overlaps with the study area

#convert to dataframe
mammal_Put<-as.data.frame(mammal_Put)
write.csv (mammal_Put, file="mammal_Put.csv")


# convert data frame to SpatialPoints class to shapefile and save as shapefile
lon<-aves_dat[,c('decimalLongitude')]
lat<-aves_dat[,c('decimalLatitude')]
coords<-SpatialPoints(cbind(lon,lat))
aves_dat <- SpatialPointsDataFrame(coords,aves_dat)
proj4string(aves_dat) = CRS("+proj=longlat +datum=WGS84 +no_defs")

aves_Put <- !is.na(over(aves_dat, as(Putbasin, "SpatialPolygons"))) # identify points that overlaps with the study area
aves_Put<- aves_dat[which(aves_Put),] # select points that overlaps with the study area

#convert to dataframe
aves_Put<-as.data.frame(aves_Put)
write.csv (aves_Put, file="aves_Put.csv")


