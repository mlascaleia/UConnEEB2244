# initialize

rm(list = ls())

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

# organize data

# GBIF.org (29 January 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.dknyen

ceor <- read.csv("SDMs/rawDownloads/ceor.csv", sep = "\t") %>%
  select(lat = decimalLatitude, lon = decimalLongitude) %>%
  mutate(plant = "ceor") %>%
  dplyr::filter(!is.na(lon))

# GBIF.org (29 January 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.m3y9bx

pumo <- read.csv("SDMs/rawDownloads/pumo.csv", sep = "\t") %>%
  select(lat = decimalLatitude, lon = decimalLongitude) %>%
  mutate(plant = "pumo")

climate <- rast(x = list.files("ClimateData/", full.names = T)) 

noExtremes <- st_bbox(c(xmax = 180, xmin = -180, ymax = 90, ymin = -60))
climate <- crop(climate, noExtremes)

# fix cool

values(climate$cool)[values(climate$cool) < -25 & !is.na(values(climate$cool))] <- -25

# fix MAP

values(climate$MAP)[values(climate$MAP) > 2500 & !is.na(values(climate$MAP))] <-
  2500 + values(climate$MAP)[values(climate$MAP) > 2500 & !is.na(values(climate$MAP))]/100

# fix MAT

values(climate$MAT)[values(climate$MAT) < -5 & !is.na(values(climate$MAT))] <- -5
values(climate$MAT)[values(climate$MAT) > 28 & !is.na(values(climate$MAT))] <- 28

# fix warm

values(climate$warm)[values(climate$warm) < 12 & !is.na(values(climate$warm))] <- 12
values(climate$warm)[values(climate$warm) > 38 & !is.na(values(climate$warm))] <- 38

# fix wet

values(climate$wet)[values(climate$wet) > 300 & !is.na(values(climate$wet))] <-
  300 + values(climate$wet)[values(climate$wet) > 300 & !is.na(values(climate$wet))]/10

# fix plants

c.sf <- st_as_sf(ceor, coords = c("lon", "lat"), crs = st_crs(climate))
p.sf <- st_as_sf(pumo, coords = c("lon", "lat"), crs = st_crs(climate))

america <- st_bbox(c(xmax = 0, xmin = -180, ymax = 90, ymin = -60))
asia <- st_bbox(c(xmax = 180, xmin = 0, ymax = 90, ymin = -60))

ceorAm <- st_crop(c.sf, america)
ceorAs <- st_crop(c.sf, asia)
pumoAm <- st_crop(p.sf, america)
pumoAs <- st_crop(p.sf, asia)

cap <- function(x, cappy){
  return(x[sample(1:nrow(x), cappy),])
}

maxx <- 500

set.seed(42)

ceorAm <- cap(ceorAm, maxx)
ceorAs <- cap(ceorAs, maxx)
pumoAm <- cap(pumoAm, maxx)
pumoAs <- cap(pumoAs, maxx)

# climate <- rast(x = list.files("ClimateData/", full.names = T)) 
# 
# noExtremes <- st_bbox(c(xmax = 180, xmin = -180, ymax = 90, ymin = -60))
# climate <- crop(climate, noExtremes)

vines <- rbind(
  cbind.data.frame(species = "CEOR",
                 range = "invasive",
                 st_coordinates(ceorAm),
                 terra::extract(climate, st_coordinates(ceorAm))),
  cbind.data.frame(species = "CEOR",
                   range = "native",
                   st_coordinates(ceorAs),
                   terra::extract(climate, st_coordinates(ceorAs))),
  cbind.data.frame(species = "PUMO",
                   range = "invasive",
                   st_coordinates(pumoAm),
                   terra::extract(climate, st_coordinates(pumoAm))),
  cbind.data.frame(species = "PUMO",
                   range = "native",
                   st_coordinates(pumoAs),
                   terra::extract(climate, st_coordinates(pumoAs)))
) %>% dplyr::select(species, range, MAT, MAP, 
                minCool = cool, maxWarm = warm, maxWet = wet,
                lat = Y, lon = X) %>%
  mutate(id = paste(species, 1:nrow(.), sep = "_")) %>%
  dplyr::relocate(id) 

vines.sf <- st_as_sf(vines, coords = c("lon", "lat"), crs = st_crs(climate)) %>%
  select(id, species)

# st_write(vines.sf[vines.sf$species %in% "CEOR",], "vine.shp", 
#          layer = "Celastrus orbiculatus")
# 
# st_write(vines.sf[vines.sf$species %in% "PUMO",], "vine.shp", 
#          layer = "Pueraria_montana", append = T)

vines <- vines %>%
  arrange(species, desc(range)) %>%
  select(-id, CODE = species)

write.csv(vines, "vines.csv", row.names = F)



# st_write(vines.sf, "plantData.kml")
# st_write(vines.sf, "vines.shp")
vn <- st_as_sf(vines[vines$range %in% "native", ], coords = c("lon","lat"), crs = st_crs(vines.sf))
vines.sf$id <- NULL
colnames(vines.sf) <- c("CODE", "geometry")

st_write(vines.sf, "vines.shp")

st_write(vn , "vinesNative.shp")
library(raster)
climi <- brick(climate)
# raster::KML(climi, "climateData.kml", overwrite = T)

png("climi.png", width = 1600, height = 1000)

plot(climi[[1]], legend.only = T)
plot(climi[[2]], legend.only = T)
plot(climi[[3]], legend.only = T)
plot(climi[[4]], legend.only = T)
plot(climi[[5]], legend.only = T)

dev.off()







