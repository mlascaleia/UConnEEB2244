# initialize

library(tidyverse)
library(sf)
library(ggmap)
library(terra)
library(spatialEco)
library(tidyterra)

# load data 

# Set bounding box for target area

box <- st_bbox(c(xmin = -72.262066, 
                 xmax = -72.253, 
                 ymax = 41.817960, 
                 ymin = 41.809073), 
               crs = st_crs(4326))


# make smaller shapefiles from large shapefiles
# larger shapefiles not on github - uploaded to huskyCT in
# this week's discussion folder

# roads <- read_sf("shapefiles/Road.shp")
# parcels <- read_sf("shapefiles/deepgis_DEEP_PARCEL.shp")
# water <- read_sf("shapefiles/Connecticut_Hydrography_Set.shp")
# water <- water[water$OBJECTID == 123485, ]
# 
# roads.b <- st_crop(roads, box)
# parcels <- st_make_valid(parcels)
# parcels.b <- st_crop(parcels, box)
# water.b <- st_crop(water, box)
# plot(water.b$geometry)
# 
# theRoad <- parcels <- parcels[parcels$OBJECTID == 570975, ]
# 
# st_write(roads.b, "small_shapefiles/roads.shp")
# st_write(parcels.b, "small_shapefiles/parcels.shp")
# st_write(water, "small_shapefiles/water2.shp")
# st_write(theRoad, "small_shapefiles/theRoad.shp")

# load small shapefiles

ro <- read_sf("cameraTraps/small_shapefiles/roads.shp")
pa <- read_sf("cameraTraps/small_shapefiles/pa_edits.kml")
rod <- read_sf("cameraTraps/small_shapefiles/theRoad.shp")
wa <- read_sf("cameraTraps/small_shapefiles/water2.shp")

# make edits to these shapefiles (not needed)

# pa$OBJECTID[pa$OBJECTID == 572630] <- "cemetery"
# pa$OBJECTID[pa$OBJECTID == 572638] <- "north hall"
# pa$OBJECTID[pa$OBJECTID == 570975] <- "road"
# pa$OBJECTID[pa$OBJECTID == 572757] <- "tls"
# pa$OBJECTID[pa$OBJECTID == 572360] <- "woods"
# pa$OBJECTID[pa$OBJECTID == 571943] <- "busby"

# pa <- pa[!grepl("^5", pa$OBJECTID), ]

# fix the areas that are not actually forest

woods <- st_union(pa[pa$Name %in% c("woods", "isWoods", "isWoods2"), ])
woods <- st_as_sf(st_difference(woods, pa[pa$Name %in% "notWoods", ]))

# If you want to delete some roads:

# ro$OBJECTID[ro$OBJECTID %in% c(25589,25901,25958,26065,26207)] <- "keep"
# ro <- ro[ro$OBJECTID %in% "keep", ]

roneat <- st_intersection(ro, pa[pa$Name %in% "north hall", ])

# only keep one small part of the water shapefile

wa <- wa[wa$OBJECTID == 123485, ]

# load camera data

locDat <- read.csv("cameraTraps/insightsDownload/deployments.csv") %>%
  select(camera = deployment_id, lon = longitude, lat = latitude,
         start = start_date, end = end_date)

camDat <- read.csv("cameraTraps/insightsDownload/sequences.csv") %>%
  select(camera = deployment_id, name = common_name, time = start_time) %>%
  right_join(locDat)

# get only the best species

wantSpecies <- c("Bobcat", "Coyote", "Eastern Chipmunk", "Eastern Gray Squirrel", 
                 "Grey Fox", "Northern Raccoon", "Red Fox", "Striped Skunk", "Sylvilagus Species", 
                 "Virginia Opossum", "White-tailed Deer", "Wild Turkey", "Woodchuck")

camDat <- camDat[camDat$name %in% wantSpecies, ]

# add some ~ jiggle~

camDat$lat <- camDat$lat + runif(nrow(camDat), -.00001, .00001)
camDat$lon <- camDat$lon + runif(nrow(camDat), -.00001, .00001)

eachSpecies <- split(camDat, f = ~name)
eachSpecies.sf <- lapply(eachSpecies, st_as_sf, coords = c("lon", "lat"), crs = 4326)

spatbox <- as.vector(st_bbox(woods))[c(1,3,2,4)]

animals <- names(eachSpecies.sf)

scaling <- log1p(unlist(lapply(eachSpecies.sf, nrow)))
scaling <- scaling/(mean(scaling) * 100)

eachSpecies.rast <- lapply(1:length(eachSpecies.sf), function(j){
  r <- log1p(sf.kde(eachSpecies.sf[[j]], res = .00001, ref = spatbox, bw = .0006, scale.factor = scaling[j]))
  names(r) <- animals[j]
  return(mask(r, woods))
})

names(eachSpecies.rast) <- animals
top <- max(unlist(lapply(eachSpecies.rast, values, na.rm = T)))

pa <- st_zm(pa)


l <- lapply(eachSpecies.rast, function(e){
  tit <- as.character(names(e))
  thisMax <- max(values(e, na.rm = T))
    pp <- ggplot() +  
      geom_sf(data = pa[!pa$Name %in% c("cemetery"),], fill = "grey95", color = "grey95") +
      geom_sf(data = pa[pa$Name %in% c("cemetery"),], fill = "#D1FFBD") +
      geom_sf(data = rod, fill = "gray") +
      geom_sf(data = ro, linewidth = 1.2, color = "gray") +
      geom_spatraster(data = e) +
      geom_sf_label(data = pa[pa$Name %in% c("cemetery"),], aes(label = Name)) +
      geom_sf_label(data = pa[pa$Name %in% c("woods"),], aes(label = Name), nudge_y = .0017, nudge_x = -.0021) +
      scale_fill_viridis_c(na.value = NA, 
                           name = "Relative\nDensity\n", 
                           limits = c(0, top),
                           breaks = c(ceiling(min(values(e), na.rm = T)), 
                                      floor(max(values(e), na.rm = T)),
                                      top),
                           labels = c("none", "max observed", "max possible"),
                           ) +
      geom_sf(data = wa, color = "blue", linewidth = 1.5) +
      # geom_sf(data = cd, color = "black", fill = "white", size = 1.5, shape = 21) +
      xlim(-72.260, -72.254) +
      ylim(41.8107, 41.8163) +
      theme_test() +
      ggtitle(tit) +
      theme(axis.title = element_blank(),
            panel.background = element_rect(fill = "grey95"))
    
    pp_d <- pp +   
      ggspatial::annotation_scale(
        location = "tr",
        pad_x = unit(0.4, "in"), pad_y = unit(1.2, "in"),
        bar_cols = c("grey60", "white")
      ) +
      ggspatial::annotation_north_arrow(
        location = "tr", which_north = "true",
        pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
        style = ggspatial::north_arrow_orienteering(fill = c("white", "grey60"))
      )
    
    return(pp_d)
})

names(l) <- animals

l$Bobcat

lapply(1:length(l), function(i){
  ggsave(plot = l[[i]], filename = paste0("cameraTraps/heatmaps/", animals[i], ".png"))
})











