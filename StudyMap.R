#Kayla Blincow
#1/5/2023

#Lionfish Study Map


#The purpose of this script is generate a study map for the lionfish paper.
#The resulting map should have a USVI general context panel, with an Caribbean
#context inlay. Beneath this will be two maps depicting the acoustic receiver
#arrays at Buck Island and Grammanik Bank.
#Not sure whether to include bathymetry or not at this point.


#NEED TO DO
#figure out bathymetry data?

#clear my workspace
rm(list = ls())

#load my packages
library(tidyverse)
library(sf)
library(maps)
#library(rgdal)
library(terra)
library(tidyterra)
library(sp)
library(raster)
#library(concaveman)
library(grid)
#library(ggsn)
library(leaflet)
library(patchwork)


#load my data

#station data
d <- read.csv("stationdata.csv")

#caribbean context data
carib <- map_data("world")
carib2 <- sf::st_as_sf(carib, coords = c("long", "lat"),
                       crs = "+proj=longlat +datum=NAD83") %>%  
  group_by(group) %>% 
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

#USVI context data
VI3 <- st_read("Data/USVI_BVI_shapefile/VI.shp")
VI3 <- st_transform(VI3, CRS("+proj=longlat +datum=NAD83"))


#TCRMP Station Data
tcrmp <- read.csv("Data/TCRMP/TRCRMP_sitemetadata.csv") %>% 
  dplyr::filter(Location != "Cane Bay" & Location != "Great Pond")

#bathymetry data
greg <- raster("Data/bathymetrydata/st_thomas_st_john_13_mhw_2014.nc", varname = "Band1")
#plot(greg)
crs(greg) <- "+proj=longlat +datum=NAD83"
greg2 <- as(greg, "SpatialPixelsDataFrame")
greg_df <- as.data.frame(greg2)


greg_VI <- raster("Data/bathymetrydata/usvi_1_mhw_2014.nc")
#plot(greg_VI)
crs(greg_VI) <- "+proj=longlat +datum=NAD83"
greg_VI2 <- as(greg_VI, "SpatialPixelsDataFrame")
gregVI_df <- as.data.frame(greg_VI2)



#Let's start with the contextual maps
xlabs <- c(-65.1, -65.0, -64.9, -64.8, -64.7, -64.6, -64.5, -64.4)
ylabs <- c(17.6, 17.8, 18.0, 18.2, 18.4, 18.6, 18.8)
  
USVI <- ggplot() +
    geom_tile(data = gregVI_df,
              aes(x = x, y = y, fill = GDAL.Band.Number.1)) +
  geom_sf(data = VI3, color = "gray60", fill = "gray60") +
  geom_point(data = dplyr::filter(d, region == "BUIS"), 
             aes(x = mean(long_nad83), y = mean(lat_nad83)),
             shape = 1, size = 6) +
  geom_text(data = dplyr::filter(d, region == "BUIS"), 
             aes(x = mean(long_nad83), y = mean(lat_nad83)),
             label = "Buck Island Array", nudge_x = -0.045, nudge_y = 0.02) +
  geom_point(data = dplyr::filter(d, region == "GRBK"),
             aes(x = mean(long_nad83), y = mean(lat_nad83)),
             shape = 1, size = 6) +
  geom_text(data = dplyr::filter(d, region == "GRBK"),
             aes(x = mean(long_nad83), y = mean(lat_nad83)),
             label = "Grammanik Bank Array", nudge_x = 0.05, nudge_y = -0.02) +
  geom_point(data = tcrmp, aes(x = Longitude, y = Latitude, shape = ReefComplex),
             size = 2.5) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)", shape = "TCRMP Sites") +
  coord_sf(ylim = c(17.65, 18.42), xlim = c(-65.10, -64.4)) +
  scale_fill_gradientn(limits = c(min(gregVI_df$GDAL.Band.Number.1), 0),
                       colors = c("black", "navy", "dodgerblue",
                                         "lightblue2", "white"),
                       breaks = c(-4500, -2500, -500)) +
  # scale_x_continuous(label = xlabs, breaks = xlabs) +
  # scale_y_continuous(label = ylabs, breaks = ylabs) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom",
        legend.text = element_text(size = 10)) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering()
  ) +
  guides(
    fill = guide_colorbar(position = "bottom"),
    shape = guide_legend(position = "top")
  ) 

inset <- ggplot() +
  geom_sf(data = carib2,
               color = "gray60", fill = "gray60") +
  ggplot2::annotate("segment", x = -60, xend = -64.5, y = 24, yend = 18.8,
           linewidth = 1.5, arrow = arrow()) +
  ggplot2::annotate(geom = "rect", ymin = 17.65, ymax = 18.42, xmin = -65.10, 
                    xmax = -64.4, color = "black", alpha = 0) +
  coord_sf(xlim = c(-95, -60), ylim = c(9, 35)) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  ggspatial::annotation_scale(location = "tr", width_hint = 0.25) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(text_size = 6),
    height = unit(.25, "in"),
    width = unit(.25, "in")
  ) 

#USVI + inset_element(inset, 0.1, 0.7, 0.9, 0.95)

#prep array map data
greg_df2 <- greg_df %>% dplyr::filter(x > -64.9715 & x < -64.9435 & y > 18.1852 & y < 18.199)
gregVI_df2 <- gregVI_df %>% filter(x > -64.609 & x < -64.58 & y > 17.7983 & y < 17.8118)

#add array maps
#Want to add 100 m contour line
bathy_c <- rast("Data/bathymetrydata/usvi_1_mhw_2014.nc")
crs(bathy_c) <- "+proj=longlat +datum=NAD83"

grad_lims <- c(-450, -5)

grbk <- ggplot() +
  geom_tile(data = greg_df2, aes(x = x, y = y,
                                 fill = GDAL.Band.Number.1)) + 
  geom_spatraster_contour(data = bathy_c, breaks = c(-100), alpha = 0) +
  geom_point(data = dplyr::filter(d, region == "GRBK"), 
             aes(x = long_nad83, y = lat_nad83)) +
  coord_sf(xlim = c(-64.97, -64.945), ylim = c(18.186, 18.198)) +
  ggplot2::annotate(geom = "text", x = -64.9695, y = 18.1955,
           label = "Grammanik Bank Array", hjust = 0) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  scale_y_continuous(breaks = c(18.186, 18.189, 18.192, 18.195, 18.198)) +
  scale_x_continuous(breaks = c(-64.9675, -64.9575, -64.9475)) +
  scale_fill_gradientn(limits = grad_lims,
                       values = c(0, 0.7864, 0.7865, 0.91175, 1),
                       colors = c("#0c0538","navy",  
                                  "#0525f7", "#7dcefa", "white")
                       ) +
  theme_classic() +
  theme(legend.position = "none") +
  ggspatial::annotation_scale(location = "tl", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(text_size = 6),
    height = unit(.25, "in"),
    width = unit(.25, "in")
  )




buis <- ggplot() +
  geom_tile(data = gregVI_df2,
            aes(x = x, y = y, fill = GDAL.Band.Number.1)) + 
  geom_spatraster_contour(data = bathy_c, breaks = c(-100), alpha = 0) +
  geom_point(data = dplyr::filter(d, region == "BUIS"), 
             aes(x = long_nad83, y = lat_nad83)) +
  coord_sf(xlim = c(-64.6075, -64.583), ylim = c(17.799, 17.811)) +
  ggplot2::annotate(geom = "text", x = -64.6073, y = 17.8016,
           label = "Buck Island Array", hjust = 0) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  scale_x_continuous(breaks = c(-64.605,  -64.595, -64.585))+
  scale_fill_gradientn(limits = grad_lims,
                       values = c(0, 0.7864, 0.7865, 0.91175, 1),
                       colors = c("#0c0538","navy",  
                                  "#0525f7", "#7dcefa", "white")
  ) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(text_size = 6),
    height = unit(.25, "in"),
    width = unit(.25, "in")
  )


#combine them all??
combo <- (USVI) + (inset/grbk/buis) + plot_layout(widths = c(2, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

##save the map
png(file="PrelimFigs/StudyMap2.png",
    width = 3500,
    height = 2700,
    res = 300)

combo

dev.off()


tiff(file="PrelimFigs/StudyMap2.tiff",
     width = 3500,
     height = 2700,
     res = 300)  
combo

dev.off()
