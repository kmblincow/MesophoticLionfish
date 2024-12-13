#Kayla Blincow
#7/20/2022

#The goal of this script to estimate centers of activities (COAs) and kernel
#utilization distributions (KUDs) for the lionfish.


#UPDATE 10/11/2022: Took a closer look at all COAs to look for out of ordinary
#movements that might not get picked up by the 95% KUD
#Couldn't get 100% KUD to calculate properly, but you can see longer distance 
#movements in the raw plot of COAs

#UPDATE 1/17/2023
#Working up figures and results for the first draft of the manuscript

#UPDATE 11/9/2023
#Need to work on the ideal KUD settings for each array type. Does it make sense
#to standardize across both? Or should I adjust based on the arrry formation?
#I think it makes sense to adjust based on the array formation


#UPDATE 5/22/2024
#Default settings of KUDs seem to work best. 
#Need to add 100m contour line to the plots and use coord_fixed to avoid distortion


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(adehabitatHR)
library(ctmm)
library(raster)
library(sf)
library(sp)
library(gt)
library(patchwork)

#load my data
d <- readRDS("DeepLionfish_clean2.rds")


#split by region
GRBK <- filter(d, region == "GRBK")
BUIS <- filter(d, region == "BUIS")

#quick abacus plots for context
GRBK2 <- GRBK %>% 
  mutate(day = date(detection_time_ast)) %>% 
  distinct(transmitter, station, day)

ggplot(GRBK2, aes(x = day, y = station)) +
  geom_point(aes(color = transmitter)) +
  theme_bw() +
  facet_wrap(~transmitter)

BUIS2 <- BUIS %>% 
  mutate(day = date(detection_time_ast)) %>% 
  distinct(transmitter, station, day)

ggplot(BUIS2, aes(x = day, y = station)) +
  geom_point(aes(color = transmitter)) +
  theme_bw() +
  facet_wrap(~transmitter)


#calculate COAs
GRBK$detection_time_coa <- floor_date(GRBK$detection_time_ast, unit = "30 minutes")

coa_df <- GRBK %>% group_by(transmitter, detection_time_coa) %>% 
  mutate(mlong = mean(long_nad83),
         mlat = mean(lat_nad83),
         nrec = length(unique(station))) %>% 
  distinct(transmitter, detection_time_coa,.keep_all = TRUE) %>% 
  data.frame()

#look at all COAs (100% positions)
ggplot(coa_df, aes(x = mlong, y = mlat)) +
  geom_point(aes(color = transmitter), alpha = 0.2) +
  facet_wrap(~transmitter)


#calculate KUDs 
kud_df <- dplyr::select(coa_df, transmitter, mlong, mlat) %>% 
  rename(id = transmitter, x = mlong, y = mlat)

#convert to spatial points dataframe
coordinates(kud_df) <- c("x", "y")
proj4string(kud_df) <- CRS("+proj=longlat +datum=NAD83")
kud_df2 <- spTransform(kud_df, CRS("+proj=utm +zone=20"))


#calculate the KUD
kud1 <- kernelUD(kud_df2, grid = 1000) ###HERE DOING THIS!!!
image(kud1)

HR95_GRBK <- getverticeshr(kud1, percent = 95, unout = "km2")

plot(HR95_GRBK)

HR50_GRBK <- getverticeshr(kud1, percent = 50, unout = "km2")

plot(HR50_GRBK)




#Do it for BUIS fish
#calculate COAs
BUIS$detection_time_coa <- floor_date(BUIS$detection_time_ast, unit = "30 minutes")

coa_df_b <- BUIS %>% group_by(transmitter, detection_time_coa) %>% 
  mutate(mlong = mean(long_nad83),
         mlat = mean(lat_nad83),
         nrec = length(unique(station))) %>% 
  distinct(transmitter, detection_time_coa,.keep_all = TRUE) %>% 
  data.frame()

#look at all COAs (100% KUD)
ggplot(coa_df_b, aes(x = mlong, y = mlat)) +
  geom_point(aes(color = transmitter), alpha = 0.2) +
  facet_wrap(~transmitter)

#calculate KUDs
kud_df_b <- dplyr::select(coa_df_b, transmitter, mlong, mlat) %>% 
  rename(id = transmitter, x = mlong, y = mlat)

#convert to spatial points dataframe
coordinates(kud_df_b) <- c("x", "y")
proj4string(kud_df_b) <- CRS("+proj=longlat +datum=NAD83")
kud_df2_b <- spTransform(kud_df_b, CRS("+proj=utm +zone=20 ellps=WGS84"))

#calculate KUD

kud2 <- kernelUD(kud_df2_b, grid = 1000)
image(kud2)

HR95_BUIS <- getverticeshr(kud2, percent = 95, unout = "km2")
HR50_BUIS <- getverticeshr(kud2, percent = 50, unout = "km2")

#make proper plots

#get unique station data
stations <- read.csv("stationdata.csv", header = T)

BUISstations <- filter(stations, region == "BUIS")
GRBKstations <- filter(stations, region == "GRBK")

#convert to spatial points dataframe
# coordinates(BUISstations) <- c("long_nad83", "lat_nad83")
# proj4string(BUISstations) <- CRS("+proj=longlat +datum=NAD83")
# BUISstats <- spTransform(BUISstations, CRS("+proj=utm +zone=20 ellps=WGS84"))
# 
# coordinates(GRBKstations) <- c("long_nad83", "lat_nad83")
# proj4string(GRBKstations) <- CRS("+proj=longlat +datum=NAD83")
# GRBKstats <- spTransform(GRBKstations, CRS("+proj=utm +zone=20 ellps=WGS84"))


#convert spatial polygons to dataframes
HR50_BUIS <- spTransform(HR50_BUIS, CRS("+proj=longlat"))
HR95_BUIS <- spTransform(HR95_BUIS, CRS("+proj=longlat"))
HR50_BUISgg <- fortify(HR50_BUIS)
HR95_BUISgg <- fortify(HR95_BUIS)

HR50_GRBK <- spTransform(HR50_GRBK, CRS("+proj=longlat"))
HR95_GRBK <- spTransform(HR95_GRBK, CRS("+proj=longlat"))
HR50_GRBKgg <- fortify(HR50_GRBK)
HR95_GRBKgg <- fortify(HR95_GRBK)

#WANT TO ADD DETECTION PERIOD ANNOTATION TO THE PANELS...
RI <- read.csv("Data/ResidencyData.csv")
RI <- dplyr::select(RI, transmitter, totdays_det, region) %>% 
  rename(id = transmitter) %>% 
  mutate(lat = 17.802, long = -64.609)
RI[RI$region == "GRBK",]$lat <- 18.1855
RI[RI$region == "GRBK",]$long <- -64.966


#Want to add 100 m contour line
grbk_bathy2 <- terra::rast("Data/bathymetrydata/Jeremiah_grammanik/gram_2m/dblbnd.adf")
crs(grbk_bathy2) <-  "+proj=utm +zone=20 +datum=WGS84"
grbk_bathy2 <- terra::project(grbk_bathy2, "+proj=longlat +datum=NAD83")

buis_bathy2 <- terra::rast("Data/bathymetrydata/usvi_1_mhw_2014.nc")
crs(buis_bathy2) <- "+proj=longlat +datum=NAD83"

#BUIS plot
pBUIS <- ggplot() +
  tidyterra::geom_spatraster_contour(data = buis_bathy2, breaks = c(-100)) +
  geom_point(data = filter(data.frame(BUISstations), lat_nad83 < 17.82), 
             aes(x = long_nad83, y = lat_nad83)) +
  geom_polygon(data = filter(HR95_BUISgg, id != "A69-1601-45341"), 
               aes(x = long, y = lat, group = piece),
               color = "dodgerblue",
               fill = "dodgerblue",
               alpha = 0.3) +
  geom_polygon(data = filter(HR50_BUISgg,id !="A69-1601-45341"), 
               aes(x = long, y = lat, group = piece), 
               color = "dodgerblue",
               fill = "dodgerblue",
               alpha = 0.5) +
  geom_text(data = filter(RI, region == "BUIS" & id !="A69-1601-45341"),
            aes(x = long, y = lat, label = totdays_det )) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  coord_sf(xlim = c(-64.61, -64.59), ylim = c(17.800, 17.810)) +
  scale_y_continuous(breaks = seq(17.800, 17.810, 0.002)) +
  scale_x_continuous(breaks = seq(-64.609, -64.590, 0.005)) +
  facet_wrap(~id)



#GRBK plot
pGRBK <- ggplot() +
  tidyterra::geom_spatraster_contour(data = grbk_bathy2, breaks = c(100)) +
  geom_point(data = filter(data.frame(GRBKstations), lat_nad83 < 18.192), 
             aes(x = long_nad83, y = lat_nad83)) +
  geom_polygon(data = filter(HR95_GRBKgg, id != "A69-9006-5416"), 
               aes(x = long, y = lat, group = piece),
               color = "#00AFBB",
               fill = "#00AFBB",
               alpha = 0.2) +
  geom_polygon(data = filter(HR50_GRBKgg, id != "A69-9006-5416"),
               aes(x = long, y = lat, group = piece),
               color = "#00AFBB",
               fill = "#00AFBB",
               alpha = 0.5) +
  geom_text(data = filter(RI, region == "GRBK" & id !="A69-9006-5416"),
            aes(x = long, y = lat, label = totdays_det )) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  coord_sf(xlim = c(-64.968, -64.952), ylim = c(18.185, 18.192)) +
  scale_y_continuous(breaks = seq(18.185, 18.192, 0.002)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude") +
  facet_wrap(~id)

png("PrelimFigs/KUDs/BUIS_KUD_95_50.png",
    width = 3000,
    height = 2000,
    res = 300)
pBUIS
dev.off()

png("PrelimFigs/KUDs/GRBK_KUD_95_50.png",
    width = 3000,
    height = 2000,
    res = 300)
pGRBK
dev.off()




combo <- pGRBK/pBUIS
combo

png("PrelimFigs/KUDs/ALL_KUD_95_50.png",
    width = 3000,
    height = 3800,
    res = 300)
combo
dev.off()

tiff("PrelimFigs/KUDs/ALL_KUD_95_50.tiff",
    width = 3000,
    height = 3800,
    res = 350)
combo
dev.off()



#There was one fish from each region that traveled a weirdly long way
#Going to plot those separately

#IDEA:
#CALCULATE KUD FOR BEFORE AND AFTER LONG DISTANCE TREK, MAKE A FIGURE SHOWING


#GRAMMANIK
coa_df2 <- filter(coa_df, transmitter == "A69-9006-5416")

#identify where to split
ggplot(coa_df2, aes(x = detection_time_coa, y = mlat)) +
  geom_point()
coa_df2$id <- 1
coa_df2[coa_df2$lat_nad83 > 18.192,]$id <- 2

ggplot(coa_df2, aes(x = detection_time_coa, y = mlat)) +
  geom_point(aes(color = as.factor(id)))


#calculate KUDs
kud_df_2a <- dplyr::select(coa_df2, id, mlong, mlat) %>% 
  filter(id == 1) %>% 
  rename(x = mlong, y = mlat)

kud_df_2b <- dplyr::select(coa_df2, id, mlong, mlat) %>% 
  filter(id == 2) %>% 
  rename(x = mlong, y = mlat)


#convert to spatial points dataframe
coordinates(kud_df_2a) <- c("x", "y")
proj4string(kud_df_2a) <- CRS("+proj=longlat +datum=NAD83")
kud_df2_2a <- spTransform(kud_df_2a, CRS("+proj=utm +zone=20 ellps=WGS84"))

coordinates(kud_df_2b) <- c("x", "y")
proj4string(kud_df_2b) <- CRS("+proj=longlat +datum=NAD83")
kud_df2_2b <- spTransform(kud_df_2b, CRS("+proj=utm +zone=20 ellps=WGS84"))

#calculate KUD
kudg2a <- kernelUD(kud_df2_2a, grid = 1000)
image(kudg2a)


HR95_GRBKtrava <- getverticeshr(kudg2a, percent = 95, unout = "km2")

plot(HR95_GRBKtrav)

HR50_GRBKtrava <- getverticeshr(kudg2a, percent = 50, unout = "km2")

plot(HR50_GRBKtrav)


kudg2b <- kernelUD(kud_df2_2b, grid = 1000, extent = 5)
image(kudg2b)


HR95_GRBKtravb <- getverticeshr(kudg2b, percent = 95, unout = "km2")

plot(HR95_GRBKtravb)

HR50_GRBKtravb <- getverticeshr(kudg2b, percent = 50, unout = "km2")



#prep data for plotting
HR50_GRBKtrava <- spTransform(HR50_GRBKtrava, CRS("+proj=longlat"))
HR95_GRBKtrava <- spTransform(HR95_GRBKtrava, CRS("+proj=longlat"))
HR50_GRBKggtrava <- fortify(HR50_GRBKtrava)
HR95_GRBKggtrava <- fortify(HR95_GRBKtrava)


HR50_GRBKtravb <- spTransform(HR50_GRBKtravb, CRS("+proj=longlat"))
HR95_GRBKtravb <- spTransform(HR95_GRBKtravb, CRS("+proj=longlat"))
HR50_GRBKggtravb <- fortify(HR50_GRBKtravb)
HR95_GRBKggtravb <- fortify(HR95_GRBKtravb)


#pull number of days active at each spot
coa_df2 %>% group_by(id) %>% 
  summarize(ndays = n_distinct(date(detection_time_coa)))
RI_GRBKtrav <- data.frame(id = c(1,2), 
                          ndays = c(14, 193),
                          lat = c(18.192, 18.196),
                          long = c(-64.961, -64.945))

HR95_GRBKggtrava$transmitter <- "A69-9006-5416"
HR95_GRBKggtravb$transmitter <- "A69-9006-5416"

GRBKtrav <- ggplot() +
  tidyterra::geom_spatraster_contour(data = grbk_bathy2, breaks = c(100)) +
  geom_point(data = filter(data.frame(GRBKstations)), 
             aes(x = long_nad83, y = lat_nad83)) +
  geom_polygon(data = HR95_GRBKggtrava, 
               aes(x = long, y = lat, group = id, color = id, fill = id),
               alpha = 0.3) +
  geom_polygon(data = HR50_GRBKggtrava, 
               aes(x = long, y = lat, group = id, color = id, fill = id),
               alpha = 0.5) +
  geom_polygon(data = HR95_GRBKggtravb, 
               aes(x = long, y = lat, group = id, color = id, fill = id),
               alpha = 0.3) +
  geom_polygon(data = HR50_GRBKggtravb, 
               aes(x = long, y = lat, group = id, color = id, fill = id),
               alpha = 0.5) +
  geom_text(data = RI_GRBKtrav,
            aes(x = long, y = lat, label = ndays)) +
  geom_curve(aes(x = -64.957, y = 18.1905, xend = -64.949, yend = 18.194),
             arrow = arrow(length = unit(0.5, "cm")),
             curvature = -0.2) +
  coord_sf(xlim = c(-64.968, -64.94), ylim = c(18.1865, 18.198)) +
  ggplot2::annotate(geom = "text", x = -64.9535, y = 18.1945, label = "2 days") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.4) +
  scale_color_manual(values = c("#00AFBB", "cadetblue")) +
  scale_fill_manual(values = c("#00AFBB", "cadetblue")) +
  facet_wrap(~transmitter) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") 




travcombo <- (pGRBK/GRBKtrav) + pBUIS +   
  plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)"))) +
  plot_layout(heights = c(3,1,3))

png("PrelimFigs/KUDs/FULLCOMBO.png",
    width = 2800,
    height = 4200,
    res = 300)
travcombo
dev.off()

tiff("PrelimFigs/KUDs/FULLCOMBO.tiff",
    width = 2800,
    height = 4200,
    res = 340)
travcombo
dev.off()


#Create Table of Results
BUIS <- HR50_BUIS@data
BUIS$region <- "BUIS"
BUIS$KUD50 <- BUIS$area
BUIS$KUD95 <- HR95_BUIS@data$area
BUIS$Tag <- str_sub(BUIS$id, -5)
BUIS <- dplyr::select(BUIS, region, Tag,
                      KUD50, KUD95)

GRBK <- HR50_GRBK@data
GRBK$region <- "GRBK"
GRBK$KUD50 <- GRBK$area
GRBK$KUD95 <- HR95_GRBK@data$area
GRBK$Tag <- str_sub(GRBK$id, -4)
GRBK <- dplyr::select(GRBK, region, Tag,
                      KUD50, KUD95)

tabled <- rbind(BUIS, GRBK)

mytable <- tabled %>% 
  gt(  groupname_col = "region",
       rowname_col = "Tag") %>% 
  cols_label(
    region = md("Region"),
    Tag = md("Tag ID"),
    KUD50 = md("50% KUD Area (m^2)"),
    KUD95 = md("95% KUD Area (m^2)")
  ) %>% 
  cols_align(align = "center") %>% 
  tab_header(
    title = "KUD Area Summary"
  )



GRBKpre <- HR95_GRBKtrava@data
GRBKpre$KUD95 <- GRBKpre$area
GRBKpre$KUD50 <- HR50_GRBKtrava@data$area

GRBKpost <- HR95_GRBKtravb@data
GRBKpost$KUD95 <- GRBKpost$area
GRBKpost$KUD50 <- HR50_GRBKtravb@data$area


#NOTE: DECIDED THIS FISH WAS EATEN UPON CLOSER EXAMINATION
# #BUCK ISLAND
# coa_df_b2 <- filter(coa_df_b, transmitter == "A69-1601-45341")
# 
# #identify where to split
# ggplot(coa_df_b2, aes(x = detection_time_coa, y = mlat)) +
#   geom_point()
# coa_df_b2$id <- 1
# coa_df_b2[coa_df_b2$lat_nad83 > 17.82,]$id <- 2
# 
# #calculate KUDs
# kud_df_b2 <- dplyr::select(coa_df_b2, id, mlong, mlat) %>% 
#   rename(x = mlong, y = mlat)
# 
# #convert to spatial points dataframe
# coordinates(kud_df_b2) <- c("x", "y")
# proj4string(kud_df_b2) <- CRS("+proj=longlat +datum=NAD83")
# kud_df2_b2 <- spTransform(kud_df_b2, CRS("+proj=utm +zone=20 ellps=WGS84"))
# 
# #calculate KUD
# kud22 <- kernelUD(kud_df2_b2, grid = 500)
# image(kud22)
# 
# 
# HR95_BUIStrav <- getverticeshr(kud22, percent = 95, unout = "m2")
# 
# plot(HR95_BUIStrav)
# 
# HR50_BUIStrav <- getverticeshr(kud22, percent = 50, unout = "m2")
# 
# plot(HR50_BUIStrav)
# 
# #prep data for plotting
# HR50_BUIStrav <- spTransform(HR50_BUIStrav, CRS("+proj=longlat"))
# HR95_BUIStrav <- spTransform(HR95_BUIStrav, CRS("+proj=longlat"))
# HR50_BUISggtrav <- fortify(HR50_BUIStrav)
# HR95_BUISggtrav <- fortify(HR95_BUIStrav)
# 
# #pull number of days active at each spot
# coa_df_b2 %>% group_by(id) %>% 
#   summarize(ndays = n_distinct(date(detection_time_coa)))
# RI_BUIStrav <- data.frame(id = c(1,2), 
#            ndays = c(26, 2),
#            lat = c(17.80, 17.83),
#            long = c(-64.605, -64.465))
# 
# BUIStrav <- ggplot() +
#   geom_point(data = filter(data.frame(BUISstations)), 
#              aes(x = long_nad83, y = lat_nad83)) +
#   geom_polygon(data = HR95_BUISggtrav, 
#                aes(x = long, y = lat, group = id, color = id, fill = id),
#                alpha = 0.3) +
#   geom_polygon(data = HR50_BUISggtrav, 
#                aes(x = long, y = lat, group = id, color = id, fill = id),
#                alpha = 0.5) +
#   geom_text(data = RI_BUIStrav,
#             aes(x = long, y = lat, label = ndays)) +
#   geom_curve(aes(x = -64.575, y = 17.805, xend = -64.48, yend = 17.825),
#                arrow = arrow(length = unit(0.5, "cm")),
#              curvature = -0.2) +
#   ggplot2::annotate(geom = "text", x = -64.525, y = 17.817, label = "3 days") +
#   ggsn::scalebar(dist = 2.5, location = "topleft", 
#                  x.min = -64.61, x.max = -64.4475, 
#                  y.min = 17.8, y.max = 17.832,
#                  transform = T,
#                  dist_unit = "km",
#                  height = 0.02,
#                  st.dist = 0.05,
#                  st.size = 3) +
#   scale_color_manual(values = c("#FC4E07", "darkorange")) +
#   scale_fill_manual(values = c("#FC4E07", "darkorange")) +
#   theme_bw() +
#   labs(x = "Longitude", y = "Latitude", title = "Buck Island/Lang Bank") +
#   theme(legend.position = "none") 

