#Kayla Blincow
#6/7/2022


#Additional Lionfish data cleaning.

#The deep lionfish data needed some additional cleaning after the QA/QC Guidance
#script.

#Sort detections by region.
#add release location
#Remove one off detections.

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)

#load my data
d <- readRDS("DeepLionfish_clean.rds")

#remove grouping
d <- d %>% ungroup()

#add region to data
unique(d$release_site)
d[d$release_site == "BAR_1",]$release_site <- "NBAR_01"
d[d$release_site == "BAR_4",]$release_site <- "NBAR_04"
d[d$release_site == "BAR_3",]$release_site <- "NBAR_03"
d[d$release_site == "BAR_2",]$release_site <- "NBAR_02"
d[d$release_site == "VR_177",]$release_site <- "177"
d[d$release_site == "VR_181",]$release_site <- "181"

d$region <- NA
d[d$release_site == "NBAR_01",]$region <- "BUIS"
d[d$release_site == "NBAR_02",]$region <- "BUIS"
d[d$release_site == "NBAR_03",]$region <- "BUIS"
d[d$release_site == "NBAR_04",]$region <- "BUIS"
d[d$release_site == "177",]$region <- "GRBK"
d[d$release_site == "181",]$region <- "GRBK"
d[d$release_site == "BUIS_59",]$region <- "BUIS"


#add release location lat/long
release <- unique(d$release_site) %>% as.data.frame()

colnames(release) <- "release_site"

release <- left_join(release, d, by = c("release_site" = "station")) %>% 
  select(release_site, lat_nad83, long_nad83) %>% 
  distinct() %>% 
  rename(releaselat = lat_nad83,
         releaselong = long_nad83)

d <- left_join(d, release)

d <- select(d, -release_lat, -release_lon, -tag_serial.x, -est_tag_dead_date,
            -Sensor_type, -Range, -Units, -Slope, -Intercept, -animal_end_date,
            -FL, -SL, -Sex, -WT..kg., -FinClip, -species.x, -tag_model, 
            -floy_tag, -Tag.Power..dB., -Tag_Delay,
            -Min_Delay, -Max_Delay, -EstTagLife_days)

#remove detections that occur at stations outside of region 
#45341: I think these are real...
filter(d, transmitter == "A69-1601-45269") %>% distinct(station)

ggplot(filter(d, transmitter == "A69-1601-45269"), 
       aes(x = detection_time_ast, y = lat_nad83)) +
  geom_line()

#remove that row
d <- d[!(d$transmitter == "A69-1601-45269" & d$station == "161"),]

#check time range of each tag
d %>% group_by(transmitter) %>% 
  summarize(timerange = (range(detection_time_ast)[2] - range(detection_time_ast)[1])) %>% 
  as.data.frame()
#looks like 45340 was only detected for part of a day after initial cleaning

ggplot(filter(d, transmitter == "A69-1601-45340")) +
  geom_point(aes(x = detection_time_ast, y = station))

#going to remove it because it was only detected for 12 hours..

d <- filter(d, transmitter != "A69-1601-45340")


#noticed some weirdnesses in the depth tags as well... 
#just pull depth-coded tags
depth <- d %>% filter(!is.na(sensor_value))

#plot depth through time for each tag
ggplot(data = depth, aes(x = detection_time_ast, y = sensor_value, 
                  color = station)) +
  geom_point() +
  scale_y_reverse() +
  labs(x = "Date", y = "Depth (m)") +
  facet_wrap(~transmitter) 

#5410: down to 150 then hops up to the surface for a loooong time
#5412: hops up to the surface then goes down to 150m then back up to the surface
#1231: weird detection(s) down at 125m
#5411: Not sure?

#look at the number of detections at 125 for 1231
depth %>% filter(transmitter == "A69-9006-1231", sensor_value > 100) %>% as.data.frame()
depth %>% filter(transmitter == "A69-9006-1231") %>% summarize(max(detection_time_ast))

#single detection down at 128 meters, going to remove that
d1231 <- d %>% filter(transmitter == "A69-9006-1231", sensor_value < 100)

d <- d[!(d$transmitter=="A69-9006-1231" & d$sensor_value == 127.9717),]


#Rick and Shaun say that much of the receivers are on the shelf edge and it's
#very possible for the receivers to detect fishies deeper than the receiver depth

#Deciding 5410 and 5412 have messed up sensors and not including them in the 
#depth analyses..
ggplot(data = filter(depth, transmitter == "A69-9006-5412"), 
       aes(x = detection_time_ast, y = station)) +
  geom_point()

ggplot(data = filter(depth, transmitter == "A69-9006-5412"), 
       aes(x = detection_time_ast, y = station)) +
  geom_point()

ggplot(data = filter(depth, transmitter == "A69-9006-5410"), 
       aes(x = detection_time_ast, y = station)) +
  geom_point() #this might be a dropped tag... going to remove this tag also

d <- filter(d, transmitter != "A69-9006-5410")


#this makes me feel like I should look more closely at each tag...

for(i in 1:length(unique(d$transmitter))){
  p <- ggplot(data = filter(d, transmitter == unique(d$transmitter)[i]), 
         aes(x = detection_time_ast, y = station)) +
    geom_point() +
    labs(title = unique(d$transmitter)[i])
  print(p)
}


stations <- d %>% select(station, lat_nad83, long_nad83, region) %>% 
  distinct()

ggplot(filter(stations, region == "BUIS"), 
       aes(x = long_nad83, y = lat_nad83, color = station)) +
  geom_point() 
ggplot(filter(stations, region == "GRBK"), 
       aes(x = long_nad83, y = lat_nad83, label = station)) +
  geom_text(hjust = 1, vjust = 1) +
  geom_point() 

#Not sure?: 1231, 1230, 45132, 45115

#5416 looks like it moved off from main array out to 53/100
#5411 looks like it move off from main array out to 158/149 (decided it was eaten)
#45341 transited up to other BUIS receivers (decided it was eaten)

#remove stations 9a and 9A as they are redundant with 9 (and have fewer detections)
d <- filter(d, station != "9a" & station != "9A")

#based on RateMovementPredationCheck.R, I have decided 5411 was predated upon
#it moves way too far, way too quickly to be a lionfish
d <- filter(d, transmitter != "A69-9006-5411" & transmitter != "A69-1601-45341")


#write the new clean data file.
write_rds(d, "DeepLionfish_clean2.rds")
