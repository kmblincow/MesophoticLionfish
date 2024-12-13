#Kayla Blincow
#1/19/2023

#The purpose of this script is to generate a data file that has all the covariates
#for the lionfish analysis.

#Covariates:
#light levels
#predator abundance
#lunar cycle
#currents (NEED)
#temperature (NEED)
#DNC


#UPDATE (4/3/2023)
#fine tuning predator presence/absence based on literature provided by Rick


#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(lunar)

#load my data files
d <- readRDS("DeepLionfish_clean2.rds")
PAR <- readRDS("Data/PAR_light/PARlionfish.rds")

# #STJ BUOY temperature
# temp2018 <- read.table("Data/STJBuoyOceanographicData/STJTemp2018.txt.gz")
# temp2019 <- read.table("Data/STJBuoyOceanographicData/STJTemp2019.txt.gz")
# temp2020 <- read.table("Data/STJBuoyOceanographicData/STJTemp2020.txt.gz")
# 
# temp <- rbind(temp2018, temp2019, temp2020)[,1:7]
# colnames(temp) <- c("year", "month", "day", "hour", "minute", "depth", "temp")
# temp$date <- paste(temp$year, temp$month, temp$day, sep = "-") %>% as.Date()
# temp$datehour <- ymd_h(paste(temp$date, temp$hour, sep = " "))
# temp <- temp[,7:9]

#compiling temperature data from Vanessa (Ginsburg Fringe/BUIS TCRMP stations)
BUIStemp1 <- read.csv("Data/Temperature/ActualData/BUIS_BT_TCBX33_1704_1811.csv",
                      col.names = c("Number", "DateTimeUTC", "Temperature"))
BUIStemp2 <- read.csv("Data/Temperature/ActualData/BUIS_BT_TCBX33_1811_1910.csv",
                      col.names = c("Number", "DateTimeUTC", "Temperature"))
BUIStemp <- rbind(BUIStemp1, BUIStemp2)
BUIStemp$region <- "BUIS"


GRBKtemp1 <- read.csv("Data/Temperature/ActualData/GRBK_BT_TCGB63_1812_2001.csv",
                      col.names = c("Number", "DateTimeUTC", "Temperature"))
GRBKtemp2 <- read.csv("Data/Temperature/ActualData/GRBK_BT_TCGB63_2001_2104.csv",
                      col.names = c("Number", "DateTimeUTC", "Temperature"))
GRBKtemp <- rbind(GRBKtemp1, GRBKtemp2)
GRBKtemp$region <- "GRBK"

temp <- rbind(BUIStemp, GRBKtemp)
temp <- temp[complete.cases(temp),]
temp$DateTimeUTC <- mdy_hm(temp$DateTimeUTC, tz = "UTC")
temp$DateTimeAST <- with_tz(temp$DateTimeUTC, 
                            tzone = "America/Manaus")

temp$datehour <- ymd_h(paste(date(temp$DateTimeAST), 
                             hour(temp$DateTimeAST), 
                             sep = " "))
temp$hour <- hour(temp$DateTimeAST)
temp$date <- date(temp$DateTimeAST)

temp$lat_nad83 <- NA
temp$long_nad83 <- NA
temp[temp$region == "BUIS",]$lat_nad83 <- mean(d[d$region == "BUIS",]$lat_nad83)
temp[temp$region == "BUIS",]$long_nad83 <- mean(d[d$region == "BUIS",]$long_nad83)

temp[temp$region == "GRBK",]$lat_nad83 <- mean(d[d$region == "GRBK",]$lat_nad83)
temp[temp$region == "GRBK",]$long_nad83 <- mean(d[d$region == "GRBK",]$long_nad83)



sun <- temp %>% dplyr::select(date, lat_nad83, long_nad83) %>% 
  rename(lat = lat_nad83, lon = long_nad83)
sun <- getSunlightTimes(data = sun, keep = c("sunrise", "sunset", "night", "nightEnd"), tz = "America/Manaus")

temp <- left_join(temp, sun)

temp$dnc <- "Night"

temp[temp$DateTimeAST >= temp$sunrise & temp$DateTimeAST <= temp$sunset,]$dnc <- "Day"
temp[temp$DateTimeAST >= temp$sunset & temp$DateTimeAST <= temp$night,]$dnc <- "Crep"
temp[temp$DateTimeAST >= temp$nightEnd & temp$DateTimeAST <= temp$sunrise,]$dnc <- "Crep"

temp <- temp %>% group_by(date, dnc, region) %>% 
  summarize(DielTemp = mean(Temperature)) 



ggplot(temp) +
  geom_line(aes(x = date, y = DielTemp, color = dnc),
            alpha = 0.5) +
  facet_wrap(~dnc)

ggplot(temp) +
  geom_boxplot(aes(x = dnc, y = DielTemp)) +
  geom_point(aes(x = dnc, y = DielTemp, color = dnc))

#currents (skipping this for now, because I don't think the STJ buoy is representative)
# curr2018 <- read.table("Data/STJBuoyOceanographicData/STJCurrents2018.txt.gz")
# curr2019 <- read.table("Data/STJBuoyOceanographicData/STJCurrents2019.txt.gz")
# curr2020 <- read.table("Data/STJBuoyOceanographicData/STJCurrents2020.txt")

#Data Generation for other Variables
dates <- seq(ymd("2018-08-02"), ymd("2020-06-28"), by = "day") %>% 
  data.frame(date = .)

#generate lunar data
dates$lunar <- lunar.phase(dates[,1], name = 8) 
dates$lunar2 <- lunar.phase(dates[,1], name = 4)
dates$lunarnum <- lunar.phase(dates[,1])

#generate predator data
#this is just by month let's build a dataset based on when we think peak numbers
#of predators are present




#Full Moons
# 2018: 1/1/2018; 1/31/2018; 3/1/2018; 3/31/2018; 4/29/2018; 5/29/2018; 6/28/2018; 7/27/2018; 8/26/2018; 9/24/2018; 10/24/2018; 11/23/2018; 12/22/2018
# 2019: 1/21/2019; 2/19/2019; 3/20/2019; 4/19/2019; 5/18/2019; 6/17/2019; 7/16/2019; 8/15/2019; 9/14/2019; 10/13/2019; 11/12/2019; 12/12/2019
# 2020: 1/10/2020; 2/9/2020; 3/9/2020; 4/7/2020; 5/7/2020; 6/5/2020; 8/3/2020; 9/3/2020; 10/1/2020; 10/31/2020; 11/30/2020; 12/29/2020



#nassau grouper
NG <- c(seq(ymd("2019-02-19") - 14, ymd("2019-02-18"), by = "day"),
        seq(ymd("2019-02-19"), ymd("2019-02-19") + 9, by = "day"),
        seq(ymd("2019-03-20") - 14, ymd("2019-03-19"), by = "day"),
        seq(ymd("2019-03-20"), ymd("2019-03-20") + 9, by = "day"),
        seq(ymd("2019-04-19") - 14, ymd("2019-04-18"), by = "day"),
        seq(ymd("2019-04-19"), ymd("2019-04-19") + 9, by = "day"),
        seq(ymd("2020-02-09") - 14, ymd("2020-02-08"), by = "day"),
        seq(ymd("2020-02-09"), ymd("2020-02-09") + 9, by = "day"),
        seq(ymd("2020-03-09") - 14, ymd("2020-03-08"), by = "day"),
        seq(ymd("2020-03-09"), ymd("2020-03-09") + 9, by = "day"),
        seq(ymd("2020-04-07") - 14, ymd("2020-04-06"), by = "day"),
        seq(ymd("2020-04-07"), ymd("2020-04-07") + 9, by = "day")
) %>% 
  data.frame(date = ., 
             NGspawn = c(
               rep("N", length(seq(ymd("2019-02-19") - 14, ymd("2019-02-18"), by = "day"))),
               rep("Y", length(seq(ymd("2019-02-19"), ymd("2019-02-19") + 9, by = "day"))),
               rep("N", length(seq(ymd("2019-03-20") - 14, ymd("2019-03-19"), by = "day"))),
               rep("Y", length(seq(ymd("2019-03-20"), ymd("2019-03-20") + 9, by = "day"))),
               rep("N", length(seq(ymd("2019-04-19") - 14, ymd("2019-04-18"), by = "day"))),
               rep("Y", length(seq(ymd("2019-04-19"), ymd("2019-04-19") + 9, by = "day"))),
               rep("N", length(seq(ymd("2020-02-09") - 14, ymd("2020-02-08"), by = "day"))),
               rep("Y", length(seq(ymd("2020-02-09"), ymd("2020-02-09") + 9, by = "day"))),
               rep("N", length(seq(ymd("2020-03-09") - 14, ymd("2020-03-08"), by = "day"))),
               rep("Y", length(seq(ymd("2020-03-09"), ymd("2020-03-09") + 9, by = "day"))),
               rep("N", length(seq(ymd("2020-04-07") - 14, ymd("2020-04-06"), by = "day"))),
               rep("Y", length(seq(ymd("2020-04-07"), ymd("2020-04-07") + 9, by = "day")))
               )
  )

#REMOVING DOG SNAPPER FROM THE ANALYSIS
# #dog snapper
# DS <- c(seq(ymd("2019-02-18") - 14, ymd("2019-02-17"), by = "day"),
#         seq(ymd("2019-02-18"), ymd("2019-02-27"), by = "day"),
#         seq(ymd("2020-02-08") - 14, ymd("2020-02-07"), by = "day"),
#         seq(ymd("2020-02-08"), ymd("2020-02-17"), by = "day")
# ) %>% 
#   data.frame(date = ., 
#              DSspawn = c(
#                rep("N", length(seq(ymd("2019-02-18") - 14, ymd("2019-02-17"), by = "day"))),
#                rep("Y", length(seq(ymd("2019-02-18"), ymd("2019-02-27"), by = "day"))),
#                rep("N", length(seq(ymd("2020-02-08") - 14, ymd("2020-02-07"), by = "day"))),
#                rep("Y", length(seq(ymd("2020-02-08"), ymd("2020-02-17"), by = "day")))
#                )
#   )

#cubera snapper (June to October)
#8/26/2018; 9/24/2018; 10/24/2018;
CS <- c(seq(ymd("2018-08-26") - 21, ymd("2018-08-26") - 7, by = "day"),
        seq(ymd("2018-08-26") - 6, ymd("2018-08-26") + 8, by = "day"),
        seq(ymd("2018-09-24") - 21, ymd("2018-09-24") - 7, by = "day"),
        seq(ymd("2018-09-24") - 6, ymd("2018-09-24") + 8, by = "day"),
        seq(ymd("2018-10-24") - 21, ymd("2018-10-24") - 7, by = "day"),
        seq(ymd("2018-10-24") - 6, ymd("2018-10-24") + 8, by = "day"),
        
        seq(ymd("2019-06-17") - 21, ymd("2019-06-17") - 7, by = "day"),
        seq(ymd("2019-06-17") - 6, ymd("2019-06-17") + 8, by = "day"),
        seq(ymd("2019-07-16") - 21, ymd("2019-07-16") - 7, by = "day"),
        seq(ymd("2019-07-16") - 6, ymd("2019-07-16") + 8, by = "day"),
        seq(ymd("2019-08-15") - 21, ymd("2019-08-15") - 7, by = "day"),
        seq(ymd("2019-08-15") - 6, ymd("2019-08-15") + 8, by = "day"),
        seq(ymd("2019-09-14") - 21, ymd("2019-09-14") - 7, by = "day"),
        seq(ymd("2019-09-14") - 6, ymd("2019-09-14") + 8, by = "day"),
        seq(ymd("2019-10-13") - 21, ymd("2019-10-13") - 7, by = "day"),
        seq(ymd("2019-10-13") - 6, ymd("2019-10-13") + 8, by = "day"),
        
        seq(ymd("2020-06-05") - 21, ymd("2020-06-05") - 7, by = "day"),
        seq(ymd("2020-06-05") - 6, ymd("2020-06-05") + 8, by = "day")
) %>% 
  data.frame(date = ., 
             CSspawn = c(
               rep("N", length(seq(ymd("2018-08-26") - 21, ymd("2018-08-26") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2018-08-26") - 6, ymd("2018-08-26") + 8, by = "day"))),
               rep("N", length(seq(ymd("2018-09-24") - 21, ymd("2018-09-24") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2018-09-24") - 6, ymd("2018-09-24") + 8, by = "day"))),
               rep("N", length(seq(ymd("2018-10-24") - 21, ymd("2018-10-24") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2018-10-24") - 6, ymd("2018-10-24") + 8, by = "day"))),
               rep("N", length(seq(ymd("2019-06-17") - 21, ymd("2019-06-17") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2019-06-17") - 6, ymd("2019-06-17") + 8, by = "day"))),
               rep("N", length(seq(ymd("2019-07-16") - 21, ymd("2019-07-16") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2019-07-16") - 6, ymd("2019-07-16") + 8, by = "day"))),
               rep("N", length(seq(ymd("2019-08-15") - 21, ymd("2019-08-15") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2019-08-15") - 6, ymd("2019-08-15") + 8, by = "day"))),
               rep("N", length(seq(ymd("2019-09-14") - 21, ymd("2019-09-14") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2019-09-14") - 6, ymd("2019-09-14") + 8, by = "day"))),
               rep("N", length(seq(ymd("2019-10-13") - 21, ymd("2019-10-13") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2019-10-13") - 6, ymd("2019-10-13") + 8, by = "day"))),
               rep("N", length(seq(ymd("2020-06-05") - 21, ymd("2020-06-05") - 7, by = "day"))),
               rep("Y", length(seq(ymd("2020-06-05") - 6, ymd("2020-06-05") + 8, by = "day")))
               
               )
  )

  
  

#yellowfin grouper
YG <- c(seq(ymd("2019-02-19") - 17, ymd("2019-02-19") - 3, by = "day"),
        seq(ymd("2019-02-19") - 2, ymd("2019-02-19") + 10, by = "day"),
        seq(ymd("2019-03-20") - 17, ymd("2019-03-20") - 3, by = "day"),
        seq(ymd("2019-03-20") - 2, ymd("2019-03-20") + 10, by = "day"),
        seq(ymd("2019-04-19") - 17, ymd("2019-04-19") - 3, by = "day"),
        seq(ymd("2019-04-19") - 2, ymd("2019-04-19") + 10, by = "day"),
        seq(ymd("2020-02-09") - 17, ymd("2020-02-09") - 3, by = "day"),
        seq(ymd("2020-02-09") - 2, ymd("2020-02-09") + 10, by = "day"),
        seq(ymd("2020-03-09") - 17, ymd("2020-03-09") - 3, by = "day"),
        seq(ymd("2020-03-09") - 2, ymd("2020-03-09") + 10, by = "day"),
        seq(ymd("2020-04-07") - 17, ymd("2020-04-07") - 3, by = "day"),
        seq(ymd("2020-04-07") - 2, ymd("2020-04-07") + 10, by = "day")
) %>% 
  data.frame(date = ., 
             YGspawn = c(
               rep("N", length(seq(ymd("2019-02-19") - 17, ymd("2019-02-19") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2019-02-19") - 2, ymd("2019-02-19") + 10, by = "day"))),
               rep("N", length(seq(ymd("2019-03-20") - 17, ymd("2019-03-20") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2019-03-20") - 2, ymd("2019-03-20") + 10, by = "day"))),
               rep("N", length(seq(ymd("2019-04-19") - 17, ymd("2019-04-19") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2019-04-19") - 2, ymd("2019-04-19") + 10, by = "day"))),
               rep("N", length(seq(ymd("2020-02-09") - 17, ymd("2020-02-09") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2020-02-09") - 2, ymd("2020-02-09") + 10, by = "day"))),
               rep("N", length(seq(ymd("2020-03-09") - 17, ymd("2020-03-09") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2020-03-09") - 2, ymd("2020-03-09") + 10, by = "day"))),
               rep("N", length(seq(ymd("2020-04-07") - 17, ymd("2020-04-07") - 3, by = "day"))),
               rep("Y", length(seq(ymd("2020-04-07") - 2, ymd("2020-04-07") + 10, by = "day")))
             )
  )


# 2019: 1/21/2019; 2/19/2019; 3/20/2019; 4/19/2019; 5/18/2019; 6/17/2019; 7/16/2019; 8/15/2019; 9/14/2019; 10/13/2019; 11/12/2019; 12/12/2019
# 2020: 1/10/2020; 2/9/2020; 3/9/2020; 4/7/2020; 5/7/2020; 6/5/2020; 8/3/2020; 9/3/2020; 10/1/2020; 10/31/2020; 11/30/2020; 12/29/2020


#tiger grouper
TG <- c(seq(ymd("2019-03-20") - 8, ymd("2019-03-20") + 5, by = "day"),
        seq(ymd("2019-03-20") + 6, ymd("2019-03-20") + 12, by = "day"),
        seq(ymd("2019-04-19") - 8, ymd("2019-04-19") + 5, by = "day"),
        seq(ymd("2019-04-19") + 6, ymd("2019-04-19") + 12, by = "day"),
        seq(ymd("2019-05-18") - 8, ymd("2019-05-18") + 5, by = "day"),
        seq(ymd("2019-05-18") + 6, ymd("2019-05-18") + 12, by = "day"),
        seq(ymd("2019-06-17") - 8, ymd("2019-06-17") + 5, by = "day"),
        seq(ymd("2019-06-17") + 6, ymd("2019-06-17") + 12, by = "day"),
        
        seq(ymd("2020-03-09") - 8, ymd("2020-03-09") + 5, by = "day"),
        seq(ymd("2020-03-09") + 6, ymd("2020-03-09") + 12, by = "day"),
        seq(ymd("2020-04-07") - 8, ymd("2020-04-07") + 5, by = "day"),
        seq(ymd("2020-04-07") + 6, ymd("2020-04-07") + 12, by = "day"),
        seq(ymd("2020-05-07") - 8, ymd("2020-05-07") + 5, by = "day"),
        seq(ymd("2020-05-07") + 6, ymd("2020-05-07") + 12, by = "day"),
        seq(ymd("2020-06-05") - 8, ymd("2020-06-05") + 5, by = "day"),
        seq(ymd("2020-06-05") + 6, ymd("2020-06-05") + 12, by = "day")
) %>% 
  data.frame(date = ., 
             TGspawn = c(
               rep("N", length(seq(ymd("2019-03-20") - 8, ymd("2019-03-20") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2019-03-20") + 6, ymd("2019-03-20") + 12, by = "day"))),
               rep("N", length(seq(ymd("2019-04-19") - 8, ymd("2019-04-19") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2019-04-19") + 6, ymd("2019-04-19") + 12, by = "day"))),
               rep("N", length(seq(ymd("2019-05-18") - 8, ymd("2019-05-18") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2019-05-18") + 6, ymd("2019-05-18") + 12, by = "day"))),
               rep("N", length(seq(ymd("2019-06-17") - 8, ymd("2019-06-17") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2019-06-17") + 6, ymd("2019-06-17") + 12, by = "day"))),
               
               rep("N", length(seq(ymd("2020-03-09") - 8, ymd("2020-03-09") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2020-03-09") + 6, ymd("2020-03-09") + 12, by = "day"))),
               rep("N", length(seq(ymd("2020-04-07") - 8, ymd("2020-04-07") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2020-04-07") + 6, ymd("2020-04-07") + 12, by = "day"))),
               rep("N", length(seq(ymd("2020-05-07") - 8, ymd("2020-05-07") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2020-05-07") + 6, ymd("2020-05-07") + 12, by = "day"))),
               rep("N", length(seq(ymd("2020-06-05") - 8, ymd("2020-06-05") + 5, by = "day"))),
               rep("Y", length(seq(ymd("2020-06-05") + 6, ymd("2020-06-05") + 12, by = "day")))
             )
  )

#combine everything
predator <- left_join(dates, NG) %>% 
  left_join(CS) %>% 
  left_join(YG) %>% 
  left_join(TG)


#I feel like many of the predator spawning seasons are confounded...
#visualize that
predator2 <- pivot_longer(predator, cols = 5:8, 
                          names_to = "Predator")

ggplot(predator2, aes(x = date, y = Predator, color = value)) +
         geom_point()
#dog snapper, tiger grouper, yellowfin grouper, nassau grouper all on the same
#schedule-ish...

# #create one column that combines them
# predator$allspawn <- NA
# predator[(!is.na(predator$NGspawn) & predator$NGspawn == "Y") | 
#            (!is.na(predator$DSspawn) & predator$DSspawn == "Y") |
#            (!is.na(predator$TGspawn) & predator$TGspawn == "Y" ) | 
#            (!is.na(predator$YGspawn) & predator$YGspawn == "Y"),]$allspawn <- "Y"
# predator[(!is.na(predator$NGspawn) & predator$NGspawn == "N") | 
#            (!is.na(predator$DSspawn) & predator$DSspawn == "N") |
#            (!is.na(predator$TGspawn) & predator$TGspawn == "N" ) | 
#            (!is.na(predator$YGspawn) & predator$YGspawn == "N"),]$allspawn <- "N"


#combine everybody
tz(d$detection_time_ast) <- "America/Manaus"
d$date <- date(d$detection_time_ast)
d$datehour <- ymd_h(paste(d$date, hour(d$detection_time_ast)))
d$hour <- hour(d$detection_time_ast)


sun2 <- d %>% dplyr::select(date, lat_nad83, long_nad83) %>% 
  rename(lat = lat_nad83, lon = long_nad83)
sun2 <- getSunlightTimes(data = sun2, keep = c("sunrise", "sunset", "night", "nightEnd"), tz = "America/Manaus")

sun2 <- dplyr::select(sun2, -date, -lat, -lon)
d <- cbind(d, sun2)

d$dnc <- "Night"

d[d$detection_time_ast >= d$sunrise & d$detection_time_ast <= d$sunset,]$dnc <- "Day"
d[d$detection_time_ast >= d$sunset & d$detection_time_ast <= d$night,]$dnc <- "Crep"
d[d$detection_time_ast >= d$nightEnd & d$detection_time_ast <= d$sunrise,]$dnc <- "Crep"


dcombo <- left_join(d, predator) %>% left_join(PAR) %>% left_join(temp)


ggplot(dcombo, aes(x = hour)) +
  geom_bar(aes(fill = dnc), position = "stack")

#export the data file
write_rds(dcombo, "LionfishClean_Covariates.rds")
