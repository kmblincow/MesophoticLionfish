#Kayla Blincow
#7/23/2024

#NOTE:
#tried it with depth and distance traveled. I'm inclined to trust depth more, 
#because even if you aren't traveling a long distance, moving up and down in the 
#same area could be indicative of hunting activity in a limited home range, and
#the distance metrics are inherently flawed because of the acoustic array setup.


#Hertel et al. Methods?

#The purpose of this script is to try to replicate Hertel et al. diel movement
#analysis on the lionfish data

#Step 1: For each movement metric, need to establish a cut-off for active v. passive
#Movement Metrics: depth change, distance traveled
#Step 2: Calssify each location observation (COA) as active or passive
#Step 3: use densityPlot to fit kernel density curves
#Step 4: Extract time of minimum activity, maximum activity, regularity (diff b
#between high and low activity), daytime AUC, nightime AUC, crep AUC
#Step 5: Do a PCA and see if individuals cluster based on activity timing
#https://rpubs.com/Bury/ClusteringOnPcaResults

#GOING TO TRY WITH ONE ACTIVITY METRIC FIRST: depth change

####SET UP####
#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(overlap)
library(DescTools)
library(vegan)
library(factoextra) #pca viz
library(paran) #pca diagnostics
library(cluster)
library(clustertend) #clustering check
library(ggtrace)
library(geosphere)
library(patchwork)


#load data
d <- readRDS("LionfishClean_Covariates.rds")


#remove tags with issues
dpth <- d %>% filter(!is.na(sensor_value) 
                     & transmitter != "A69-9006-5412" &
                       transmitter != "A69-9006-5411" )

#Look at depth metrics (overall)
dpth %>% group_by(transmitter) %>% 
  summarize(maxdpth = max(sensor_value),
            mindpth = min(sensor_value),
            avgdpth = mean(sensor_value),
            sddpth = sd(sensor_value))
#Look at depth metrics (by diel period)
dpth %>% group_by(transmitter, dnc) %>% 
  summarize(maxdpth = max(sensor_value),
            mindpth = min(sensor_value),
            avgdpth = mean(sensor_value),
            sddpth = sd(sensor_value)) %>% 
  as.data.frame()

#calculating 5 min coas
dpth$detection_time_coa <- floor_date(dpth$detection_time_ast, unit = "5 minutes")

coa_df <- dpth %>% group_by(transmitter, detection_time_coa) %>% 
  summarize(mlong = mean(long_nad83),
         mlat = mean(lat_nad83),
         nrec = length(unique(station)),
         mndpth = mean(sensor_value)) %>% 
  data.frame()

#compare
coa_df %>% group_by(transmitter) %>% 
  summarize(maxdpth = max(mndpth),
            mindpth = min(mndpth),
            avgdpth = mean(mndpth),
            sddpth = sd(mndpth)) %>% 
  as.data.frame()


#do a sensitivity analysis on depth cut offs for activity
#NOTE: tags have pressure sensor accuracy of +/-1m
#low end: change of depth > 2m
dpths <- coa_df %>% group_by(transmitter) %>% 
  mutate(dpthchange = abs(mndpth - lag(mndpth, default = mndpth[1])),
         timechange = as.numeric(detection_time_coa - lag(detection_time_coa))) %>% filter(timechange == 5)

ggplot(dpths) + geom_histogram(aes(dpthchange)) +
  geom_vline(aes(xintercept = 2)) +
  geom_vline(aes(xintercept = 1)) +
  facet_wrap(~transmitter)
dpths %>%  group_by(transmitter) %>% 
  summarize(maxdpthchange = max(dpthchange),
            mindpthchange = min(dpthchange),
            avgdpthchange = mean(dpthchange),
            sddpthchange = sd(dpthchange),
            mediandpthchange = median(dpthchange)) %>% 
  as.data.frame()



ggplot(dpths) + geom_line(aes(x = detection_time_coa, y = dpthchange)) +
  facet_wrap(~transmitter)

dpths$activity <- 0
dpths[dpths$dpthchange > 2,]$activity <- 1

dpths %>% group_by(transmitter) %>% 
  summarize(propactive = sum(activity)/n())

#calculate the activity distribution
dpths$time01 <- (hour(dpths$detection_time_coa) * 3600 + minute(dpths$detection_time_coa) * 60 + second(dpths$detection_time_coa))/86400
dpths$timerad <- dpths$time01*2*pi

densities <- dpths %>% filter(activity == 1) %>% 
  group_by(transmitter) %>% 
  reframe(densityPlot(timerad, xcenter = "noon", rug = T, add = F, 
                      main = first(transmitter), extend = NULL))

#Extract information from density curves
# minimum activity, maximum activity, regularity (diff between high and low 
#activity), daytime AUC, nightime AUC, proportion of daytime ind. more likely 
#to be active, proportion of nighttime ind. more likely to be active

summarymetrics <- densities %>% group_by(transmitter) %>%
  summarize(minAct = min(y),
            minActT = x[which.min(y)],
            maxAct = max(y),
            maxActT = x[which.max(y)],
            reg = maxAct - minAct)

#find how to classify hours based on sunlight
hourgroups <- d %>% group_by(dnc) %>% 
  reframe(hours = hour(detection_time_ast)) %>% 
  distinct() %>% 
  arrange(dnc, hours) %>% 
  mutate(hoursRad = (hours/24)*2*pi)

#crepuscular hours were any hour interval that was classified at CREP, Day/Night
#hours were hours that had no overlap with crepuscular at any point


#calculate area under the density curve associated with different diel periods
dielAUC <- data.frame(transmitter = unique(densities$transmitter),
                     dayAUC = NA,
                     nightAUC1 = NA,
                     nightAUC2 = NA,
                     crepAUC1 = NA,
                     crepAUC2 = NA)

for(i in 1:length(unique(densities$transmitter))){
  tag <- filter(densities, transmitter == unique(densities$transmitter)[i])
  custom_density_function <- function(x) {
    approx(tag$x, tag$y, x)$y
  }
  dielAUC$dayAUC[i] <- integrate(custom_density_function,
            lower = 7, upper = 17)$value
  dielAUC$nightAUC1[i] <- integrate(custom_density_function,
                                 lower = 0, upper = 4)$value
  dielAUC$nightAUC2[i] <- integrate(custom_density_function,
                                    lower = 21, upper = 24)$value
  dielAUC$crepAUC1[i] <- integrate(custom_density_function,
                                    lower = 4, upper = 7)$value
  dielAUC$crepAUC2[i] <- integrate(custom_density_function,
                                    lower = 17, upper = 21)$value
}

#check that worked
dielAUC %>% group_by(transmitter) %>% 
  summarize(rowsum = sum(dayAUC, nightAUC1, nightAUC2, crepAUC1, crepAUC2)) #yep!

#
summarymetrics$dayAUC <- dielAUC$dayAUC
summarymetrics$nightAUC <- dielAUC$nightAUC1 + dielAUC$nightAUC2
summarymetrics$crepAUC <- dielAUC$crepAUC1 + dielAUC$crepAUC2

####DO THE PCA/CLUSTERING####

#do a PCA (not sure min/max activity time makes any sense, not using those...)
mypca <- prcomp(summarymetrics[,c(6:9)], scale. = TRUE)
summary(mypca)

#check the results
fviz_eig(mypca) #might want 3 dimensions?
fviz_pca_var(mypca) 

#check dimenionsality using parallel analysis with scree plot in Paran package
paran(summarymetrics[,c(6:9)], iterations=10000, quietly=FALSE,
      status=FALSE, all=TRUE, cfa=FALSE, graph=TRUE,
      color=TRUE, col=c("black","red","blue"),
      lty=c(1,2,3), lwd=1, legend=TRUE, file="",
      width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)
#According to Paran package description, below graph results can be evaluated in two ways:
#We retain those factors which have adjusted eigenvalue higher than 1.
#We retain those factors which real PCA eigenvalues are higher than randomly generated ones.

#2 principal components...

#look at each component..
var <- get_pca_var(mypca)

PC1 <- fviz_contrib(mypca, "var", axes=1, xtickslab.rt=90) # default angle=45°
plot(PC1, main = "Variables percentage contribution PC1")

PC2 <- fviz_contrib(mypca, "var", axes=2, xtickslab.rt=90) # default angle=45°
plot(PC2, main = "Variables percentage contribution PC2")


#look for clustering
fviz_nbclust(mypca$x, FUNcluster=kmeans, k.max = 4) 
fviz_nbclust(mypca$x, FUNcluster=kmeans, method="gap_stat", k.max = 4)+ theme_classic()
#3 clusters

km1 <- eclust(mypca$x, "kmeans", hc_metric = "euclidean", k = 3)
km2 <- eclust(mypca$x, "kmeans", hc_metric = "manhattan", k = 3)

fviz_silhouette(km1)
fviz_silhouette(km2)
#the distance metrics are the same, we can go with euclidean

km1
#add cluster group to pca for plotting
summarymetrics$cluster <- km1$cluster

ellipseplot <- fviz_ellipses(mypca, habillage = km1$cluster) 
#not sure why plot below shows different results..
#mapping: x = ~x, y = ~y, colour = ~Groups, fill = ~Groups 
#geom_polygon: na.rm = FALSE
#stat_conf_ellipse: na.rm = FALSE, level = 0.95, npoint = 100, bary = TRUE
#position_identity 

biplot <- fviz_pca_biplot(mypca,
                          habillage = km1$cluster, # color by groups
                          addEllipses = TRUE,
                          col.var = "black") +
  theme(legend.position = "none")
biplot

####DO MY OWN PLOTTING####
#make this on my own so I can customize it
library(ggbiplot)
summarymetrics$cluster2 <- "Group 1"
summarymetrics[summarymetrics$cluster > 1,]$cluster2 <- "Group 2"
summarymetrics[summarymetrics$cluster > 2,]$cluster2 <- "Group 3"


pal_shape <- gsub("^.*?(.)$", "\\1", summarymetrics$cluster2)
pal_color <- pal_shape
pal_shape <- scales::shape_pal()(3)[as.integer(pal_shape)]
names(pal_shape) <- summarymetrics$cluster2


#pal_color <- setNames(scales::hue_pal()(5), sort(unique(results$species)))[pal_color]
pal_color <- c("#1E88E5", "#D81B60", "#3C0C8A")[as.integer(pal_color)]
names(pal_color) <- summarymetrics$cluster2



pbiplot <- ggbiplot(mypca, varname.size = 3.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(data = ~ dplyr::bind_cols(.x, cluster2 = summarymetrics$cluster2), 
             aes(shape = cluster2, color = cluster2), 
             size = 3.5) +
  scale_color_manual(values = pal_color) +
  scale_shape_manual(values = pal_shape) +
  theme_bw() +
  labs(x = "PC1 (63.3%)", y = "PC2 (32.3%)", color = "", shape = "") +
  scale_x_continuous(limits = c(-3,3)) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme(legend.position = c(.8, .9),
        #legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_blank())

 
#density plots 
densityplotdata <- left_join(densities, summarymetrics)

densityplotdata$transmitter <- factor(densityplotdata$transmitter, 
                                      levels = c("A69-9006-1228",
                                                "A69-9006-1229",
                                                "A69-9006-1230",
                                                "A69-9006-5415",
                                                "A69-9006-1231",
                                                "A69-9006-1232",
                                                "A69-9006-5414",
                                                "A69-9006-1227",
                                                "A69-9006-5416"
                                                ))

pdensity <- ggplot(data = densityplotdata, aes(x = x, y = y, group = transmitter)) +
  ggplot2::annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 0.175,
           alpha = 0.6) +
  ggplot2::annotate("rect", xmin = 4, xmax = 7, ymin = 0, ymax = 0.175,
           alpha = 0.2) +
  ggplot2::annotate("rect", xmin = 17, xmax = 21, ymin = 0, ymax = 0.175,
           alpha = 0.2) +
  ggplot2::annotate("rect", xmin = 21, xmax = 24, ymin = 0, ymax = 0.175,
           alpha = 0.6) +
  geom_line(aes(color = transmitter), linewidth = 1, alpha = 0.75) +
  scale_color_manual(values = c( "#3ACBE8", "#1CA3DE", "#0D85D8","#0041C7", 
                                 "#E298B3", "#D81B60", "#A90C45", 
                                 "#8E5FDC","#3C0C8A")) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18, 24)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~cluster2) +
  labs(x = "Time of Day (h)", y = "Activity Probability Density", color = "") +
  theme(legend.position = "left")



combo <- pbiplot + pdensity + plot_annotation(tag_levels = "a", tag_prefix = "(",
                                     tag_suffix = ")")

png("PrelimFigs/DepthActivityPCA.png",
    width = 4000,
    height = 1500,
    res = 300)
combo
dev.off()


combo2 <- (pbiplot + theme(legend.position = "right")) / (pdensity + theme(legend.position = "right")) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(heights = c(1.4,1))



png("PrelimFigs/DepthActivityPCA2.png",
    width = 2750,
    height = 2200,
    res = 300)
combo2
dev.off()

tiff("PrelimFigs/DepthActivityPCA2.tiff",
    width = 2750,
    height = 2200,
    res = 350)
combo2
dev.off()



####SENSITIVITY ANALYSIS####
#1 m cut off
dpths$activity <- 0
dpths[dpths$dpthchange > 1,]$activity <- 1

dpths %>% group_by(transmitter) %>% 
  summarize(propactive = sum(activity)/n())

#calculate the activity distribution
dpths$time01 <- (hour(dpths$detection_time_coa) * 3600 + minute(dpths$detection_time_coa) * 60 + second(dpths$detection_time_coa))/86400
dpths$timerad <- dpths$time01*2*pi

check <- dpths %>% filter(activity == 1)
check %>% group_by(transmitter) %>% summarize(n = n(),
                                              mintime = min(detection_time_coa),
                                              maxtime = max(detection_time_coa),
                                              days = maxtime - mintime) 


densities2 <- dpths %>% filter(activity == 1) %>% 
  group_by(transmitter) %>% 
  reframe(densityPlot(timerad, xcenter = "noon", rug = T, add = F, 
                      main = first(transmitter), extend = NULL))



####Sensitivity analysis 2####
#3 m cut off
dpths$activity <- 0
dpths[dpths$dpthchange > 3,]$activity <- 1

dpths %>% group_by(transmitter) %>% 
  summarize(propactive = sum(activity)/n())

#calculate the activity distribution
dpths$time01 <- (hour(dpths$detection_time_coa) * 3600 + minute(dpths$detection_time_coa) * 60 + second(dpths$detection_time_coa))/86400
dpths$timerad <- dpths$time01*2*pi

densities3 <- dpths %>% filter(activity == 1) %>% 
  group_by(transmitter) %>% 
  reframe(densityPlot(timerad, xcenter = "noon", rug = T, add = F, 
                      main = first(transmitter), extend = NULL))


densities$cutoff <- as.factor("2 m")
densities2$cutoff <- as.factor("1 m")
densities3$cutoff <- as.factor("3 m")

sensitivity <- rbind(densities, densities2, densities3)
sensitivity$cutoff <- factor(sensitivity$cutoff, 
                             levels = c("1 m", "2 m", "3 m"))

#make a plot of all cut off values
psense <- ggplot() +
  annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 0.25,
           alpha = 0.6) +
  annotate("rect", xmin = 4, xmax = 7, ymin = 0, ymax = 0.25,
           alpha = 0.2) +
  annotate("rect", xmin = 17, xmax = 21, ymin = 0, ymax = 0.25,
           alpha = 0.2) +
  annotate("rect", xmin = 21, xmax = 24, ymin = 0, ymax = 0.25,
           alpha = 0.6) +
  geom_line(data = sensitivity, aes(x = x, y = y, 
                                    color = cutoff, group = cutoff),
            linewidth = 1, alpha = 0.75) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18, 24)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~transmitter) +
  labs(x = "Time of Day", y = "Activity Probability Density", 
       color = "Depth Change Cut Off") +
  theme(legend.position = "bottom") 


png("PrelimFigs/ActivitySensitivity.png",
    width = 2000,
    height = 1500,
    res = 300)
psense
dev.off()
