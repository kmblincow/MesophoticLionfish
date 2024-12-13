#Kayla Blincow
#7/24/2024

#TCRMP plot

#The purpose of this script is to plot the lionfish TCRMP data

#NOTE: There's a weirdness where there are some instances where lionfish were 
#counted but they report no biomass..



#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(patchwork)
library(rstatix)

#load data files
biomass <- read.csv("Data/TCRMP/TCRMP_LionfishRawBiomass.csv")
count <- read.csv("Data/TCRMP/TCRMP_LionfishRawCount.csv")
sites <- read.csv("Data/TCRMP/TRCRMP_sitemetadata.csv")

allfish <- read.csv("Data/TCRMP/TCRMP_Master_Fish_Census_Dec2023.csv")

#diagnostic checks of TCRMP data
allcheck <- allfish %>% filter(Period == "Annual") %>% 
  group_by(Location, SampleYear) %>% 
  reframe(ntrans = n_distinct(Transect))


allfish$SppTotal2 <- rowSums(allfish[,9:19])
inconsistenttotals <- allfish %>% filter(SppTotal != SppTotal2)
inconsistenttotals %>% filter(str_detect(CommonName, 'ionfish'))


#NEED TO STANDARDIZE BY EFFORT--TAKE INTO ACCOUNT AREA SURVEYED EACH YEAR AND
#REPORT RESULTS BASED ON /100M2
#REMOVE GINSBURG FRINGE?


#combine things

#counts
count <- count %>% 
  group_by(Location, SampleYear) %>% 
  reframe(TotalCount = sum(SppTotal, na.rm = T))

countcombo <- right_join(count, allcheck) 
countcombo[is.na(countcombo$TotalCount),]$TotalCount <- 0
countcombo[countcombo$Location == "Jacks",]$Location <- "Jacks Bay"

countcombo <- countcombo %>% 
  filter(SampleYear > 2010) %>% 
  mutate(Density = TotalCount/ntrans)


#biomass
biomass <- biomass %>% group_by(Location, SampleYear) %>% 
  reframe(TotalBiomass = sum(SppTotal, na.rm = T))

biomasscombo <- right_join(biomass, allcheck) 
biomasscombo[is.na(biomasscombo$TotalBiomass),]$TotalBiomass <- 0
biomasscombo[biomasscombo$Location == "Jacks",]$Location <- "Jacks Bay"

biomasscombo <- biomasscombo %>% 
  filter(SampleYear > 2010) %>% 
  mutate(Biomass = TotalBiomass/ntrans)


sites <- sites %>% dplyr::select(-Latitude, -Longitude, -YearAdded)

all <- left_join(countcombo, biomasscombo)
all <- left_join(all, sites)


all_avg <- all %>% group_by(SampleYear, ReefComplex) %>% 
  summarize(mnCount = mean(Density),
            mnBiomass = mean(Biomass))

all$ReefComplex <- factor(all$ReefComplex, levels = c("Nearshore",
                                                      "Offshore",
                                                      "Mesophotic"))
all_avg$ReefComplex <- factor(all_avg$ReefComplex, levels = c("Nearshore",
                                                      "Offshore",
                                                      "Mesophotic"))


#make a plot 
pTS <- ggplot() +
  geom_point(data = filter(all, Location != "Grammanik Tiger FSA" &
                             Location != "Buck Island Tiger FSA"),
             aes(x = SampleYear, y = Density, group = Location,
                             color = ReefComplex), alpha = 0.3) +
  geom_path(data = filter(all, Location != "Grammanik Tiger FSA" &
                            Location != "Buck Island Tiger FSA"),
            aes(x = SampleYear, y = Density, group = Location,
                            color = ReefComplex), alpha = 0.3) +
  geom_point(data = filter(all, Location == "Grammanik Tiger FSA"),
             aes(x = SampleYear, y = Density, group = Location,
                 color = ReefComplex), shape = 8) +
  geom_path(data = filter(all, Location == "Grammanik Tiger FSA"),
            aes(x = SampleYear, y = Density, group = Location,
                color = ReefComplex), alpha = 0.6) +
  geom_point(data = filter(all, Location == "Buck Island STX Deep"),
             aes(x = SampleYear, y = Density, group = Location,
                 color = ReefComplex), shape = 7) +
  geom_path(data = filter(all, Location == "Buck Island STX Deep"),
            aes(x = SampleYear, y = Density, group = Location,
                color = ReefComplex), alpha = 0.6) +
  geom_path(data = all_avg, aes(x = SampleYear, y = mnCount, 
                                color = ReefComplex),
            linewidth = 0.9) +
  geom_point(data = all_avg, aes(x = SampleYear, y = mnCount, 
                                 fill = ReefComplex),
             size = 2, shape = 21) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_color_manual(values = c("lightblue3", "dodgerblue", "#0F52BA")) +
  scale_fill_manual(values = c("lightblue3", "dodgerblue", "#0F52BA")) +
  labs(x = "Year", y = expression(paste("Lionfish Biomass (ind./100 ", m^2, ")"))) +
  facet_wrap(~ReefComplex) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust=1))


#do an ANOVA to see which reef type has got higher biomass on average from 2011-2021?
m1 <- lm(Biomass ~ ReefComplex, data = all)
plot(m1) #doesn't meet assumptions

#kruskal wallis
kruskal.test(Biomass ~ ReefComplex, data = all) #significant
pairwise.wilcox.test(all$TotalBiomass, all$ReefComplex,
                     p.adjust.method = "bonferroni")
pairwise_wilcox_test(data = all, formula = Biomass ~ ReefComplex,
                     p.adjust.method = "bonferroni")


pANOVA <- ggplot(data = all, aes(x = ReefComplex, y = Biomass, 
                       fill = ReefComplex)) +
  geom_jitter(width = 0.05, alpha = 0.25, size = 0.75) +
  geom_violin(aes(color = ReefComplex), width = 0.75, alpha = 0.65,
              linewidth = 0.3) +
  ggplot2::annotate("text", x = 3, y = 2000, label = "c") +
  ggplot2::annotate("text", x = 2, y = 2000, label = "b") +
  ggplot2::annotate("text", x = 1, y = 2000, label = "a") +
  labs(x = "Reef Type", y = expression(paste("Estimated Lionfish Biomass (g/100 ", m^2, ")"))) +
  scale_color_manual(values = c("lightblue3", "dodgerblue", "#0F52BA")) +
  scale_fill_manual(values = c("lightblue3", "dodgerblue", "#0F52BA")) +
  theme_bw() +
  theme(legend.position = "none") 

combo <- pTS/pANOVA + plot_annotation(tag_levels = "a", tag_prefix = "(",
                            tag_suffix = ")")

png("PrelimFigs/TCRMP.png",
    width = 2000,
    height = 2100,
    res = 300)
combo
dev.off()

tiff("PrelimFigs/TCRMP.tiff",
    width = 2000,
    height = 2100,
    res = 350)
combo
dev.off()


#summary info
all %>% dplyr::select(ReefComplex, Location) %>% distinct() %>% 
  group_by(ReefComplex) %>% summarize(n = n())

all %>% group_by(ReefComplex) %>% 
  summarize(n = n(),
            minCnt = min(Density, na.rm = T),
            maxCnt = max(Density, na.rm = T),
            avgCnt = mean(Density, na.rm = T),
            sdCnt = sd(Density,  na.rm = T))

all %>% filter(Location == "Grammanik Tiger FSA") %>% 
  group_by(ReefComplex) %>% 
  summarize(n = n(),
            minCnt = min(Density, na.rm = T),
            maxCnt = max(Density, na.rm = T),
            avgCnt = mean(Density, na.rm = T),
            sdCnt = sd(Density,  na.rm = T))

all %>% filter(Location == "Grammanik Tiger FSA")

all %>% filter(Location == "Buck Island STX Deep") %>% 
  group_by(ReefComplex) %>% 
  summarize(n = n(),
            minCnt = min(Density, na.rm = T),
            maxCnt = max(Density, na.rm = T),
            avgCnt = mean(Density, na.rm = T),
            sdCnt = sd(Density,  na.rm = T))

all %>% filter(Location == "Buck Island STX Deep")


sites %>% group_by(ReefComplex) %>% 
  summarize(mind = min(Depth),
            maxd = max(Depth),
            meand = mean(Depth),
            sdd = sd(Depth),
            n = n())

#ID sites with no lionfish
setdiff(sites$Location, unique(all$Location))
