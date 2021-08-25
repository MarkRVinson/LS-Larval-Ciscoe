

##library
library(ggplot2)
library(rgdal)
library(tidyverse)
library(lubridate)
library(doBy)
library(here)
library(readr)
library(readxl)
library(scatterpie)
library(scales)
library(qwraps2)
library(png)
library(magick)

###########################################################################################################
##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=24, family='serif'), 
                 axis.title=element_text(size=24, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=20, family='serif'),
                 plot.caption=element_text(size=20, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=24, family='serif'), 
                 legend.title=element_text(size=24, family='serif'),
                 strip.text=element_text(size=20, family='serif'))



plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=20, family='serif'),
                  plot.caption=element_text(size=20, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  legend.title=element_text(size=24, family='serif'),
                  strip.text=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'

###########################################################################################################
raw.data<-read_excel('SuperiorLarvaeAge1s2019.xlsx', sheet = 'CatchData')
raw.data$SPECIES<-as.factor(raw.data$SPECIES)

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
##raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%y-%m-%d')

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

###ADD TARGET, NEARSHORE - OFFSHORE
raw.data<-mutate(raw.data, TARGET = ifelse(LOCATION <500, "nearshore", "offshore"))

####################################################################################################################NEARSHORE DATA####
##Composite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
towdata <- raw.data %>% 
  subset(SPECIES==217) %>%
  subset(SPECIES==217 | SPECIES ==0) %>%
  
  ##Composite the two nets into one sample
  group_by(OP_DATE, TIME, YEAR,	LOCATION,	TARGET, Mid.Lat.DD, Mid.Long.DD, DIST_SHORE_M, BEG_DEPTH,
           END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, SEABIRD_BEAM, TOW_TIME, DISTANCE) %>%
  summarise_at(vars(FISH), sum) %>%
  ungroup() %>%
  
  ##Calculate fish density - number per hectare 
  ## Sampling area in square feet = DISTANCE (in miles) * 5280 (feet per mile) * 6.56168 (Net Width in feet, two 1 m wide nets)
  ## Sampling area in hectares = area in square ft / 107639
  ## Divide the fish count by the sampling area in hectares = number of fish per hectare
  
  mutate(fish_ha = FISH/(((DISTANCE*5280)*6.56168)/107639.1))  %>%
  
  ##Add Field for Julian Day
  mutate(jday = yday(OP_DATE))  

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))


#####################################################################################################
##Fish Density Plots all YEARS
#####################################################################################################
##map with stations color coded by density
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=towdata, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=fish_ha), size=2, stroke=0.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Fish per ha', trans = "log", breaks = my_breaks, labels=my_breaks)+
  map_theme+
  ##  geom_text(aes(label=LOCATION))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoe Density',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access) +
  theme(legend.position=c(0.18,-0.13), 
        legend.direction="horizontal",
        legend.text=element_text(size=10, family='serif'), 
        legend.title=element_text(size=14, family='serif'),
        axis.title=element_text(size=16, family='serif'),
        axis.text=element_text(size=16, family='serif')) +
  facet_wrap(~YEAR) 

ggsave(here('Plots and Tables/AllYears_LS_CiscoeLarvae_Density.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(towdata, aes(x=DIST_SHORE_M/1000, y=log(fish_ha), fill=YEAR, colour=YEAR)) +
  geom_jitter(size=3, shape=21) +
  geom_smooth(se=FALSE, size=1 ) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position = "none") +
  labs( x='Distance from shore (km)', y='Number per hectare',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) 


###########################################################################################################
##subset data to whatever you want to analyze##############################################################
##NOTE: need to have the shapefiles folder in your working directory for these to work
data1 <- towdata %>%
  subset(YEAR==2019)

###########################################################################################################
#####################################################################################################
##Map of sampling locations
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


ggplot(data1, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data1, mapping=aes(Mid.Long.DD, Mid.Lat.DD), fill='black', size=3, shape=21, stroke=1.5)+
  ## scale_fill_manual(values=c('black','grey'), name='Survey', labels=c('Nearshore','Offshore'))+
  map_theme+
  ##  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Larval Cisco Sampling Stations',
       subtitle='Collections made May-July 2019')

ggsave (here('Plots and Tables/2019_LS_CiscoeLarvae_Sites_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")


##Map of sampling locations
##Nearshore and Offshore sites designated
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


ggplot(data1, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data1, mapping=aes(Mid.Long.DD, Mid.Lat.DD, fill=TARGET), size=3, shape=21, stroke=1.5)+
  scale_fill_manual(values=c('black','grey'), name='Survey', labels=c('Nearshore','Offshore'))+
  map_theme+
  ##  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Larval Cisco Sampling Stations',
       subtitle='Collections made May-July 2019')

ggsave (here('Plots and Tables/2019_LS_CiscoeLarvae_Sites_by_Survey_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")



#####################################################################################################
##map with stations color coded by density
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

my_breaks = c(1, 10, 100, 1000, 10000)

ggplot(data1, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data1, aes(Mid.Long.DD, Mid.Lat.DD, size=fish_ha, fill=fish_ha), 
             shape=21, alpha=0.6) +
  theme_bw() +
  map_theme  +
  theme(legend.position=c(0.1,0.68)) +
  scale_size_continuous(range = c(2, 12), breaks=pretty_breaks(5)) +
  scale_fill_distiller(direction = -1, palette="RdYlBu", breaks=pretty_breaks(5)) +
  guides(fill= guide_legend(title='Fish per ha'), 
         size = guide_legend(title='Fish per ha')) +
  
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoe Density',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density.png'), dpi = 300, width = 40, height = 20, units = "cm")


################################################################################
## get length data##############################################################

lengths<-read_excel('SuperiorLarvaeAge1s2019.xlsx', sheet = 'Final_leg_geno')
lengths$TL_mm<-as.numeric(lengths$TL_mm) 

##Combine effort and length tables ##############################################################

data2 <- lengths %>%
  select(Record,LOCATION,Ind,TL_mm,Lifestage,Species) %>%
  right_join(data1) %>%
  subset(Species != 'NONE') %>%
  subset(Lifestage == 'larvae')
  
  str(data2)



#####################################################################################################

data3 <- data2 %>%
  filter(Species == 'Bloater' | Species == 'Cisco' | Species == 'Kiyi')


#####################################################################################################
##Fish length as a function of sampling date
###2019

ggplot(data3, aes(x=as.Date(jday, origin = as.Date("2019-01-01")), y=TL_mm, fill=Species)) +
  geom_jitter(size=3, shape=21) +
  scale_y_continuous()+
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  scale_x_date(date_labels = "%b %e") +
 ## geom_segment(aes(y=min(data2$TL_mean-1), yend=max(data2$TL_mean+1), 
   ##                x=mean(as.Date(jday, origin = as.Date("2018-01-01"))), 
  ##                 xend=mean(as.Date(jday, origin = as.Date("2018-01-01")))),
  ##               size=1, color='black') +
    
    
      theme(legend.title = element_blank(),
        legend.direction = "horizontal", 
        legend.position= c(0.7, 1.05), 
        legend.background = element_rect(fill="thistle2",
                                         size=0.5, linetype="solid", 
                                         colour ="black"))  +
        
  labs( x='Julian Day', y='Total length (mm)',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Length_Date.png'), dpi = 300, width = 40, height = 20, units = "cm")


#####################################################################################################
##maps with sites coded by species
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##my_breaks = c(1, 10, 100, 1000, 10000)

ggplot(data2, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data2, mapping=aes(Mid.Long.DD, Mid.Lat.DD), size=3, stroke=1.5, shape=21, fill='cadetblue2')+
  map_theme+
  ##geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoes',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) +
  facet_wrap(~Species) 


ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Map_Species.png'), dpi = 300, width = 40, height = 20, units = "cm")
#####################################################################################################


#####################################################################################################
##map with proportional pie charts of species collections w/ hybrids
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

#img <- image_read(here('Plots and Tables/CiscoeLarvae.png'))

data3 <- data2 %>%
  group_by(LOCATION, Species) %>%
  count(Species) %>%
  ungroup() %>%
  spread(Species, n)  %>%
  left_join(data2) %>%
  select(LOCATION, Mid.Long.DD, Mid.Lat.DD, DIST_SHORE_M, END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, 
         Bloater, Cisco, Kiyi, 'Lake Whitefish', 'ART-HOY', 'ART-KIY', 'HOY-KIY') %>%
  distinct(LOCATION, .keep_all = TRUE) 

  data3[is.na(data3)] <- 0
  
ggplot(data3, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_scatterpie(data=data3, aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish", 
                          "ART-HOY", "ART-KIY", "HOY-KIY"), pie_scale = 0.5) +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.13, .78)) +
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoes',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Map_CiscoeSpeciesPies.png'), dpi = 300, width = 40, height = 20, units = "cm")



#####################################################################################################
##map with proportional pie charts of species collections w/o hybrids
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

img <- image_read(here('Plots and Tables/CiscoeLarvae.png'))
raster <- as.raster(img)

data3 <- data2 %>%
  group_by(LOCATION, Species) %>%
  count(Species) %>%
  ungroup() %>%
  spread(Species, n)  %>%
  left_join(data2) %>%
  select(LOCATION, Mid.Long.DD, Mid.Lat.DD, DIST_SHORE_M, END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, 
         Bloater, Cisco, Kiyi) %>%
  distinct(LOCATION, .keep_all = TRUE) 

data3[is.na(data3)] <- 0

ggplot(data3, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_scatterpie(data=data3, aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  cols= c("Bloater", "Cisco", "Kiyi"), pie_scale = 0.65) +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .83)) +
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoes',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)  +
  annotation_raster(raster,-91.2, -89, 48.5, 49)
ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Map_CiscoePies.png'), dpi = 300, width = 40, height = 20, units = "cm")




#####################################################################################################
##Species composition as of distance from shore
###2019
data4 <- data3 %>%
  pivot_longer(c('Bloater', 'Cisco', 'Kiyi'), 
               names_to = "Species", values_to = "count") %>%
  subset(count>0)

#data4 <- data3 %>%
#  pivot_longer(c('Bloater', 'Cisco', 'Kiyi'), names_to = "Species", values_to = "count") %>%
#  subset(count>0)

#data4 <- data3 %>%
#  pivot_longer(c('Cisco', 'Kiyi'), names_to = "Species", values_to = "count") %>%
#  subset(count>0)


ggplot(data4, aes(x=DIST_SHORE_M/1000, y=count, fill=Species, colour=Species)) +
  geom_jitter(size=3, shape=21) +
  geom_smooth(se=FALSE, size=1 ) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = "none") +
  labs( x='Distance from shore (km)', y='Count',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) +
  facet_wrap(~Species) 

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_SpeciesDistance.png'), dpi = 300, width = 40, height = 20, units = "cm")



#####################################################################################################
##Species composition as of trawl end depth
###2019

ggplot(data4, aes(x=END_DEPTH, y=count, fill=Species, colour=Species)) +
  geom_point(size=3, shape=21, fill='black') +
  geom_smooth(se=FALSE, size=1 ) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = "none") +
  labs( x='Depth (m)', y='Count',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) +
  facet_wrap(~Species) 

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_SpeciesDepth.png'), dpi = 300, width = 40, height = 20, units = "cm")


#####################################################################################################
##Species composition as of surface temperature
###2019

ggplot(data4, aes(x=SEABIRD_TEMP, y=count, fill=Species, colour=Species)) +
  geom_point(size=3, shape=21, fill='black') +
  geom_smooth(se=FALSE, size=1 ) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = "none") +
  labs( x='Surface Temperature (C)', y='Count',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) +
  facet_wrap(~Species) 

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_SpeciesTemp.png'), dpi = 300, width = 40, height = 20, units = "cm")


#####################################################################################################
##Length by day
###2019

data3 <- data2 %>%
group_by(jday, Species) %>% 
  mutate(minL=min(TL_mm), maxL=max(TL_mm), meanL=mean(TL_mm)) %>% 
  distinct(jday, .keep_all = TRUE) %>%
  subset(Species == 'Cisco' | Species == 'Kiyi')

ggplot(data3, aes(x=jday, y=meanL, fill=Species, colour=Species)) +
  geom_jitter(size=3, shape=21, stroke=1.5)+
  geom_line(size=1.5) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  labs( x='Julian Day', y='Mean total length (mm)',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_LengthByDay.png'), dpi = 300, width = 40, height = 20, units = "cm")


#####################################################################################################
##Temperature by day
###2019

dataT <- data2 %>%
  group_by(jday) %>% 
  mutate(minT=min(SEABIRD_TEMP), maxT=max(SEABIRD_TEMP), meanT=mean(SEABIRD_TEMP)) %>% 
  distinct(jday, .keep_all = TRUE)

ggplot(dataT, aes(x=jday, y=meanT)) +
  geom_jitter(size=3, shape=21, stroke=1.5)+
  geom_line(size=1.5) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  labs( x='Julian Day', y='Mean surface (C)',
        title='Lake Superior Larval Ciscoe Collections',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeTempsByDay.png'), dpi = 300, width = 40, height = 20, units = "cm")


#####################################################################################################
##Ciscoe species summary table
###2019

dataX <- data2  %>%
  group_by(Species) %>%
  summarise(n())
ungroup()

datz <- data2 %>%
summarise(sum(FISH))



#####################################################################################################
##map with proportional pie charts of species collections w/ hybrids
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

data5 <- lengths %>%
  select(Record,LOCATION,Ind,TL_mm,Lifestage,Species) %>%
  right_join(data1) %>%
  subset(Species != 'NONE') %>%
  subset(Lifestage == 'age1')

data6 <- data5 %>%
  group_by(LOCATION, Species) %>%
  count(Species) %>%
  ungroup() %>%
  spread(Species, n)  %>%
  left_join(data5) %>%
  select(LOCATION, Mid.Long.DD, Mid.Lat.DD, DIST_SHORE_M, END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, 
         Bloater, Cisco, Kiyi, 'ART-HOY') %>%
  distinct(LOCATION, .keep_all = TRUE) 

data6[is.na(data6)] <- 0

ggplot(data6, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_scatterpie(data=data6, aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  cols= c("Bloater", "Cisco", "Kiyi",  
                          "ART-HOY"), pie_scale = 0.75) +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.13, .78)) +
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Age-1 Ciscoes Based on Genotype Designations',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/2019_LS_CiscoeAge1Genotypes_Map.png'), dpi = 300, width = 40, height = 20, units = "cm")



#####################################################################################################
##Age1 map with proportional pie charts of species collections w/o hybrids
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

data5 <- lengths %>%
  select(Record,LOCATION,Ind,TL_mm,Lifestage,Species) %>%
  right_join(data1) %>%
  subset(Species != 'NONE') %>%
  subset(Lifestage == 'age1')


data6 <- data5 %>%
  group_by(LOCATION, Species) %>%
  count(Species) %>%
  ungroup() %>%
  spread(Species, n)  %>%
  left_join(data5) %>%
  select(LOCATION, Mid.Long.DD, Mid.Lat.DD, DIST_SHORE_M, END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, 
         Bloater, Cisco, Kiyi) %>%
  distinct(LOCATION, .keep_all = TRUE) 

data6[is.na(data6)] <- 0

ggplot(data6, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_scatterpie(data=data6, aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  cols= c("Bloater", "Cisco", "Kiyi"), pie_scale = 0.65) +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.13, .78)) +
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Age-1 Ciscoes Based on Genotype Designations',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access) 
ggsave(here('Plots and Tables/2019_LS_Age1_BlCiKiGenotype_Map.png'), dpi = 300, width = 40, height = 20, units = "cm")

