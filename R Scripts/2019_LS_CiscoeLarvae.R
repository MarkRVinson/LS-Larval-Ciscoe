
##library
library(tidyverse)
library(doBy)
library(readxl)
library(ggplot2)
library(plotrix)
library(psych)
library(rlang)
library(dplyr)
library(purrr)
library(forcats)
library(viridis)
library(reshape)
library(rgdal)
library(xlsx)
library(lubridate)
library(plyr)
library(gganimate)
library(magick)
library(grid)
library(ggforce)
library(here)
library(ggpubr)
library(ggtern)

##load the raw RVCAT data file
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
raw.data<-read.csv(here('Data','LS_Neuston_Samples.csv'))
raw.data$SPECIES<-as.factor(raw.data$SPECIES)

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2


####################################################################################################################NEARSHORE DATA####
##Compoisite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
 towdata <- raw.data %>% 
  subset(SPECIES==217 | SPECIES ==0) %>%

##Composite the two nets into one sample
  group_by(OP_DATE, TIME, YEAR,	LOCATION,	Mid.Lat.DD, Mid.Long.DD, DIST_SHORE_M, BEG_DEPTH,
          END_DEPTH, SEABIRD_TEMP, SEABIRD_CHL, SEABIRD_BEAM, TOW_TIME, DISTANCE, SPECIES) %>%
  summarise_at(vars(FISH), sum) %>%
    ungroup() %>%

##Calculate fish density - number per hectare based on tow distance
mutate(fish_ha = FISH/(((1609.34*DISTANCE)*2)*0.000001)/100) %>%

##Add Field for Julian Day
mutate(jday = yday(OP_DATE))  


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))


##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=20, family='serif'), 
                 legend.title=element_text(size=20, family='serif'),
                 strip.text=element_text(size=20, family='serif'))

 

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=20, family='serif'),
                  axis.title=element_text(size=20, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=16, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=16, family='serif'),
                  plot.caption=element_text(size=16, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=16, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'


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
  theme(legend.position=c(0.18,-0.15), 
        legend.direction="horizontal",
        legend.text=element_text(size=10, family='serif'), 
        legend.title=element_text(size=16, family='serif'),
        axis.title=element_text(size=16, family='serif'),
        axis.text=element_text(size=16, family='serif')) +
    facet_wrap(~YEAR) 

ggsave(here('Plots and Tables/AllYears_LS_CiscoeLarvae_Density.png'), dpi = 300, width = 30, height = 16, units = "cm")


####################################################################################################################NEARSHORE DATA####
##subset data to whatever you want to analyze########################################################################################################MAP##########
##NOTE: need to have the shapefiles folder in your working directory for these to work
data <- towdata %>%
 subset(YEAR==2019)



####################################################################################################################NEARSHORE DATA####
##sample site map########################################################################################################MAP##########
##NOTE: need to have the shapefiles folder in your working directory for these to work

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(data, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data, mapping=aes(Mid.Long.DD, Mid.Lat.DD), size=4, stroke=1.5)+
##  scale_color_manual(values=c('salmon','cadetblue2'), name='Survey', labels=c('Nearshore','Offshore'))+
  map_theme+
##  geom_text(aes(label=Site))+
  labs(caption=ann_data_access,
       title='Lake Superior Larval Cisco Sampling Stations',
       subtitle='Collections made May-July 2019')

ggsave (here('Plots and Tables/2019_LS_CiscoeLarvae_Sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


####################################################################################################################NEARSHORE DATA####
##sample site map, colored by sampling day##########
##NOTE: need to have the shapefiles folder in your working directory for these to work

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(data, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=jday), size=4, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Julian\nday')+
  map_theme+
  ##  geom_text(aes(label=Site))+
  labs(caption=ann_data_access,
       title='Lake Superior Larval Cisco Sampling Stations',
       subtitle='Collections made May-July 2019')

ggsave (here('Plots and Tables/2019_LS_CiscoeLarvae_Date_Sites.png'), dpi = 300, width = 40, height = 20, units = "cm")



#####################################################################################################
##map with stations color coded by density
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

my_breaks = c(1, 10, 100, 1000, 10000)

ggplot(data, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=fish_ha), size=4, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Fish per ha', trans = "log", breaks = my_breaks, labels=my_breaks)+
  map_theme+
  ##geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoe Density',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##map with stations color coded by Chla
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##my_breaks = c(1, 10, 100, 1000, 10000)

ggplot(data, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=SEABIRD_CHL), size=4, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Chl a')+
  map_theme+
  ##geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Surface Chlorophyll a concentration',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Chla.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##map with stations color coded by Beam Transmission
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##my_breaks = c(1, 10, 100, 1000, 10000)

ggplot(data, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=data, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=SEABIRD_BEAM), size=4, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Light beam\ntransmission, %')+
  map_theme+
  ##geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Surface Water Transparency',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Beam.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Fish density as a function of distance from shore
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=DIST_SHORE_M/1000, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=DIST_SHORE_M/1000, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=DIST_SHORE_M/1000, y=log(fish_ha), color=YEAR), size=1, stroke=1.5) +
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Sampling distance from shore (km)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe Distance From Shore',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Distance.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Fish density as a function of sampling date
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=jday, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=jday, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=jday, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Julian Day', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe Collections by Date',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Date.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##Fish density as a function of distance from shore
###2019

ggplot(data, aes(x=DIST_SHORE_M/1000, y=log(fish_ha))) +
  geom_point(size=4, stroke=1.5)+
  geom_smooth(size=2, stroke=1.5)+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  labs( x='Sampling distance from shore (km)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe Distance From Shore',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density_Distance.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##Fish density as a function of sampling date
###2019

ggplot(data, aes(x=jday, y=log(fish_ha))) +
  geom_point(size=4, stroke=1.5)+
  geom_smooth(method=lm, size=1, stroke=1.5) +
  stat_cor(label.x = 180, label.y = 8)+ 
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  labs( x='Julian Day', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe Collections by Date',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density_Date.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Fish density as a function of water temperature
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=SEABIRD_TEMP, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=SEABIRD_TEMP, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=SEABIRD_TEMP, y=log(fish_ha), color=YEAR), size=1, stroke=1.5) +
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Surface water temperature (C)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Water Temperatures',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Temperature.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##Fish density as a function of water temperature
###2019

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(data, aes(x=SEABIRD_TEMP, y=log(fish_ha))) +
  geom_point(size=4, stroke=1.5)+
  geom_smooth(size=1, stroke=1.5) +
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Surface water temperature (C)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Water Temperatures',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density_Temperature.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Fish density as a function of Chl a
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=SEABIRD_TEMP, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=SEABIRD_CHL, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=SEABIRD_CHL, y=log(fish_ha), color=YEAR), size=1, stroke=1.5) +
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Surface water Chl a concentration', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Chlorophyll a Concentration',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Chla.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##Fish density as a function of Beam Transmission
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=SEABIRD_TEMP, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=SEABIRD_BEAM, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=SEABIRD_BEAM, y=log(fish_ha), color=YEAR), size=1, stroke=1.5) +
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Surface water transparency, 0-100', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Water Transparency',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Beam.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Fish density as a function of bathymetric depth
###ALL YEARS

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(towdata, aes(x=(BEG_DEPTH+END_DEPTH)/2, y=log(fish_ha))) +
  geom_point(data=towdata, mapping=aes(x=(BEG_DEPTH+END_DEPTH)/2, y=log(fish_ha), color=YEAR), size=4, stroke=1.5)+
  geom_smooth(data=towdata, mapping=aes(x=(BEG_DEPTH+END_DEPTH)/2, y=log(fish_ha), color=YEAR), size=1, stroke=1.5) +
  scale_color_gradient(low='cadetblue2', high='red', name='Year')+
  scale_y_continuous()+
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Bathymetric depth (m)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Bathymetric Depth',
        subtitle='Collections made May-July for the years 2014-2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Depth.png'), dpi = 300, width = 30, height = 16, units = "cm")

#####################################################################################################
##Fish density as a function of bathymetric depth
###2019

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(data, aes(x=(BEG_DEPTH+END_DEPTH)/2, y=log(fish_ha))) +
  geom_point(size=4, stroke=1.5)+
  ##geom_smooth(size=1, stroke=1.5) +
  geom_smooth(method=lm, size=1, stroke=1.5) +
  stat_cor(label.x = 250, label.y = 8)+ 
  scale_y_continuous()+
  scale_x_continuous()+  
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Bathymetric depth (m)', y='Log(Fish per hectare)',
        title='Lake Superior Larval Ciscoe in Relation to Bathymetric Depth',
        subtitle='Collections made May-July 2019',
        caption=ann_data_access)

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density_Depth.png'), dpi = 300, width = 30, height = 16, units = "cm")


#####################################################################################################
##Ternary plot of larvarl Fish density
###2019
my_breaks = c(1, 100, 1000, 25000, 75000)

ggtern(data=data, aes(x=DIST_SHORE_M/1000, y=(BEG_DEPTH + END_DEPTH)/2, z=SEABIRD_TEMP)) +
  geom_point(aes(color=fish_ha),size=4, stroke=1.5) +
 #   stat_density_tern(geom = 'polygon', aes(fill = ..level.., alpha = ..level..)) +
  guides(alpha='none') +
  geom_density_tern() +
  tern_limits(T=.3,L=0.5,R=.2) + 
  theme_zoom_T(0.6) +
  scale_color_gradient(low='cadetblue2', high='red', name='Fish per ha', trans = "log", breaks = my_breaks, labels=my_breaks)  +
  labs(x="Distance from Shore", y="Bathymetric Depth",z="Temperature",
       title='Lake Superior Larval Ciscoe',
       subtitle='Collections made May-July 2019',
       caption=ann_data_access) +
  theme_bw()  +
  plot_theme +
  theme_showarrows() + 
  theme(legend.position      = c(0, 1),
        legend.justification = c(0, 1),
        legend.box.just      = 'left') 

ggsave(here('Plots and Tables/2019_LS_CiscoeLarvae_Density_Ternary.png'), dpi = 300, width = 30, height = 16, units = "cm")

