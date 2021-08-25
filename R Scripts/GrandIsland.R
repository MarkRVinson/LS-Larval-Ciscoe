
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
library(plotrix)

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

ann_data_access<-'Data: U.S. Geological Survey & The Nature Conservancy'

##In case we're making a map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)



###########################################################################################################
##Load data
ciscoes <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Fish')
effort<-read_excel(here('Data', 'GICiscoes.xlsx'), sheet = 'Effort') %>%
  mutate(MidDepth_ft = ((StartDepth_ft+EndDepth_ft)/2), 
         MidDepth_m = ((StartDepth_m+EndDepth_m)/2), 
         week = week(Date))


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))


##Merge with effort data
ciscoes.all <- ciscoes %>%
  left_join(effort) %>%
  left_join(sci.names) %>%
  select(1:2, 4:9, 13:16, 19:24, 27:28, 30:34)

ciscoes.winter <- ciscoes.all %>%
  filter(Method == "Gillnet") 


##########################################################################################
##Bring data from Lake Michigan 1930-32
##########################################################################################
##Fulmar operations data
fulmar.op <-read_csv(here('Data','Fulmar_op.csv')) 

##Fulmar fish data
fulmar.fish <-read_csv(here('Data','Fulmar_fishv2.csv')) 

##Join the two files and select relevant fields
fulmar.all <- fulmar.fish %>%
  left_join(fulmar.op) %>% 
  mutate(Sex = case_when(
    SEX == 1 ~ 'male',
    SEX == 2 ~ 'female')) %>%
  mutate(Maturity = case_when(
           MATURITY == 1 ~ 'immature',
           MATURITY == 2 ~ 'developing', 
           MATURITY == 7 ~ 'abnormal', 
           MATURITY == 4 ~ 'ripe', 
           MATURITY == 6 ~ 'spent', 
           MATURITY == 3 ~ 'gravid')) %>%
  mutate(GNetMesh_in = case_when(
    MESH_SIZE == 23 ~ 60/25.4,
    MESH_SIZE == 24 ~ 64/25.4,
    MESH_SIZE == 25 ~ 67/25.4,
    MESH_SIZE == 26 ~ 70/25.4,
    MESH_SIZE == 30 ~ 76/25.4)) %>%
  mutate(Sample = OP_ID,
         Date = OP_DATE,
         StartDepth_m = BEG_DEPTH,
         EndDepth_m = END_DEPTH,
         Lat_DD = LAT,
         Long_DD = LON,
         Length_mm = LENGTH, 
         MidDepth_m = ((StartDepth_m+EndDepth_m)/2),
         MidDepth_ft = MidDepth_m * 3.281,
         week = week(Date)) %>%
  select(Sample, Date, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm) %>%
  left_join(sci.names) 

fulmar.maturity <- fulmar.all %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  drop_na() %>%
  group_by(week, Maturity) %>%
  summarise(Maturity.n = n()) 

ggplot(fulmar.maturity, aes(fill = Maturity, x=week, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.08, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Michigan Kiyi Female Maturation',
        subtitle='Collections made 1930-1932',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/LakeMichigan.Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")




##########################################################################################
##Annual summaries of effort and Species collected - winter gill netting
#####################################################################################################
###Species collected
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

year.sum.fish <-ciscoes.winter %>%
  select(3, 9, 24) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  group_by(winter.year, SPECIES) %>%
  summarise(fishes = n()) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

  
ggplot(year.sum.fish, aes(x=winter.year, y=fishes)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=winter.year, xend=winter.year, y=0, yend=fishes), size=1, color='black')+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks()) +
  annotate(geom="text", x=2018, y=100, label="Weather\nprevented\ncollections", size=8, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.2, .7)) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Winter', y='Count',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.YearSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")

###########################################################################################################
###Effort number of sets by mesh
year.effort.sum <-ciscoes.winter %>%
  select(9:10, 12:14, 21:22) %>%
  distinct(Date, GNetGang, GNetMesh_in) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  group_by(winter.year, GNetMesh_in) %>%
  summarise(sets = n()) %>%
  ungroup() 


ggplot(year.effort.sum, aes(x=winter.year, y=sets)) +
  geom_jitter(aes(fill= as_factor(GNetMesh_in)), shape=21, alpha=0.8, size=12) +
  geom_segment(x=2017.5, xend=2017.5, y=0, yend=4, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2018.5, xend=2018.5, y=0, yend=7, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2019.5, xend=2019.5, y=0, yend=11, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2020.5, xend=2020.5, y=0, yend=11, linetype = "dashed", size=1, color='black') +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks()) +
  annotate(geom="text", x=2018, y=3, label="Weather\nprevented\ncollections", size=8, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = c(.25, .85)) +
  guides(fill= guide_legend(title = "Mesh in inches", direction="horizontal", title.position = "top")) + 
  labs( x='Winter', y='Number of gill net sets',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.YearSets.png'), dpi = 300, width = 35, height = 16, units = "cm")

##########################################################################################
##Sample site map
#####################################################################################################

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ciscoes.map <- ciscoes.all %>%
  select(Sample, Date, Method, Lat_DD, Long_DD, MidDepth_m) %>%
  mutate(Season = case_when(
    month(Date) >10 ~ 'winter',
    month(Date) <=10 & month(Date) >=9 ~ 'fall', 
    month(Date) <9 & month(Date) >=6 ~ 'summer', 
    month(Date) <6 ~ 'spring'))


ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_point(shape=21, fill = 'deepskyblue', size=10) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.4,46.85)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.75,-86.5)) +
  theme_bw() +
  map_theme +
  theme(legend.position = c(.85, .8), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collection Sites',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/GrandIsleGillNetLocations.png'), dpi = 300, width = 40, height = 20, units = "cm")


ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  geom_point(data=subset(ciscoes.map, Method == "Gillnet"), 
             mapping=aes(Long_DD, Lat_DD, fill=MidDepth_m), shape=21, size=10) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Depth (m)") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.4,46.85)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.75,-86.5)) +
  theme_bw() +
  map_theme +
  theme(legend.position = c(.85, .8), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collection Sites',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/GrandIsleGillNetLocationsDepths.png'), dpi = 300, width = 40, height = 20, units = "cm")



ggplot(ciscoes.map, aes(x=Long_DD, y = Lat_DD)) +
  geom_jitter(aes(fill = Method), shape=21, size=7) + ## , alpha=0.4) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(.15, .85), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Kiyi Gonad Maturation Collection Locations',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi_GSI_Sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


ggplot(ciscoes.map, aes(x=Long_DD, y = Lat_DD)) +
  geom_jitter(aes(fill = Season), shape=21, size=7) + ## , alpha=0.4) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(.15, .85), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Kiyi Gonad Maturation Collection Locations',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi_GSI_Seasons.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################
##Ciscoe species collection by mesh
#####################################################################################################
##summarize catch by mesh and depth - bubble plot
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in, StartDepth_ft) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

## Catch by mesh and depth, sized by catch
ggplot(catch.mesh, aes(x=GNetMesh_in, y=StartDepth_ft)) +
  geom_point(data=catch.mesh, aes(x=GNetMesh_in, y=StartDepth_ft, size=freq, fill=legend), 
             shape=21, alpha=0.8) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_reverse(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = "none") +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Mesh (inches)', y='Depth (ft)',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#########################################################################################
##All ciscoes
##Catch by mesh
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

# sample size by mesh
Mcatch <- ciscoes.winter %>%
  group_by(GNetMesh_in) %>% 
  summarize(mesh=n())


catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Mcatch) %>%
  mutate(naxis = paste0(GNetMesh_in, "\n", "n=", mesh)) %>% 
  mutate(fish = "fish") %>%
  unite("xaxis", naxis, fish, sep=" ") %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME, fish, sep=", ") 

ggplot(catch.mesh, aes(x=xaxis, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=xaxis, xend=xaxis, y=0, yend=freq), size=1, color='black')+
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = c(.15, .7), 
        legend.text = element_text(size=20, family='serif')) + 
#        axis.text.x=element_text(size=20, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Gill net mesh (inches)', y='Proporton of species` total catch by mesh',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.MeshSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth in 1.5 inch mesh
Scatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(MidDepth_ft, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_ft, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=MidDepth_ft, xend=MidDepth_ft, y=0, yend=freq), size=1, color='black')+
#  geom_segment(aes(x=min(MidDepth_ft), xend=max(MidDepth_ft), y=0.05, yend=0.05), size=3, color='black') +
  scale_x_continuous(breaks=pretty_breaks(), limits = c(200, 700)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = c(.12, .73), 
        legend.text = element_text(size=20, family='serif')) + 
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Depth (ft)', y='Proporton of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections in 1.5 inch Mesh',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.Depth_1.5Inch.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth all meshes
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  group_by(MidDepth_ft, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_ft, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=MidDepth_ft, xend=MidDepth_ft, y=0, yend=freq), size=1, color='black')+
  #  geom_segment(aes(x=min(MidDepth_ft), xend=max(MidDepth_ft), y=0.05, yend=0.05), size=3, color='black') +
  scale_x_continuous(breaks=pretty_breaks(), limits = c(300, 750)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.15, .72)) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Depth (ft)', y='Proporton of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DepthSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")


##############################################################################################
##Depth Density all meshes

ggplot(catch.depth, aes(x=MidDepth_ft, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks(), limits = c(200, 800)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (ft)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Ciscoes.DepthDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##Depth density for bloater, cisco, and kiyi

##All ciscoes - by depth all meshes
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  group_by(SPECIES, MidDepth_ft) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

ggplot(catch.depth, aes(x=MidDepth_ft, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks(), limits = c(200, 800)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (ft)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Cisco.Bloater.Kiyi.DepthDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")

#########################################################################################
##Kiyi- by depth - density plot for 1.5 inch mesh

catch.depth <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES, MidDepth_ft) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  left_join(sci.names)

Scatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- catch.depth %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_ft, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks(), limits = c(200, 800)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
labs( x='Depth (ft)', y='Relative density',
      title='Lake Superior Grand Island Winter Ciscoe Collections in 1.5 inch Mesh',
      subtitle='Collections made November-January, 2017-2021',
      caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.DepthDensity_1.5inch.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by mesh - density plot
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

ggplot(catch.mesh, aes(x=GNetMesh_in, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.12,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous() +
  scale_fill_brewer(palette="Accent") +
  labs( x='Gill net mesh (inches)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.MeshDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
#Catch by date
##All ciscoes - all meshes all depths
Scatch <- ciscoes.winter %>%
 # filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.date <- ciscoes.winter %>%
 # filter(GNetMesh_in == 1.5) %>%
  group_by(Date, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.date, aes(x=as.factor(Date), y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=10) +
  geom_segment(aes(x=as.factor(Date), xend=as.factor(Date), y=0, yend=freq), size=1, color='black')+
#  scale_x_discrete(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Date', y='Proportion of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DateSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")

#########################################################################################
#Catch by date - 1.5 inch mesh
##All ciscoes - all meshes all depths
Tcatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.date <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(Date, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.date, aes(x=as.factor(Date), y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=10) +
  geom_segment(aes(x=as.factor(Date), xend=as.factor(Date), y=0, yend=freq), size=1, color='black')+
  #  scale_x_discrete(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Date', y='Proportion of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections in 1.5 inch Mesh',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DateSpecies.1.5inch.png'), dpi = 300, width = 35, height = 16, units = "cm")



#############################################################################################
## GSI for Kiyi
#############################################################################################
###Females
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  subset(Sex != 'unknown') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week) %>%
  summarise(GSI.n = n(), GSIweek.median = median(GSI), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(shape=21, fill = 'deepskyblue', size=10) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.Female.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")



##By year
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  subset(Sex != 'unknown') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, year = year(Date), jday = yday(Date), month = month(Date), week = week(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  drop_na() %>%
  group_by(winter.year, week) %>%
  summarise(GSI.n = n(), GSIweek.median = median(GSI), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, fill=as.factor(winter.year))) +
  geom_jitter(shape=21, size=10) +
#  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
#  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.Female.Maturation.Year.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Males & Females
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206) %>%
  subset(Sex != 'unknown') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Sex) %>%
  summarise(GSI.n = n(), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 


ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(aes(fill = Sex), shape=21, size=10, alpha=0.4) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
#  theme(legend.position = "none") +
  theme(legend.position = c(.15, .9)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Kiyi.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Stacked bar maturity class
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
    mutate(jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Maturity) %>%
  summarise(Maturity.n = n()) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'spent' ~ 'spent')) %>%
  filter(Mature.class != 'immature')

ggplot(kiyi.gsi, aes(fill = Mature.class, x=Dorder, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.65, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Bloater
bloater.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 204) %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Sex) %>%
  summarise(GSI.n = n(), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 


ggplot(bloater.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(aes(fill = Sex), shape=21, size=10, alpha=0.4) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = bloater.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Bloater Maturation',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Bloater.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Cisco
cisco.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 202) %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Sex) %>%
  summarise(GSI.n = n(), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 


ggplot(cisco.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(aes(fill = Sex), shape=21, size=10, alpha=0.4) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = cisco.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Cisco Maturation',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Cisco.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")



####################################################################################################
##Combine Lake Michigan and Grand Island Fish
####################################################################################################

allfish <- ciscoes.all %>%
  select(Sample, Date, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME, SCIENTIFIC_NAME) %>%
  bind_rows(fulmar.all) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

###Stacked bar maturity class
maturity <- allfish %>%
  select(Date, week, Period, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  drop_na() %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'spent' ~ 'spent',
    Maturity == 'gravid' ~ 'gravid')) %>%
  filter(Mature.class != 'immature') %>%
  group_by(Period, week, Mature.class) %>%
  summarise(Maturity.n = n()) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 
  
  
ggplot(maturity, aes(fill = Mature.class, x=Dorder, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Female Kiyi Maturation',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)
  
ggsave(here('Plots and Tables/KiyiMaturation.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Sex ratios
sexratio <- allfish %>%
  group_by(Period, week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') 
  
ggplot(sexratio, aes(fill = Sex, x=week, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.07, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Kiyi Sex Ratio',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)

ggsave(here('Plots and Tables/Kiyi.SexRatios.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth all meshes
allfish <- ciscoes.winter %>%
  select(Sample, Date, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME, SCIENTIFIC_NAME) %>%
  bind_rows(fulmar.all) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

Tcatch <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarize(fish=n())

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  #subset(SPECIES==206 | 
  #         SPECIES == 204 | 
  #         SPECIES == 202 |  
  #         SPECIES == 207 |
  #         SPECIES == 208 |
  #         SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206) %>%
  ungroup()

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
#  geom_vline(data=depth.sum, aes(xintercept=depth.mean, color= Period), size=1, show.legend = FALSE) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.15,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Kiyi Depth Distribution',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/KiyiDepths.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


#### All ciscoes

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) 

## unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
    ungroup()  %>%
  left_join(sci.names) 
  

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
#  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
#  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
#  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
#  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.2,-0.16), legend.direction = "horizontal") +
  
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Ciscoe Depth Distributions',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~COMMON_NAME)

ggsave(here('Plots and Tables/SpeciesDepths.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")



####################################################################################################
##Combine Lake Michigan and Grand Island Fish for months October - January
####################################################################################################

##Sex ratios
allfish <- ciscoes.all %>%
  select(Sample, Date, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME, SCIENTIFIC_NAME) %>%
  bind_rows(fulmar.all) %>%
  subset(month(Date) >= 10) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

sexratio <- allfish %>%
  group_by(Period, week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') 


ggplot(sexratio, aes(fill = Sex, x=week, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.4, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Kiyi Sex Ratio',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)

ggsave(here('Plots and Tables/Kiyi.SexRatios.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth all meshes
allfish <- ciscoes.winter %>%
  select(Sample, Date, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME, SCIENTIFIC_NAME) %>%
  bind_rows(fulmar.all) %>%
  subset(month(Date) >= 10) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

Tcatch <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarize(fish=n())

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  #subset(SPECIES==206 | 
  #         SPECIES == 204 | 
  #         SPECIES == 202 |  
  #         SPECIES == 207 |
  #         SPECIES == 208 |
  #         SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206) %>%
  ungroup()

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  #  geom_vline(data=depth.sum, aes(xintercept=depth.mean, color= Period), size=1, show.legend = FALSE) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.15,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Kiyi Depth Distribution',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/KiyiDepths.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")


#### All ciscoes

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) 

## unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  ungroup()  %>%
  left_join(sci.names) 


ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  #  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  #  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  #  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  #  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.2,-0.16), legend.direction = "horizontal") +
  
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Ciscoe Depth Distributions',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~COMMON_NAME)

ggsave(here('Plots and Tables/SpeciesDepths.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")



##########################################################################################################
#Kiyi fecundity 
##########################################################################################################
f1 <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'KiyiEggs')  

fecundity <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'KiyiEggs') %>%
  pivot_longer(7:11, names_to = "Replicate", values_to = "Eggs_g", values_drop_na = TRUE) %>%
  group_by(Fish_Number) %>%
  summarise(EggWt.mean = mean(Eggs_g), na.rm=TRUE)

KiyiEggs <- fecundity %>%
  left_join(f1) %>%
  mutate(Fresh = OvFreshWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  mutate(Frozen = OvFrozenWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  select(Fish_Number, Length_mm, Weight_g, Fresh, Frozen) 
 
ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
#  geom_jitter(data = KiyiEggs, aes(x=Length_mm, y = Frozen), fill = 'magenta',  shape=21, size=4) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
#  scale_x_log10() +
#  scale_y_log10() +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total length (mm)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Collections were made December 2018 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.png'), dpi = 300, width = 35, height = 16, units = "cm")


  


