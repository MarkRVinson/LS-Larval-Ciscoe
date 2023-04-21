
##library
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(here)
library(readxl)
library(lubridate)
library(scatterpie)
library(scales)
library(ggrepel)
library(patchwork)
library(ggridges)
library(viridis)
library(rstatix)
library(rpart)
library(rpart.plot)
library(geosphere)



###########################################################################################################
##set default themes for all plots and maps
map_theme<-theme(strip.text=element_text(size=24, family='serif'),
                 axis.text=element_text(size=24, family='serif'),
                 axis.title=element_text(size=24, family='serif'),
                 legend.text=element_text(size=24, family='serif'),
                 legend.title=element_text(size=24, family='serif'),
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=22, family='serif'),
                 plot.caption=element_text(size=20, family='serif'),
                 legend.position=c(0.1,0.7))

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  strip.text=element_text(size=24, family='serif'),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  legend.text=element_text(size=24, family='serif'),
                  legend.title=element_text(size=24, family='serif'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=22, family='serif'),
                  plot.caption=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/P9XVOLR1'

#######################################################################################
#######################################################################################
##ICE COVER_____________________________________________________________________________________________________________________________ICE COVER__________
##ice data, access the most recent data from: https://www.glerl.noaa.gov/data/ice/#historical -> Daily averages by lake -> Lake Superior 
##save file as '~/R/Scripts/RVCAT/sup_ice_recent'

raw.ice<-read_xlsx(here('Data','LS_GLERL_Ice.xlsx'), sheet = 'WideIce')

##change date format into usable form
raw.ice$date<-as.character(raw.ice$date)
raw.ice$date<-parse_date(ymd(raw.ice$date, format='%d-%b-%y')) 



ice <- raw.ice %>%
  pivot_longer(2:51, names_to = 'ice.year', values_to = 'ice.cover', names_transform = list(ice.year = as.numeric)) %>% 
  mutate(Day = day(date),
         Month = month(date), 
         Month.long = month.name[Month],
         Jday = yday(date)) %>%
  mutate(Year = case_when(
    Jday > 250 ~ ice.year - 1,
    Jday < 250 ~ ice.year)) %>%
  select(-date) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  mutate(jday2 = case_when(
    Jday < 250 ~ Jday*1,
    Jday > 250 ~ (Jday*-1 + 366) * -1)) 


ice <- ice %>%
  mutate(across(Month.long, factor, levels = c("November", "December",
                                               "January", "February", 
                                               "March", "April",
                                               "May", "June")))

ggplot(subset(ice, ice.year >= 2014)) + 
  aes(x=ice.year, y = ice.cover) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI ice cover')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  #  labs(caption=ann_data_access) +
  #  title='Lake Superior Larval Ciscoe Abundance',
  #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=4, scales="free_y")

ggsave(here('Plots/Best','IceCover.png'), dpi = 150, width = 40, height = 40, units = "cm")  


ice.month <- ice %>%
  subset(ice.year >=2014) %>%
  select(ice.year, Month, ice.cover) %>%
  rename(Year = ice.year) %>%
  group_by(Year, Month) %>%
  summarise(ice.min = min(ice.cover, na.rm=TRUE),
            ice.max = max(ice.cover, na.rm=TRUE),
            ice.mean = mean(ice.cover, na.rm=TRUE)) %>%
  ungroup()


#########################################################################################################################
###############################################################################################################
##Get BT temperature data
##load data 
BTdata1 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample")
BTdata1$Date<-as.character(BTdata1$Date)
BTdata1$Date<-ymd(BTdata1$Date)

BTdata.neuston <- BTdata1 %>%
  subset(Year >= 2014) %>%
  select(Sample, LOCATION, Date, SurfaceT, 'd<3mT', ChlSurf, Chl3m) %>%
  rename(T3m = 'd<3mT')  %>%
  group_by(LOCATION, Date) %>%
  summarise(TSurface = mean(SurfaceT),
            T3m = mean(T3m),
            ChlSurface = mean(ChlSurf),
            Chl3m = mean(Chl3m)) %>%
  ungroup() %>%
  mutate(Year = year(Date)) 


#######################################################################################
#########################################################################################################################
##Get weather data

Weather.sites <-read_excel(here('Data',"WeatherData.xlsx"), sheet="Location")

Weather1 <-read_excel(here('Data',"WeatherData.xlsx"), sheet="Weather1")
Weather1$Date<-as.character(Weather1$Date)
Weather1$Date<-ymd(Weather1$Date)

Weather2 <-read_excel(here('Data',"WeatherData.xlsx"), sheet="Weather2")
Weather2$Date<-as.character(Weather2$Date)
Weather2$Date<-ymd(Weather2$Date)

Weather <- Weather1 %>%
  bind_rows(Weather2) %>%
  mutate(Year = year(Date), 
         Month = month(Date),
         Month.long = month.name[Month]) 
  
Weather <- Weather %>%
  mutate(across(Month.long, factor, levels = c("January", "February", "March", "April",
                                               "May", "June", "July", "August", "September",
                                               "October", "November", "December")))

ggplot(subset(Weather, 
              Station == 'DISW3' |
                Station == 'PILM4' |
                Station == 'STDM4' |
                Station == 'ROAM4')) + 
  aes(x=Year, y = RWSP) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI wind speed (m/s)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  #  labs(caption=ann_data_access) +
  #  title='Lake Superior Larval Ciscoe Abundance',
  #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=4, scales="free_y")

ggsave(here('Plots/Best','WindSpeed.png'), dpi = 150, width = 40, height = 40, units = "cm")  


ggplot(subset(Weather, 
              Station == 'DISW3' |
                Station == 'PILM4' |
                Station == 'STDM4' |
                Station == 'ROAM4')) + 
  aes(x=Year, y = ATMP) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI air temperature (C)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  #  labs(caption=ann_data_access) +
  #  title='Lake Superior Larval Ciscoe Abundance',
  #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=4, scales="free_y")

ggsave(here('Plots/Best','AirTemp.png'), dpi = 150, width = 40, height = 40, units = "cm")  


Weather.month <- Weather %>%
#  subset(Station == 'DISW3' |
#           Station == 'PILM4' |
#           Station == 'STDM4' |
#           Station == 'ROAM4') %>%
  mutate(Year = year(Date), 
         Month = month(Date),
         Month.long = month.name[Month]) %>%
  group_by(Year, Month) %>%
  summarise(WSPD.min = min(RWSP, na.rm=TRUE),
            WSPD.max = max(RWSP, na.rm=TRUE),
            WSPD.mean = mean(RWSP, na.rm=TRUE),
            WSPD.sum = sum(RWSP, na.rm=TRUE),
            AirT.min = min(ATMP, na.rm=TRUE),
            AirT.max = max(ATMP, na.rm=TRUE),
            AirT.mean = mean(ATMP, na.rm=TRUE)) %>%
  ungroup()

ggplot(subset(Weather, 
              Station == 'DISW3' |
                Station == 'PILM4' |
                Station == 'STDM4' |
                Station == 'ROAM4')) + 
  aes(x=Year, y = WDIR) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI wind direction')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  #  labs(caption=ann_data_access) +
  #  title='Lake Superior Larval Ciscoe Abundance',
  #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=4, scales="free_y")

ggsave(here('Plots/Best','WindDirection.png'), dpi = 150, width = 40, height = 40, units = "cm")  

####################################################################################################################NEARSHORE DATA####
##Neuston data
##Neuston data 2014-2022

codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

neuston.data<-read.csv(here('Data','2014-2022_Neuston_data.csv'))
neuston.data$SPECIES<-as.factor(neuston.data$SPECIES) 

ShoreDistance <- read_excel(here('Data','ShoreDistance.xlsx'))


##change date format into usable form
neuston.data$OP_DATE<-as.character(neuston.data$OP_DATE)
neuston.data$OP_DATE<-parse_date(neuston.data$OP_DATE, format='%d-%b-%y')

neuston.data <- neuston.data %>%
#  left_join(BTdata.neuston) %>%
  left_join(ShoreDistance) %>%
  left_join(sci.names) %>%
  mutate(Fishing.Depth = 0.5,
         Gear = 'Neuston') %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Month.short = month.abb[Month],
         Month.long = month.name[Month],
         Jday = yday(OP_DATE), 
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, Day, sep=" ", remove = FALSE) 

neuston.data[is.na(neuston.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

neuston.data[is.na(neuston.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

neuston.data$Mid.Lat.DD<-(neuston.data$BEG_LATITUDE_DD+neuston.data$END_LATITUDE_DD)/2
neuston.data$Mid.Long.DD<-(neuston.data$BEG_LONGITUDE_DD+neuston.data$END_LONGITUDE_DD)/2
neuston.data$Mid.Depth<-(neuston.data$BEG_DEPTH+neuston.data$END_DEPTH)/2

###########################################################
neuston.effort <- neuston.data %>%
  select(OP_ID, TARGET, Date, TIME, Date1, Day, Week, 
         Month, Month.short, Month.long, Jday, Year, Gear, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Fishing.Depth) %>%
  distinct(OP_ID, .keep_all = TRUE) %>% 
  left_join(BTdata.neuston)

#########################################################################################
##Composite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
##Composite the two nets into one sample
##Calculate fish density - number per hectare 
## Sampling area in square feet = DISTANCE (in miles) * 5280 (feet per mile) * 6.56168 (Net Width in feet, two 1 m wide nets)
## Sampling area in hectares = area in square ft / 107639
## Divide the fish count by the sampling area in hectares = number of fish per hectare

neuston.1 <- neuston.data %>%   
  group_by(Date, TIME, LOCATION, SPECIES) %>%
  summarise(Fish = sum(N)) %>%
  ungroup() %>%
  left_join(neuston.data) %>%
  left_join(BTdata.neuston) %>%
  distinct(Date, TIME, LOCATION, SPECIES, .keep_all = TRUE) %>%
  mutate(fish_ha = Fish/(((DISTANCE*5280)*6.56168)/107639.1)) %>%
  select(OP_ID, TARGET, Date, TIME, Date1, Week, Day, Month, Month.short, Month.long, Jday, Year, Gear, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Fishing.Depth, ShoreDistance, 
         SPECIES, COMMON_NAME, Fish, fish_ha, TSurface, T3m, ChlSurface, Chl3m) 


############################################################################################ 
############################################################################################
##Get the number fish with genetics
###Get genetics data
data_path <- here('Data', 'genetics.xlsx')
genetics <-read_excel(data_path, sheet = 'Genomics')

##Make SPECIES and GENETICS factors
genetics$SPECIES<-as.factor(genetics$SPECIES)
genetics$GENETICS<-as.factor(genetics$GENETICS)

neuston.genetics <- neuston.effort %>%
  left_join(genetics) %>%
  select(-Weight) %>%
  mutate(GENETICS_NAME = case_when(
    GENETICS == 202 ~ "Cisco",
    GENETICS == 203 ~ "Lake Whitefish",
    GENETICS == 204 ~ "Bloater",
    GENETICS == 206 ~ "Kiyi",
    GENETICS == 208 ~ "Shortnose Cisco", 
    GENETICS == 210 ~ "Pygmy Whitefish",
    GENETICS == 217 ~ "unidentified ciscoe",
    GENETICS == 218 ~ "ciscoe hybrid")) %>%
  drop_na(GENETICS)


genetics.sum <- neuston.genetics %>%
  group_by(Year) %>%
  summarise(Genetics = n()) %>%
  ungroup() 


genetics.fish.sum <- neuston.genetics %>%
  group_by(Year, GENETICS_NAME) %>%
  summarise(Genetics.fish = n()) %>%
  ungroup() %>%
  left_join(genetics.sum)

##############################################################################################################
####Neuston annual summary
neuston.sites.total <- neuston.1 %>%
  summarise(All.Sites = n_distinct(LOCATION), 
            Present.Sites = n_distinct(LOCATION[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                                  SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                                  SPECIES == 211 | SPECIES == 217 | SPECIES == 218]))

neuston.sites <- neuston.1 %>%
#  subset(Year == 2019) %>%
#  select(LOCATION, Date, TIME, SPECIES, COMMON_NAME, Fish) %>%
  group_by(LOCATION) %>% 
  summarise(Collections = n_distinct(Date,TIME), 
            Fish = sum(Fish[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                                  SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                                  SPECIES == 211 | SPECIES == 217 | SPECIES == 218]))

neuston.sites.years <- neuston.1 %>%
#  subset(Year == 2019) %>%
  group_by(Year) %>% 
#  select(OP_ID,LOCATION,Date, TIME, SPECIES, Fish)
  summarise(All.Sites = n_distinct(LOCATION),
            Absent.sites = n_distinct(LOCATION[SPECIES == 0]),
            Present.Sites = n_distinct(LOCATION[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                                 SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                                 SPECIES == 211 | SPECIES == 217 | SPECIES == 218])) %>%
  unite(Sites, All.Sites, Present.Sites, sep = ' / ', remove = FALSE)
  


neuston.collections.years <- neuston.1 %>%
  distinct(Year, LOCATION, Date, TIME, .keep_all = TRUE) %>%
  #select(OP_ID,LOCATION,Date, TIME, SPECIES, Fish)
  group_by(Year) %>% 
  summarise(All = n(), 
            All.Collections = n_distinct(OP_ID), 
            Absent.Collections = n_distinct(OP_ID[SPECIES == 0]),
            Present.Collections = n_distinct(OP_ID[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                                  SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                                  SPECIES == 211 | SPECIES == 217 | SPECIES == 218])) %>%
  unite(Collections, All.Collections, Present.Collections, sep = ' / ', remove = FALSE)


  
neuston.sum.month <- neuston.1 %>%
  select(Date, Month, Month.long, Year, LOCATION, SPECIES, Fish, fish_ha) %>%
  group_by(Year, Month.long) %>%
  summarise(NOHA.mean = mean(fish_ha[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                       SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                       SPECIES == 211 | SPECIES == 217 | SPECIES == 218 | SPECIES == 0])) %>%
  ungroup() %>%
  pivot_wider(names_from = Month.long, values_from = NOHA.mean)
            
  

neuston.sum.year <- neuston.1 %>%
  select(Date, Month, Month.long, Day, Year, LOCATION, SPECIES, Fish, fish_ha) %>%
  group_by(Year) %>%
  summarise(min.date = min(Date),
            max.date = max(Date),
            catch.total = sum(Fish[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                     SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                     SPECIES == 211 | SPECIES == 217 | SPECIES == 218 | SPECIES == 0]), 
            catch.min = min(Fish[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                   SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                   SPECIES == 211 | SPECIES == 217 | SPECIES == 218| SPECIES == 0]), 
            catch.max = max(Fish[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                   SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                   SPECIES == 211 | SPECIES == 217 | SPECIES == 218| SPECIES == 0]), 
            NOHA.mean = mean(fish_ha[SPECIES == 202 & Month <=7 | SPECIES == 203 & Month <=7 | 
                                       SPECIES == 204 & Month <=7 | SPECIES == 206 & Month <=7 |
                                       SPECIES == 208 & Month <=7 | SPECIES == 210 & Month <=7 |
                                       SPECIES == 211 & Month <=7 | SPECIES == 217 & Month <=7 | 
                                       SPECIES == 218 & Month <=7 | SPECIES == 0 & Month <=7]),
            NOHA.median = median(fish_ha[SPECIES == 202 & Month <=7 | SPECIES == 203 & Month <=7 | 
                                           SPECIES == 204 & Month <=7 | SPECIES == 206 & Month <=7 |
                                           SPECIES == 208 & Month <=7 | SPECIES == 210 & Month <=7 |
                                           SPECIES == 211 & Month <=7 | SPECIES == 217 & Month <=7 | 
                                           SPECIES == 218 & Month <=7 | SPECIES == 0 & Month <=7]),
            NOHA.min = min(fish_ha[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                     SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                     SPECIES == 211 | SPECIES == 217 | SPECIES == 218| SPECIES == 0]), 
            NOHA.max = max(fish_ha[SPECIES == 202 | SPECIES == 203| SPECIES == 204| 
                                     SPECIES == 206 | SPECIES == 208| SPECIES == 210|
                                     SPECIES == 211 | SPECIES == 217 | SPECIES == 218| SPECIES == 0])) %>%
  ungroup() %>%
  left_join(neuston.sites.years) %>%
  left_join(neuston.collections.years) %>%
  left_join(neuston.sum.month) %>% 
  left_join(genetics.sum) %>%
  mutate(Month.min = month(min.date),
         Month.max = month(max.date),
         Day.min = day(min.date),
         Day.max = day(max.date),
         Month.min.long = month.name[Month.min],
         Month.max.long = month.name[Month.max]) %>%
  unite(date.min, Month.min.long,Day.min, sep=' ') %>%
  unite(date.max, Month.max.long,Day.max, sep=' ') %>%
  select(Year, Sites, Collections, date.min, date.max, catch.min, catch.total, 
         NOHA.mean, NOHA.median, NOHA.max, May, June, July, August, September, October, Genetics) %>%
  mutate(across(8:16, round, 0))


##########################################################################################################
##Annual larval growth, 2014-2022
##Get  larval length data combine with neuston effort data
larval.lengths<-read.csv(here('Data','2014-2022_Neuston_lengths.csv'))

larval.lengths$SPECIES<-as.factor(larval.lengths$SPECIES) 

larval.lengths <- larval.lengths %>%
  select(OP_ID, SPECIES, LENGTH) %>%
  subset(SPECIES == '202' |
           SPECIES == '203' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '208' |
           SPECIES == '210' |
           SPECIES == '217' |
           SPECIES == '218' ) %>%
  subset(LENGTH <30) %>%
  subset(LENGTH >=5) %>%
  left_join(sci.names) %>% 
  left_join(neuston.effort)


  
########################################################################
##Larval lengths annual sum

larval.lengths.month.sum <- larval.lengths %>%
  group_by(Year, Month.long) %>%
  summarise(n = n(),
            mean.L = mean(LENGTH)) %>%
  ungroup() %>%
  select(Year, Month.long, mean.L) %>%
  pivot_wider(names_from = Month.long, values_from = mean.L) %>%
  select(Year, May, June, July) %>%
  rename(May.L = May) %>%
  rename(June.L = June) %>%
  rename(July.L = July)
  
  

larval.lengths.annual.sum <- larval.lengths %>%
  subset(Month <=7) %>%
  group_by(Year) %>%
  summarise(n.L = n(),
            mean.L = mean(LENGTH),
            median.L = median(LENGTH),
            min.L = min(LENGTH),
            max.L = max(LENGTH)) %>%
  ungroup() %>%
  left_join(larval.lengths.month.sum) %>%
  mutate(across(3:9, round, 1))

neuston.sum.year <- neuston.sum.year %>%
  left_join(larval.lengths.annual.sum)

#########################################################################################

openxlsx::write.xlsx(neuston.sum.year, here('Plots/Best','Neuston.Annual.Sum.xlsx'))

###############################################
###Annual and Monthly length ANOVA
length.aov <- larval.lengths %>%
  subset(Month <8) %>%
  select(Year, Month.long, LENGTH) %>%
  mutate(ID = row_number())

larval.month.aov <- length.aov %>% 
  subset(Year != 2021) %>%
  group_by(Year) %>%
  anova_test(LENGTH ~ Month.long) 


larval.month.tukey <- length.aov %>% 
  group_by(Year) %>%
  tukey_hsd(LENGTH ~ as.factor(Month.long))

larval.year.aov <- length.aov %>% 
  subset(Year != 2021) %>%
  group_by(Month.long) %>%
  anova_test(LENGTH ~ Year) 

larval.year.tukey <- length.aov  %>% 
  group_by(Month.long) %>%
  tukey_hsd(LENGTH ~ as.factor(Year))


############################################################################################
larval.growth.sum <- larval.lengths %>%
  group_by(Year) %>% 
  do({
    mod = lm(LENGTH ~ Jday, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  }) %>%
  mutate(Slope = round(Slope, 2),
         text1 = "mm per day") %>%
  unite("rate", Year, Slope, sep = ", ", remove = FALSE) %>%
  unite("Legend", rate, text1, sep = " ", remove = FALSE) 

larval.length.growth <- larval.lengths %>%
  left_join(larval.growth.sum) 

###############################


ggplot(subset(larval.length.growth, Month <= 7 & LENGTH >= 8), 
       aes(x=Jday, y = LENGTH, color = Legend, fill = Legend)) +
  #  geom_jitter(shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", size = 2, se = FALSE) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Mean May-July Growth Rates"),
         color=guide_legend(title="Mean May-July Growth Rates")) +
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  scale_color_brewer(palette = 'RdYlBu', direction = -1) + 
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title.align=0.5,
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.17, .7)) + 
  labs( x='Date', 
        y='Length (mm)',
        title='Lake Superior Larval Ciscoe Growth',
        subtitle='Unidentified ciscoes', 
        caption=ann_data_access) 

ggsave(here('Plots/Best','AnnualLarvalCiscoeGrowth.png'), dpi = 150, width = 40, height = 20, units = "cm")  


############################################################################################
##Larval length plots

larval.lengths <- larval.lengths %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))


ggplot(subset(larval.lengths, Month <8)) + 
  aes(x=Year, y = LENGTH) + 
  stat_summary(fun = "mean", geom = "point", fill = 'grey70', alpha = 0.2, size = 6) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 0.5) +
  scale_x_continuous(breaks = pretty_breaks(5), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(5), name = 'Mean & 95% CI total length (mm)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  facet_wrap(~Month.long, nrow=3, scales="free_y")

#  labs(caption=ann_data_access,
#       title='Lake Superior Larval Ciscoe Length',
#       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots/Best','Annual.Larval.Length.ByMonth.png'), 
       dpi = 300, width = 40, height = 40, units = "cm") 



############
##2014-2022 sampling location maps
##################
##Map with pies colored by year sampled
pie.map.months <- neuston.1 %>%
  distinct(LOCATION, Date, .keep_all = TRUE) %>%
  group_by(Month.long, LOCATION) %>% 
  summarise(Sampled = n()) %>%
  pivot_wider(names_from = Month.long, values_from = Sampled, values_fill = 0) %>%
  ungroup() %>%
  left_join(neuston.1) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  select(LOCATION, 'May', 'June', 'July', 'August', 'September', 'October', Mid.Lat.DD, Mid.Long.DD)


ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


##Sites and month sampled
ggplot(pie.map.months) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.months,
                  cols= c('May', 'June', 'July', 'August', 'September', 'October'),
                  pie_scale = 0.6,
                  legend_name = 'Month') +
  geom_point(data=Weather.sites, aes(Longitude, Latitude), shape = 2, size = 8, stroke = 2) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.15, .77),
        legend.text=element_text(size=22, family='serif')) +
  labs(x='Longitude', y='Latitude', title = 'a)')

ggsave(here('Plots/Best', 'Larval.Months.Piemap.png'), 
       height=20, width=40, dpi=300, units='cm')



###############################################################################################
##Map with pies colored by year sampled
pie.map.years <- neuston.1 %>%
  distinct(LOCATION, Year, .keep_all = TRUE) %>%
  group_by(LOCATION, Year) %>% 
  summarise(Sampled = n()) %>%
  pivot_wider(names_from = Year, values_from = Sampled, values_fill = 0) %>%
  ungroup() %>%
  left_join(neuston.1) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  select(LOCATION, '2014', '2015', '2016', '2017', '2018', '2019', '2021', '2022', Mid.Lat.DD, Mid.Long.DD)


##Sites and years sampled
ggplot(pie.map.years) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.years,
                  cols= c("2019", "2021", "2022"),
                  pie_scale = 0.6,
                  legend_name = 'Year') +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(#legend.title = element_blank(),
    legend.position = c(.13, .82),
    legend.text=element_text(size=22, family='serif')) +
  labs(x='Longitude', y='Latitude', title = 'b)')

ggsave(here('Plots/Best', 'Larval.Genetics.Years.Piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


#############################################################################
##Combine the plots into a single figure

months <-ggplot(pie.map.months) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.months,
                  cols= c('May', 'June', 'July', 'August', 'September', 'October'),
                  pie_scale = 0.6,
                  legend_name = 'Month') +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x=element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        legend.position = c(.15, .77),
        legend.text=element_text(size=22, family='serif')) +
  labs(title = 'a)')

years <- ggplot(pie.map.years) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.years,
                  cols= c("2019", "2021", "2022"),
                  pie_scale = 0.6,
                  legend_name = 'Year') +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(#legend.title = element_blank(),
    legend.position = c(.13, .82),
    legend.text=element_text(size=22, family='serif')) +
  labs(x='Longitude', y='Latitude', title = 'b)')


####Combine the two plots with Patchwork
sites <- months / years & ylab(NULL) & theme(plot.margin = margin(5.5, 5.5, 5.5, 0))

# Use the tag label as a y-axis label
wrap_elements(sites) +
  labs(tag = "Latitude") +
  theme(plot.tag = element_text(angle = 90, size=22, family='serif'),
    plot.tag.position = "left")

#ggsave(here('Plots/Best', 'Larval.Genetics.Sampling.Maps.png'), 
#       height=40, width=40, dpi=300, units='cm')


#############################################################################################################
############################################################################################
##Visualilze Grand Portage Fish
GPortage <-read_excel(data_path, sheet = 'GrandPortage') %>%
  left_join(neuston.effort) %>%
  subset(GENETICS == 202) %>%
  drop_na(GP) %>%
  select(OP_ID, SPECIMEN_NUMBER, Length, GP, Year, LOCATION, Mid.Lat.DD, Mid.Long.DD) %>%
  rowwise() %>%
  mutate(GPQ = case_when(
    GP >= 0.7 ~ 'Grand Portage strain Cisco',
    GP <0.7 ~ 'Typical Cisco'), 
    PR.Long = -89.570,
    PR.Lat = 47.999, 
    PR.distance = distm(c(Mid.Long.DD, Mid.Lat.DD), c(PR.Long, PR.Lat), fun = distGeo)) %>%
  arrange(desc(GPQ)) 
  
GPortage.n <- GPortage %>%
  group_by(GPQ) %>%
  tally()

GPortage <- GPortage %>%
  left_join(GPortage.n) %>%
  unite("label1", GPQ, sep = ", n=", n, remove = FALSE)

GPortage.sites <- GPortage %>%
  group_by(LOCATION, GPQ) %>%
  summarise(fish = n()) %>%
  left_join(GPortage) %>%
  distinct(LOCATION, GPQ, .keep_all = TRUE) %>%
  select(LOCATION, GPQ, Mid.Lat.DD, Mid.Long.DD, fish) %>%
  ungroup() %>%
  left_join(GPortage.n) %>%
  unite("label1", GPQ, sep = ", n=", n, remove = FALSE) %>%
  arrange(desc(GPQ)) 


###################################################################################################
##Sites GPortage flavoring
ggplot(GPortage, aes(PR.distance/1000, GP)) +
  geom_jitter(shape = 21, size=6, fill = 'violet', alpha = 0.8) +
  theme_bw() +
  plot_theme +
  geom_hline(yintercept = 0.7, size = 1.5, color = 'black') +
  labs(x='Distance from Pigeon River inlet, USA/Canada (km)', y='Grand Portage genetic flavoring', 
       title = 'Lake Superior Grand Portage Influenced Cisco Larvae',
       subtitle = 'Values > 0.7 are considered to be pure Grand Portage strain C. artedi')

ggsave(here('Plots/Best', '2021_2022.Larval.GrandPortage.Distance.png'), 
       height=20, width=40, dpi=300, units='cm')



##Sites GPortage flavoring
ggplot(GPortage, aes(PR.distance/1000, Length, fill = label1)) +
  geom_jitter(shape = 21, size=6) +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_bw() +
  plot_theme +
  theme(legend.title = element_blank(),
    legend.position = c(.85, .9),
    legend.text=element_text(size=18, family='serif')) +
  labs(x='Distance from Pigeon River inlet, USA/Canada (km)', y='Total length (mm)', 
       title = 'Lake Superior Grand Portage Influenced Cisco Larvae',
       subtitle = 'Grand Portage strain Cisco had Q-scores > 0.7')

ggsave(here('Plots/Best', '2021_2022.Larval.GPortage.Length.Distance.png'), 
       height=20, width=40, dpi=300, units='cm')

##Sites GPortage flavoring
ggplot(GPortage, aes(PR.distance/1000, Length, fill = GPQ)) +
  geom_jitter(shape = 21, size=6) +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_bw() +
  plot_theme +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .9),
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Distance from Pigeon River inlet, USA/Canada (km)', y='Total length (mm)', 
       title = 'Lake Superior Grand Portage Influenced Cisco Larvae',
       subtitle = 'Grand Portage strain Cisco had Q-scores > 0.7') +
  facet_wrap(~Year, nrow=2)

ggsave(here('Plots/Best', '2021_2022.Larval.GrandPortage.Length.Distance.png'), 
       height=20, width=40, dpi=300, units='cm')



##Grand Portage strain Locations
ggplot() +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(data=subset(GPortage.sites, GPQ == 'Grand Portage strain Cisco'),
             aes(x=Mid.Long.DD, y=Mid.Lat.DD, size = fish, fill = label1), 
             shape = 21, fill = 'violet', alpha = 0.8) +
  scale_size_continuous(range = c(6,12), name='GP strain\n  Cisco') +
  scale_fill_continuous(name='GP strain\n  Cisco') +
  geom_segment(aes(x=-90, y = 48.5, xend = -89.570, yend = 47.999), size = 1) +
  ##, arrow = arrow(length = unit(0.2, "inches"))) + 
  geom_text(aes(x = -89.9, y = 48.7), 
            label = "Pigeon River\nGrand Portage", 
            size = 8, family='serif', colour = "black") + 
  theme_bw() +
  map_theme +
  theme(#legend.title = element_blank(),
    legend.position = c(.1, .7),
    legend.text=element_text(size=22, family='serif')) +
  labs(x='Longitude', y='Latitude', 
       title = 'Lake Superior Grand Portage Influenced Cisco Larvae Collection Locations',
       subtitle = 'Grand Portage strain Cisco had Q-scores > 0.7') 

ggsave(here('Plots/Best', '2021_2022.GPortage.Locations.png'), 
       height=20, width=40, dpi=300, units='cm')


ggplot() +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(data=GPortage.sites,
             aes(x=Mid.Long.DD, y=Mid.Lat.DD, fill = label1, size = fish), 
             shape = 21, stroke = 1.1) +
  scale_fill_manual(values = c('violet', 'grey90'), name = 'Cisco') + 
  scale_size_continuous(range = c(2,12), name = 'Cisco') +
  guides(size = guide_legend(order=1),
         fill = guide_legend(override.aes = list(size=10))) +
  geom_segment(aes(x=-89.5, y = 48.7, xend = -89.570, yend = 47.999), size = 1) +
  geom_text(aes(x = -89.2, y = 48.9), 
            label = "Pigeon River\nGrand Portage", 
            size = 8, family='serif', colour = "black") + 
  theme_bw() +
  map_theme +
  theme(legend.title = element_blank(),
    legend.position = c(.17, .78),
    legend.text=element_text(size=22, family='serif')) +
  labs(x='Longitude', y='Latitude', 
       title = 'Lake Superior Grand Portage Influenced Cisco Larvae Collection Locations',
       subtitle = 'Grand Portage strain Cisco had Q-scores > 0.7') 

ggsave(here('Plots/Best', '2021_2022.GPQ.Locations.png'), 
       height=20, width=40, dpi=300, units='cm')


########################################################################################

##################################################################################################################################
###################################WATER TEMPERATURE
##################################################################################################################################

##Data from https://coastwatch.glerl.noaa.gov/statistic/statistic.html 
##need to download data for current year and append to Excel file

glerl.data<-read_xlsx(here('Data','LS_GLERL_WTemp.xlsx'), sheet = 'GLERL')

##change date format into usable form
glerl.data$date<-as.character(glerl.data$date)
glerl.data$date<-ymd(glerl.data$date)

glerl.data <- glerl.data %>%
  select(date, Superior) %>%
  mutate(Day = day(date),
         Week = week(date), 
         Month = month(date),
         Month.short = month.abb[Month],
         Month.long = month.name[Month],
         Jday = yday(date), 
         Year = year(date), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, Day, sep=" ", remove = FALSE) %>%
  rename(SurfTemp.daily = Superior) %>% 
  group_by(Year) %>%
  mutate(SurfTemp.cumulative = cumsum(SurfTemp.daily)) 

########################################################
glerl.data <- glerl.data %>%
  mutate(across(Month.long, factor, levels = c("January", "February", "March", "April",
                                               "May", "June", "July", "August", "September",
                                               "October", "November", "December")))

ggplot(subset(glerl.data, Year >= 2014)) + 
  aes(x=Year, y = SurfTemp.daily) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI water temperature (C)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  #  labs(caption=ann_data_access) +
  #  title='Lake Superior Larval Ciscoe Abundance',
  #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=4, scales="free_y")

ggsave(here('Plots/Best','WaterTemp.png'), dpi = 150, width = 40, height = 40, units = "cm")  




glerl.sum <- glerl.data %>%
  select(Date, SurfTemp.daily, SurfTemp.cumulative)

glerl.month <- glerl.sum %>%
  mutate(Year = year(Date), 
         Month = month(Date),
         Month.long = month.name[Month]) %>%
  subset(Year >= 2014) %>% 
  group_by(Year, Month) %>%
  summarise(SurfT.min = min(SurfTemp.daily, na.rm=TRUE),
            SurfT.max = max(SurfTemp.daily, na.rm=TRUE),
            SurfT.mean = mean(SurfTemp.daily, na.rm=TRUE),
            SurfDD.min = min(SurfTemp.cumulative, na.rm=TRUE),
            SurfDD.max = max(SurfTemp.cumulative, na.rm=TRUE),
            SurfDD.mean = mean(SurfTemp.cumulative, na.rm=TRUE)) %>%
  ungroup()


neuston.DD <- neuston.1 %>%
  left_join(glerl.sum)
  

ggplot(subset(glerl.data, Year >= 2014 & Month <9)) +
  geom_line(aes(x=as.Date(Jday, origin = as.Date("2014-01-01")), y=SurfTemp.cumulative, 
                color = as.factor(Year)), size=2) +
  scale_x_date(date_labels = "%b", breaks=pretty_breaks(12)) +
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_brewer(palette = 'RdYlBu') +
  theme_bw()  +
  plot_theme +
  labs( #title='Lake Superior Daily Surface Water Temperature', 
        #caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html', 
        x='Date',y='Cumulative degree days') +
  theme(legend.title = element_blank(), 
        legend.position= c(0.1, 0.6)) 

#+
#  facet_grid(~Month.long)

ggsave(here('Plots/Best', 'Annual.DegreeDays.png'), 
       height=20, width=40, dpi=300, units='cm')


##################################################################

neuston.sum.month.long <- neuston.sum.month %>%
  pivot_longer(2:7, names_to = "Month.long", values_to = "NOHA.mean")

larval.lengths.month.sum.long <- larval.lengths %>%
  group_by(Year, Month.long) %>%
  summarise(n = n(),
            mean.L = mean(LENGTH)) %>%
  ungroup() %>%
  select(Year, Month.long, mean.L) 

glerl.monthly.DD <- glerl.data %>%
  group_by(Year, Month.long) %>%
  summarise(Max.DD = max(SurfTemp.cumulative)) %>%
  ungroup() %>%
  left_join(neuston.sum.month.long) %>%
  left_join(larval.lengths.month.sum.long) %>%
  drop_na(NOHA.mean, mean.L)

#################################
larval.growth.DD <- lm(glerl.monthly.DD$mean.L  ~ I(glerl.monthly.DD$Max.DD)) 
summary(larval.growth.DD)


#############################################################################################
##Length and degree days 

larval.lengths.DD <- larval.lengths %>%
  left_join(glerl.sum) %>%
  group_by(Date) %>%
  summarise(mean.L = mean(LENGTH),
            mean.DD = mean(SurfTemp.cumulative),
            mean.T = mean(SurfTemp.daily)) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         Month.long = month.name[Month])


larval.length.DD <- lm(larval.lengths.DD$mean.L  ~ I(larval.lengths.DD$mean.DD)) 
summary(larval.length.DD)

larval.lengths.DD <- larval.lengths.DD %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))

ggplot(subset(larval.lengths.DD, Month.long == "May" | Month.long == "June" | 
                Month.long == 'July' )) + 
  ## | Month.long == 'August' | Month.long == 'September')) +
  geom_point(aes(x=mean.DD, y= mean.L, fill = Month.long), shape = 21, size=6) +
  geom_smooth(aes(x=mean.DD, y= mean.L), method = "lm", color = 'black', size = 2, se = FALSE) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks())+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  theme_bw() +
  plot_theme +
  labs(#title='Lake Superior Daily Surface Water Temperature', 
    #caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html', 
    x='Accumulated degree days', y='Mean length (mm)') +
  theme(legend.title = element_blank(), 
        legend.position= c(0.1, 0.8)) 

ggsave(here('Plots/Best', 'Larval.length.All.DD.png'), 
       height=20, width=40, dpi=300, units='cm')




glerl.monthly.DD <- glerl.monthly.DD %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))

ggplot(subset(glerl.monthly.DD, Month.long == "May" | Month.long == "June" | 
                Month.long == 'July' )) + 
  ## | Month.long == 'August' | Month.long == 'September')) +
  geom_point(aes(x=Max.DD, y= mean.L, fill = Month.long), shape=21, size=6) +
  geom_smooth(aes(x=Max.DD, y= mean.L), method = "lm", color = 'black', size = 2, se = FALSE) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks())+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  theme_bw() +
  plot_theme +
  labs(#title='Lake Superior Daily Surface Water Temperature', 
    #caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html', 
    x='Accumulated degree days', y='Mean length (mm)') +
  theme(legend.title = element_blank(), 
        legend.position= c(0.1, 0.8)) 

ggsave(here('Plots/Best', 'Larval.length.DD.png'), 
       height=20, width=40, dpi=300, units='cm')



ggplot(subset(glerl.monthly.DD, Month.long == "May" | Month.long == "June" | 
                Month.long == 'July' )) + 
  ## | Month.long == 'August' | Month.long == 'September')) +
  geom_point(aes(x=Max.DD, y= NOHA.mean, fill = Month.long), shape=21, size=6) +
  geom_smooth(aes(x=Max.DD, y= NOHA.mean), method = "lm", size = 2, se = FALSE) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks())+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  theme_bw() +
  plot_theme +
  labs(#title='Lake Superior Daily Surface Water Temperature', 
    #caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html', 
    x='Accumulated degree days', y='Mean larval abundance (fish/ha)') +
  theme(legend.title = element_blank(), 
        legend.position= c(0.8, 0.8)) 

ggsave(here('Plots/Best', 'Larval.density.DD.png'), 
       height=20, width=40, dpi=300, units='cm')




#################################################################################################################
#################################################################################################################
##regression tree-----------------------------------------------------------------------regression tree-----


larval.month <- glerl.monthly.DD %>%
  select(Year, Month.long, NOHA.mean) %>%
  pivot_wider(names_from = Month.long, values_from = NOHA.mean) %>%
  select(Year, May, June, July)

larval.model <- Weather.month %>%
  left_join(ice.month) %>%
  left_join(glerl.month) %>%
  mutate(Month.long = month.name[Month]) %>%
  subset(Month < 8) %>%
  select(Year, Month.long, ice.max, 
         WSPD.mean, 
         AirT.mean,
         SurfT.mean,
         SurfDD.max) %>%
  pivot_wider(names_from = Month.long, values_from = c(Month.long, ice.mean, 
                                                       WSPD.mean, 
                                                       AirT.mean,
                                                       SurfT.mean,
                                                       SurfDD.max)) %>%
  select(1,9:13, 16:43) %>%
  left_join(larval.month)
  

##########################################################################################
treedata <- larval.model %>%
  subset(Year != 2020) %>%
#  subset(Year != 2021) %>%
  select(-Year) 
#  select(-June) %>%
#  select(-July)

%>%
  mutate(recruits = case_when(
    July > 200 ~ 'High',
    July > 100 & July < 200 ~ 'Medium',
    July < 100 ~ 'Low')) %>%
  select(-recruits)

rp1<-rpart(July~., data=treedata, control=rpart.control(minbucket=1), method='anova')
rpart.plot(rp1,fallen.leaves=TRUE, tweak=1, space=0, extra="auto", box.palette='auto', type=2, digits=1)


rp1<-rpart(July~., data=treedata, control=rpart.control(minbucket=2), method='anova') ##play with minbucket value, changes tree a lot
rpart.plot(rp1,fallen.leaves=TRUE, tweak=1, space=0, extra=106, box.palette='auto', type=2, digits=1)
plotcp(rp1, upper='splits')
prune(rp1, cp=0.1)
printcp(rp1)
summary(rp1, cp=0.1)
xpred.rpart(rp1, xval=500, return.all=FALSE)

#################################################################################################################
#################################################################################################################

#################################################################
##Larval length and surface temperature
##########################################################################################################
#######################################################################
##Larval density and surface temp
larval.lengths.sum <- larval.lengths %>%
  subset(LENGTH <30) %>%
  subset(LENGTH >=8) %>%
  group_by(Date, LOCATION, SPECIES) %>%
  summarise(mean.L = mean(LENGTH)) %>%
  ungroup() %>% 
  left_join(neuston.effort) %>%
  left_join(sci.names) 

larval.lengths.sum <- larval.lengths.sum %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))

ggplot()  +
  geom_point(data=subset(larval.lengths.sum, Month <=7 & mean.L <25), 
             aes(TSurface, mean.L, fill=Month.long), shape = 21, size=5) +
  geom_smooth(data=subset(larval.lengths.sum, Month <=7 & mean.L <25), 
              aes(TSurface, mean.L, color=Month.long), method = "lm", size = 2, se = FALSE) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Mean length (mm)')+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Surface temperature (C)')+
  scale_fill_manual(values = c('blue', 'skyblue', 'darkorange')) + 
  scale_color_manual(values = c('blue', 'skyblue', 'darkorange')) + 
  guides(fill= guide_legend(title='Month'), 
         color = guide_legend(title='Month')) +
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.1,0.85)) 

ggsave(here('Plots/Best', 'Larval.MeanLength.SurfaceT.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################


####################################################################################################
##Larval Density Plots all YEARS
#####################################################################################################
##map with stations color coded by density
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

my_breaks = c(1, 100, 1000, 25000, 75000)

ggplot(neuston.1, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=neuston.1, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=fish_ha), size=2, stroke=0.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Fish per ha', trans = "log", 
                       breaks = my_breaks, labels=my_breaks,
                       guide = guide_colourbar(
                         direction = "horizontal", 
                         title.position = "top"))+
  map_theme+
  ##  geom_text(aes(label=LOCATION))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Larval Ciscoe Density',
        subtitle='Collections made May-July for the years 2014-2023',
        caption=ann_data_access) +
  
  theme(legend.key.width = unit(1, "cm"),
        legend.position=c(0.82, 0.13), 
        legend.text=element_text(size=16, family='serif'), 
        legend.title=element_text(size=16, family='serif'),
        axis.title=element_text(size=16, family='serif'),
        axis.text=element_text(size=16, family='serif')) +
  #  geom_text(aes(label=LOCATION))+
  facet_wrap(~Year) 

ggsave(here('Plots/Best', 'LS_CiscoeLarvaeDensity_AllYears.png'), 
       dpi = 300, width = 25, height = 15, units = "cm")


###################################################################
##average densities across all years
neuston.months <- neuston.1 %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  select(LOCATION, Month.long)

larval.station.means <- neuston.1 %>%
  select(Date, Month, Month.long, Date, Jday, Year, TARGET, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, ShoreDistance, 
         SPECIES, Fish, fish_ha) %>%
  subset(SPECIES == 202 & Month <=7 | SPECIES == 203 & Month <=7 | 
           SPECIES == 204 & Month <=7 | SPECIES == 206 & Month <=7 |
           SPECIES == 208 & Month <=7 | SPECIES == 210 & Month <=7 | 
           SPECIES == 211 & Month <=7 | SPECIES == 217 & Month <=7 | 
           SPECIES == 218 & Month <=7 | SPECIES == 0 & Month <=7) %>%
  group_by(LOCATION) %>%
  summarise(Jday.mean = mean(Jday),
            Jday.median = median(Jday),
            Jday.min = min(Jday),
            Jday.max = max(Jday), 
            Jday.range = Jday.max-Jday.min,
            Mid.Lat.DD = mean(Mid.Lat.DD), 
            Mid.Long.DD = mean(Mid.Long.DD),
            ShoreDistance = mean(ShoreDistance), 
            Mid.Depth = mean(Mid.Depth), 
            fish_ha = mean(fish_ha),
            samples = n()) %>% 
  ungroup()  %>%
  left_join(neuston.months) %>% 
  subset(samples >=2) 
  
##Sampling date summary
larval.station.dates <- larval.station.means %>%
  summarise(Jday.range.mean = mean(Jday.range),
            Jday.range.median = median(Jday.range))
            

##map with stations color coded by density
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(larval.station.means, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Latitude')+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Longitude')+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=larval.station.means, aes(Mid.Long.DD, Mid.Lat.DD, size=fish_ha, fill=fish_ha), 
             shape=21) +
  geom_point(data=subset(larval.station.means, fish_ha <=0.5), aes(Mid.Long.DD, Mid.Lat.DD), 
             size=2, fill='black', shape=21) +
  theme_bw() +
  map_theme  +
  theme(legend.position=c(0.13,0.7)) +
  scale_size_continuous(range = c(2, 12), breaks=pretty_breaks(6)) +
  scale_fill_distiller(direction = -1, palette="RdYlBu", breaks=pretty_breaks(6)) +
  guides(fill= guide_legend(title='Fish per ha'), 
         size = guide_legend(title='Fish per ha')) 

ggsave(here('Plots/Best', 'LarvalDensity.MeanAllYears.Map.png'), dpi = 300, width = 40, height = 20, units = "cm")



###################################################################################################################
##Larval density and distance from shore

neuston.1 <- neuston.1 %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September", "October")))

larval.station.means <- larval.station.means %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September", "October")))


ggplot(neuston.1) +
  geom_point(aes(ShoreDistance/1000, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits=c(0,5000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Distance from shore (km)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.85,0.7)) 

ggsave(here('Plots/Best', 'Larval.Density.AllYears.ShoreDistance.png'), dpi = 300, width = 40, height = 20, units = "cm")


larval.density.shore.all <- ggplot(larval.station.means) +
  geom_point(aes(ShoreDistance/1000, fish_ha, fill=Month.long), shape = 21, size=4) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha') +
  scale_x_continuous(breaks=pretty_breaks(), name = 'Distance from shore (km)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.85,0.7),
        axis.text=element_text(size=22, family='serif'),
        axis.title=element_text(size=22, family='serif'),
        legend.text=element_text(size=22, family='serif'),
        legend.title=element_text(size=22, family='serif')) 

ggplot(larval.station.means) +
  geom_point(aes(ShoreDistance/1000, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits = c(0,2000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Distance from shore (km)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  annotation_custom(ggplotGrob(larval.density.shore.all), ymin=850, ymax=2100, xmax=51, xmin=16)+
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position="none") 

ggsave(here('Plots/Best', 'Larval.Density.MeanAllYears.ShoreDistance.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################
##Larval density and surface temp

ggplot(neuston.1) +
  geom_point(aes(TSurface, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits = c(0,2000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Surface temperature (C)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.9,0.75)) 

ggsave(here('Plots/Best', 'Larval.Density.MeanAllYears.SurfaceT.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################
##########################################################################################################
##Larval density and Chla

ggplot(neuston.1) +
  geom_point(aes(Chl3m, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits = c(0,2000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Surface chlorophyl a')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.85,0.7)) 

ggsave(here('Plots/Best', 'Larval.Density.MeanAllYears.SurfaceChla.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################
##########################################################################################################
##Larval density and depth, all data

ggplot(neuston.1) +
  geom_point(aes(Mid.Depth, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits = c(0,10000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Bathymetric depth (m)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position=c(0.85,0.7)) 

ggsave(here('Plots/Best', 'Larval.Density.Data.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################
##########################################################################################################
##Larval density and depth, station means

ggplot(larval.station.means) +
  geom_point(aes(Mid.Depth, fish_ha, fill=Month.long), shape = 21, size=5) +
  scale_y_continuous(breaks=pretty_breaks(), name = 'Fish per ha', limits = c(0,10000))+
  scale_x_continuous(breaks=pretty_breaks(), name = 'Bathymetric depth (m)')+
  scale_fill_brewer(palette = 'RdYlBu', direction = -1) + 
  theme_bw() +
  plot_theme +
  guides(fill= guide_legend(title='Month')) +
  theme(legend.position="none") 

ggsave(here('Plots/Best', 'Larval.Density.MeanAllYears.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm")

##########################################################################################################
######################################################################################################################
##Larval densities by year for nearshore and offshore - no shallow CSMI sites
neuston.ciscoe.densities.noCSMI <- neuston.ciscoe.densities %>%
  subset(TARGET==2 | TARGET==118 | TARGET==117 & Year>2013 & Mid.Depth >80) 

ggplot(subset(neuston.ciscoe.densities.noCSMI, Month <8)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'olivedrab', alpha = .7) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  geom_text(aes(x = 2020, y = 200), label = "No\ndata", size = 8, family='serif', colour = "black") + 
  geom_text(aes(x = 2021, y = 200), label = "No\ndata", size = 8, family='serif', colour = "black") + 
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots/Best','Annual.NoCSMI.Larval.Densities.png'), 
       dpi = 300, width = 40, height = 20, units = "cm") 

neuston.ciscoe.densities.noCSMI <- neuston.ciscoe.densities.noCSMI %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))


ggplot(subset(neuston.ciscoe.densities.noCSMI, Month <8)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'gray40', alpha = .7) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
#  geom_text(aes(x = 2020, y = 300), label = "No\ncollections", size = 7, family='serif', colour = "black") + 
#  geom_text(aes(x = 2020.51, y = 100), label = "No\ncollections", size = 7, family='serif', colour = "black") + 
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
#  labs(caption=ann_data_access) +
     #  title='Lake Superior Larval Ciscoe Abundance',
    #   subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=3, scales="free_y")

ggsave(here('Plots/Best','Annual.NoCSMI.Larval.Densities.ByMonth.png'), 
       dpi = 300, width = 40, height = 40, units = "cm") 


############################################################################################
### ANOVA for month and year
fish.aov <- neuston.1 %>%
  subset(TARGET==2 | TARGET==118 | TARGET==117 & Year>2013 & Mid.Depth >80) %>%
  subset(Month <= 7) %>%
  select(Year, Month.long, fish_ha) %>%
#  drop_na() %>%
  mutate(ID = row_number())

fit.month <- fish.aov %>% 
  group_by(Year) %>%
  anova_test(fish_ha ~ Month.long) 


fit.month.tukey <- fish.aov %>% 
  group_by(Year) %>%
  tukey_hsd(fish_ha ~ as.factor(Month.long))

fit.year <- fish.aov %>% 
  group_by(Month.long) %>%
  anova_test(fish_ha ~ Year) 

fit.year.tukey <- fish.aov  %>% 
  group_by(Month.long) %>%
  tukey_hsd(fish_ha ~ as.factor(Year))

############################################################################################

###Annual summary table
neuston.ciscoe.annual <- neuston.ciscoe.densities.noCSMI %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  subset(Month <8) %>%
  group_by(Year) %>%
  summarise(n = n(), 
            FishSum = sum(Fish), 
            FishMean = mean(fish_ha)) %>%
  ungroup()

###Annual summary table
neuston.ciscoe.month <- neuston.ciscoe.densities.noCSMI %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  subset(Month <8) %>%
  group_by(Month) %>%
  summarise(n = n(), 
            FishSum = sum(Fish), 
            FishMean = mean(fish_ha)) %>%
  ungroup()

#########################################################################################################
##load the raw RVCAT effort data file to get effort data
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
trawl.data<-read.csv(here('Data','RVCAT.csv'))
trawl.data$SPECIES<-as.factor(trawl.data$SPECIES) 

##change date format into usable form
trawl.data$OP_DATE<-as.character(trawl.data$OP_DATE)
trawl.data$OP_DATE<-parse_date(trawl.data$OP_DATE, format='%d-%b-%y')

trawl.data <- trawl.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Month.short = month.abb[Month],
         Month.long = month.name[Month],
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, Day, sep=" ", remove = FALSE) %>%
  left_join(ShoreDistance) %>%
  left_join(sci.names)

trawl.data[is.na(trawl.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- trawl.data[is.na(trawl.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
trawl.data[is.na(trawl.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- trawl.data[is.na(trawl.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

trawl.data[is.na(trawl.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- trawl.data[is.na(trawl.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
trawl.data[is.na(trawl.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- trawl.data[is.na(trawl.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

trawl.data$Mid.Lat.DD<-(trawl.data$BEG_LATITUDE_DD+trawl.data$END_LATITUDE_DD)/2
trawl.data$Mid.Long.DD<-(trawl.data$BEG_LONGITUDE_DD+trawl.data$END_LONGITUDE_DD)/2

##Replace zeros which are known to be wrong to na for all variables other than SPECIES, NUM, WEIGHT
trawl.data$TIME <- replace(trawl.data$TIME, trawl.data$TIME == 0, NA)             
trawl.data$TOW_TIME <- replace(trawl.data$TOW_TIME, trawl.data$TOW_TIME == 0, NA)             
trawl.data$FISHING_DEPTH <- replace(trawl.data$FISHING_DEPTH, trawl.data$FISHING_DEPTH == 0, NA)
trawl.data$SURF_TEMP <- replace(trawl.data$SURF_TEMP, trawl.data$SURF_TEMP == 0, NA)
trawl.data$BEG_SURF <- replace(trawl.data$BEG_SURF, trawl.data$BEG_SURF == 0, NA)
trawl.data$END_SURF <- replace(trawl.data$END_SURF, trawl.data$END_SURF == 0, NA)
trawl.data$BEG_BOTTOM <- replace(trawl.data$BEG_BOTTOM, trawl.data$BEG_BOTTOM == 0, NA)
trawl.data$END_BOTTOM <- replace(trawl.data$END_BOTTOM, trawl.data$END_BOTTOM == 0, NA)

trawl.data$STATE <- replace(trawl.data$STATE, trawl.data$STATE == 'E', 'ONT.E')
trawl.data$STATE <- replace(trawl.data$STATE, trawl.data$STATE == 'W', 'ONT.W')
trawl.data$STATE <- str_trim(trawl.data$STATE)
trawl.data$M_UNIT <- str_trim(trawl.data$M_UNIT)

##calculate the mid-point (average) trawl depth
trawl.data$Mid.Depth <- rowMeans(trawl.data[,c("BEG_DEPTH","END_DEPTH")], na.rm=TRUE)

##Calculate an average Surface and bottom temp
trawl.data$Surface.Temp <- rowMeans(trawl.data[,c("SURF_TEMP", "BEG_SURF", "END_SURF")], na.rm = TRUE)
trawl.data$Bottom.Temp <- rowMeans(trawl.data[,c("BEG_BOTTOM", "END_BOTTOM", "TEMP_BEG_BOTTOM", "TEMP_END_BOTTOM")], na.rm = TRUE)


##Replace FISHING_DEPTH with END_DEPTH for bottom trawls and keep as is for mid-water trawls
trawl.data <- trawl.data %>%
  mutate(FISHING_DEPTH = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ Mid.Depth, 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ FISHING_DEPTH))

##add country based on states
trawl.data <- trawl.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'ONT.E'  ~ "Canada",
    STATE == 'ONT.W'  ~ "Canada"))


trawl.effort <- trawl.data %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ 'Bottom trawl', 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ 'Mid-water trawl')) %>%
  select(OP_ID, TARGET, TIME, Date, Date1, Day, Week, Month, Month.short, Month.long, Year, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, FISHING_DEPTH, ShoreDistance, Gear) %>%
  distinct(OP_ID, .keep_all = TRUE) 


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

##############################################################################################################################
## Get all genetics data and Join to neuston effort and trawl effort
##Join the two effort table 

neuston.effort <- neuston.effort %>%
  select(OP_ID, TARGET, TIME, Date, Date1, Day, 
         Week, Month, Month.short, Month.long, Year, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, 
         Mid.Depth, Fishing.Depth, ShoreDistance) %>%
         mutate(Gear = 'Neuston') %>%
  distinct(OP_ID, .keep_all=TRUE)

trawl.effort <- trawl.effort %>%
  select(OP_ID, TARGET, TIME, Date, Date1, Day, 
         Week, Month, Month.short, Month.long, Year, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, 
         Mid.Depth, FISHING_DEPTH, ShoreDistance, Gear)

effort <- trawl.effort %>%
  rename(Fishing.Depth = FISHING_DEPTH) %>%
  bind_rows(neuston.effort)


###Get genetics data
data_path <- here('Data', 'genetics.xlsx')
genetics <-read_excel(data_path, sheet = 'Genomics')

genetics <- genetics %>%
  mutate(Match = case_when(
    SPECIES == GENETICS  ~ "Yes", 
    SPECIES != GENETICS  ~ "No"))

##Make SPECIES and GENETICS factors
genetics$SPECIES<-as.factor(genetics$SPECIES)
genetics$GENETICS<-as.factor(genetics$GENETICS)

genetics <- genetics %>%
  left_join(effort) %>%
  select(-Weight) %>%
  left_join(sci.names) %>%
  mutate(GENETICS_NAME = case_when(
    GENETICS == 202 ~ "Cisco",
    GENETICS == 203 ~ "Lake Whitefish",
    GENETICS == 204 ~ "Bloater",
    GENETICS == 206 ~ "Kiyi",
    GENETICS == 208 ~ "Shortnose Cisco", 
    GENETICS == 210 ~ "Pygmy Whitefish",
    GENETICS == 217 ~ "unidentified ciscoe",
    GENETICS == 218 ~ "ciscoe hybrid")) %>%
  drop_na(GENETICS)



#################################################
##Plot some data!

#Includes Unknowns, putative hybrids
ggplot(subset(genetics, GENETICS != 217 & Length <= 100), aes(x=ShoreDistance/1000, y=Length, fill = GENETICS_NAME)) +
  geom_jitter(shape = 21, size = 5) +
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme(legend.title = element_blank(), 
        legend.direction = 'horizontal',
        legend.position = 'bottom') +
  labs(y='Length (mm)', x='Distance from shore (km)',
       title='2022 Lake Superior Ciscoe Spatial Distribution',
       subtitle='Identifications based on genomics', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks(6)) 

ggsave(here('Plots/Best','Genetics.Age0.Ciscoe.DistanceShore.png'), 
       dpi = 150, width = 40, height = 20, units = "cm")  


ggplot(subset(genetics, GENETICS != 217 & Length <= 100), aes(x=ShoreDistance/1000, y=Length)) +
  geom_jitter(shape = 21, size = 5, fill = 'violet', alpha = 0.6) +
  plot_theme+
  theme(legend.title = element_blank(), 
        legend.direction = 'horizontal',
        legend.position = 'bottom') +
  labs(y='Length (mm)', x='Distance from shore (km)',
       title='2022 Lake Superior Ciscoe Spatial Distribution',
       subtitle='Identifications based on genomics', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks(6)) +
  facet_wrap(.~GENETICS_NAME, nrow=3) 

ggsave(here('Plots/Best','Genetics.Age0.Ciscoe.DistanceShore.Facet.png'), 
       dpi = 150, width = 40, height = 20, units = "cm")  

###################################################################################################################
##Study site map and then species pie maps
##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


## Pre-genetic larval samples collected in 2022
genetics.sites <- genetics %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  select(OP_ID, LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth)


collections.2022.larval <- neuston.1 %>%
  subset(Year == 2022) %>%
  subset(SPECIES == 217 |
           SPECIES == 0) %>%
  group_by(Date, LOCATION) %>% 
  summarise(Fish = sum(Fish)) %>%
  left_join(neuston.1) %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(Gear = 'Neuston',
         Year = year(Date)) %>%
  select(LOCATION, Date, Year, Gear, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Fish)

genetics.age0 <- genetics %>%
  subset(Gear != 'Neuston') %>%
  subset(Length <= 150) %>%
  group_by(Date, LOCATION) %>%
  summarise(Fish = n()) %>%
  left_join(genetics.sites) %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  mutate(Month = month(Date),
         Year = year(Date), 
         Gear = case_when(
    Month <= 7 ~ 'Bottom trawl', 
    Month >7 ~ 'Mid-Water trawl')) %>%
  select(LOCATION, Date, Year, Gear, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Fish)
  

collections <- collections.2022.larval %>%
  bind_rows(genetics.age0) %>%
  mutate(FishType = case_when(
    Gear == 'Neuston' ~ 'Larval',
    Gear != 'Neuston' ~ 'Age-0+'))


collections.2022 <- collections %>%
  subset(Year == 2022)
  

###Sample location maps
##larval collections
ggplot(collections.2022.larval) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(data=subset(collections.2022.larval, Fish>=1), mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Fish), 
              shape = 21, fill = 'violet', alpha = 0.6) +  
 geom_jitter(data=subset(collections.2022.larval, Fish==0), mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
               shape = 21, color = 'black', size = 4) +  
  theme_bw() +
  map_theme +
  scale_size_continuous(range = c(0, 15), breaks=pretty_breaks(8), name = 'Fish collected') +
  theme(legend.position = c(.12, .7),
        legend.title=element_text(size=18, family='serif'), 
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Larval Ciscoe Collections',
       subtitle='Samples collected 2022', 
       caption=ann_data_access) 

ggsave(here('Plots/Best', '2022.Larval.Sample.Locations.png'), 
       height=20, width=40, dpi=300, units='cm')


##larval and age-0 collections
ggplot(collections.2022) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(data=subset(collections.2022, Fish>=1), mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Fish, fill = FishType), 
              shape = 21) +  
  geom_jitter(data=subset(collections.2022, Fish==0), mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
              shape = 21, color = 'black', size = 4) +  
  theme_bw() +
  map_theme +
  guides(color = guide_legend(override.aes = list(size=10))) +   
  scale_size_continuous(range = c(0, 15), breaks=pretty_breaks(8), name = 'Fish collected') +
  scale_fill_brewer(palette="Pastel1", name = 'Life stage',
                    guide = guide_legend(override.aes = list(size=10))) + 
  theme(legend.position = c(.07, .62),
        legend.title=element_text(size=18, family='serif'), 
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Ciscoe Genomic Collections',
       caption=ann_data_access) 

ggsave(here('Plots/Best', '2022.Genomics.Sample.Locations.png'), 
       height=20, width=40, dpi=300, units='cm')



###General Boring Sample location map
ggplot(collections) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(data=subset(collections.2022, Fish>=1), 
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Fish), 
              shape = 21, fill = 'violet', alpha = 0.6) +  
  geom_jitter(data=subset(collections.2022, Fish==0), mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
              shape = 21, color = 'black', size = 4) +  
  theme_bw() +
  map_theme +
  scale_size_continuous(range = c(0, 10), breaks=pretty_breaks(8), name = 'Fish collected') +
  theme(legend.position = c(.12, .75),
        legend.title=element_text(size=18, family='serif'), 
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Ciscoe Genomic Collections',
       subtitle='Samples collected 2019-2022', 
       caption=ann_data_access) 

ggsave(here('Plots/Best', 'Genetics.Sample.Locations.png'), 
       height=20, width=40, dpi=300, units='cm')




####Species distribution pie map

pie.map.fish <- genetics %>%
  group_by(OP_ID, GENETICS_NAME) %>%
  tally() %>%
  pivot_wider(names_from = GENETICS_NAME, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(effort) 

##All fish
ggplot(pie.map.fish) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.fish,
                  cols= c("Bloater", "Cisco", "Kiyi", "Shortnose Cisco"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.2, .8),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Larval and Age-0 Ciscoe Occurrences', 
       subtitle='Samples collected 2019-2022', 
       caption=ann_data_access)  

ggsave(here('Plots/Best', 'Genetics.Piemap.png'), 
       height=20, width=40, dpi=300, units='cm')

###By Month
pie.map.fish$Month.long <- factor(pie.map.fish$Month.long,
                                  levels = c("May", "June", "July", 
                                  "August", "September", "October"))

ggplot(pie.map.fish) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.fish,
                  cols= c("Bloater", "Cisco", "Kiyi", "Shortnose Cisco"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.5, .9),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Larval and Age-0 Ciscoe Occurrences', 
       subtitle='Samples collected 2019-2022', 
       caption=ann_data_access)  +
  facet_wrap(.~Month.long, nrow=3) 

ggsave(here('Plots/Best', 'Genetics.Piemap.ByMonth.png'), 
       height=40, width=40, dpi=300, units='cm')


###Just Larvae
pie.map.larvae <- genetics %>%
  subset(Gear == 'Neuston') %>%
  group_by(Date, TIME, LOCATION, GENETICS_NAME) %>%
  tally() %>%
  pivot_wider(names_from = GENETICS_NAME, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(neuston.effort) %>%
  distinct(Date, TIME, LOCATION, .keep_all = TRUE) %>%
  mutate(Tlarvae = rowSums(.[4:7]))


##Larval Pie Map by Year
ggplot(pie.map.larvae) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.larvae,
                  cols= c("Bloater", "Cisco", "Kiyi"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .35),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Larval Ciscoe Occurrences', 
       subtitle='Samples collected 2019-2022', 
       caption=ann_data_access) +
  facet_wrap(~Year, nrow=2)

ggsave(here('Plots/Best', 'Genetics.Larvae.Piemap.Year.png'), 
       height=30, width=40, dpi=300, units='cm')

pie.map.larvae <- pie.map.larvae %>%
  select(-`ciscoe hybrid`)


##2022 Larval Pie Map by Species
ggplot(subset(pie.map.larvae, Year == 2022)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(pie.map.larvae, Year == 2022), 
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish"),
                  pie_scale = 0.6) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.13, .85),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Larval Ciscoe Occurrences', 
       caption=ann_data_access) 

ggsave(here('Plots/Best', '2022.Larvae.Piemap.png'), 
       height=20, width=40, dpi=300, units='cm')



###Age-0, <100 mm, from mid-water and bottom trawl collected fish
pie.map.Age0 <- genetics %>%
  subset(Gear != 'Neuston') %>%
  subset(Length <= 100) %>%
  subset(Year == 2022) %>%
  group_by(OP_ID, GENETICS_NAME) %>%
  tally() %>%
  pivot_wider(names_from = GENETICS_NAME, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(effort) 


ggplot(pie.map.Age0) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=pie.map.Age0,
                  cols= c("Bloater", "Cisco", "Kiyi", "Shortnose Cisco"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.2, .8),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Age-0 Ciscoe Occurrences', 
       subtitle='Samples collected in 2022', 
       caption=ann_data_access)  

ggsave(here('Plots/Best', 'Genetics.2022.Age0.Piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


####################################################################################################################
##Lengths
##Lengths of all genetically identified fish
ggplot(subset(genetics, GENETICS != 217 &  SPECIES != 217), aes(x=Length, fill = GENETICS_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1') + 
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks(6)) +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(y='Count', x='Length (mm)',
       title='Lake Superior Genetically Identified Ciscoe Lengths',
       subtitle='', 
       caption=ann_data_access) 

ggsave(here('Plots/Best','Genetic.Lengths.png'), dpi = 150, width = 40, height = 20, units = "cm")  


#Yolks by week and not by species
yolk.lengths.217 <- genetics %>%
  subset(Gear == 'Neuston') %>%
  ##  subset(Year == 2022) %>%
  group_by(Year, Week, YolkSac) %>%
  summarise(n = n(),
            meanL = mean(Length),
            minL = min(Length),
            maxL = max(Length)) %>%
  ungroup() %>%
  left_join(effort) %>%
  distinct(Year, Week, YolkSac, .keep_all = TRUE) %>%
  select(Year, Week, Date1, YolkSac, n, meanL, minL, maxL) %>%
  drop_na()

ggplot(yolk.lengths.217, aes(x=as.factor(Week), y=meanL, color = YolkSac))  + 
  geom_pointrange(data=yolk.lengths.217, aes(ymin = minL, ymax=maxL, color=YolkSac), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_text_repel(data=yolk.lengths.217, aes(x=as.factor(Week), y=meanL, label = n),
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=yolk.lengths.217$Week, labels=yolk.lengths.217$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Yolk sac present")) +
  scale_color_brewer(palette="Pastel1", labels = c("No", "Yes")) +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_text(size=24, family='serif'),
        legend.text = element_text(size=24, family='serif'), 
        # legend.direction = "horizontal", 
        legend.position = c(.15, .85)) + 
  labs( x='Week', 
        y='Mean length (mm)',
        title='Lake Superior Larval Ciscoe Lengths and Yolk Sac Occurrence',
        subtitle='Unidentified ciscoes', 
        caption=ann_data_access) +
  facet_wrap(.~Year, ncol=1)

ggsave(here('Plots/Best', 'Ciscoe.Larval.Length.Yolks.png'), 
       dpi = 300, width = 40, height = 20, units = "cm")  




##Yolks Lengths by Species and week 
yolk.length <- genetics %>%
  subset(Gear == 'Neuston') %>%
##  subset(Year == 2022) %>%
  group_by(Year, Week, GENETICS_NAME, YolkSac) %>%
  summarise(n = n(),
            meanL = mean(Length),
            minL = min(Length),
            maxL = max(Length)) %>%
  ungroup() %>%
  left_join(effort) %>%
  distinct(GENETICS_NAME, Year, Week, YolkSac, .keep_all = TRUE) %>%
  select(Year, Week, Date1,  GENETICS_NAME, YolkSac, n, meanL, minL, maxL) %>%
  drop_na()


ggplot(subset(yolk.length, Year == 2022), 
       aes(x=as.factor(Week), y=meanL, color = YolkSac))  + 
  geom_pointrange(data=subset(yolk.length, Year == 2022), 
                  aes(ymin = minL, ymax=maxL, color=YolkSac), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_text_repel(data = subset(yolk.length, Year == 2022), 
                  aes(x=as.factor(Week), y=meanL, label = n), 
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=yolk.length$Week, labels=yolk.length$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Yolk sac present")) +
  scale_color_brewer(palette="Pastel1", labels = c("No", "Yes")) +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_text(size=24, family='serif'),
        legend.text = element_text(size=24, family='serif'), 
        # legend.direction = "horizontal", 
        legend.position = c(.2, .8)) + 
  labs( x='Week', 
        y='Mean length (mm)',
        title='Lake Superior Larval Coregonus Lengths and Yolk Sac Occurrence',
        subtitle='', 
        caption=ann_data_access) +
  facet_wrap(~GENETICS_NAME)

ggsave(here('Plots/Best', 'Genetics.2022.Larval.Length.Yolks.png'), 
       dpi = 300, width = 40, height = 20, units = "cm")  


ggplot(yolk.length, 
       aes(x=as.factor(Week), y=meanL, color = YolkSac))  + 
  geom_pointrange(data=yolk.length, 
                  aes(ymin = minL, ymax=maxL, color=YolkSac), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_text_repel(data = subset(yolk.length, Year == 2022), 
                  aes(x=as.factor(Week), y=meanL, label = n), 
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=yolk.length$Week, labels=yolk.length$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Yolk sac present")) +
  scale_color_brewer(palette="Pastel1", labels = c("No", "Yes")) +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_text(size=24, family='serif'),
        legend.text = element_text(size=24, family='serif'), 
        # legend.direction = "horizontal", 
        legend.position = c(.2, .8)) + 
  labs( x='Week', 
        y='Mean length (mm)',
        title='Lake Superior Larval Coregonus Lengths and Yolk Sac Occurrence',
        subtitle='', 
        caption=ann_data_access) +
  facet_wrap(~Year + GENETICS_NAME)

ggsave(here('Plots/Best', 'Genetics.Larval.Length.Yolks.png'), 
       dpi = 300, width = 40, height = 40, units = "cm")  

##########################################################################################################################
###2022 larval yolk P/A all as unidentified
###Get larval data for fish with no genetic results yet....
data_path <- here('Data', 'genetics.xlsx')
larvae <-read_excel(data_path, sheet = 'Larvae')

larvae$SPECIES<-as.factor(larvae$SPECIES)
larvae$GENETICS<-as.factor(larvae$GENETICS)

yolk.dates.1 <- larvae %>%
  filter(!is.na(YolkSac)) %>% 
  left_join(sci.names) %>% 
  left_join(neuston.effort) %>%
  group_by(Date, COMMON_NAME, YolkSac) %>%
  tally() %>%
  pivot_wider(names_from = YolkSac, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(yolks = (Y/(Y+N))*100) %>%
  left_join(neuston.effort) %>%
  distinct(Date, COMMON_NAME, .keep_all = TRUE) %>%
  ungroup() 

yolk.dates.2 <- yolk.dates.1 %>%
  distinct(Week, .keep_all = TRUE) 


ggplot(yolk.dates.1, 
       aes(x=as.Date(Date), y = yolks)) +
  geom_point(shape = 21, size=6, fill = "yellow")  +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_text_repel(data = subset(yolk.dates.1, Year == 2022), aes(x=as.Date(Date), y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Yolk sac")) +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  #axis.text.x=element_text(size=16, family='serif', angle = 90, vjust=0, hjust=0)) +
  labs( x='Date', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Ciscoe Yolk Sac Occurrence',
        subtitle=' ', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022.YolkProp.png'), dpi = 300, width = 40, height = 20, units = "cm")  



ggplot(subset(yolk.dates.1, Year == 2022),
       aes(x=Date, y = yolks)) +
  geom_point(shape = 21, size=6, fill = "yellow")  +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_text_repel(data = subset(yolk.dates.1, Year == 2022), aes(x=Date, y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Yolk sac")) +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  #axis.text.x=element_text(size=16, family='serif', angle = 90, vjust=0, hjust=0)) +
  labs( x='Date', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Ciscoe Yolk Sac Occurrence',
        subtitle=' ', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022.YolkProp.png'), dpi = 300, width = 40, height = 20, units = "cm")  


###2022 larval yolk P/A for genetically identified larvae
yolk.dates.3 <- genetics %>%
  filter(!is.na(YolkSac)) %>% 
  left_join(sci.names) %>% 
  left_join(neuston.effort) %>%
  group_by(Date, GENETICS_NAME, YolkSac) %>%
  tally() %>%
  pivot_wider(names_from = YolkSac, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(yolks = (Y/(Y+N))*100) %>%
  left_join(neuston.effort) %>%
  distinct(Date, GENETICS_NAME, .keep_all = TRUE) %>%
  ungroup() 

yolk.dates.4 <- yolk.dates.3 %>%
  distinct(Week, .keep_all = TRUE) 


ggplot(subset(yolk.dates.3, Year == 2022), aes(x=as.Date(Date), y = yolks, fill = GENETICS_NAME)) + 
  geom_point(shape = 21, size=6)  +
  geom_line(data=subset(yolk.dates.3, Year == 2022), 
              aes(x=as.Date(Date), y=yolks), size = 1) +
  geom_text_repel(data = subset(yolk.dates.3, Year == 2022), aes(x=as.Date(Date), y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Yolk sac"),
         color=guide_legend(title="Yolk sac")) +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  #axis.text.x=element_text(size=16, family='serif', angle = 90, vjust=0, hjust=0)) +
  labs(x='Date', 
       y='Proportion of catch with yolk sac',
       title='Lake Superior Larval Ciscoe Yolk Sac Occurrence',
       subtitle='Collections May-Oct 2022 ', 
       caption=ann_data_access)
  

ggsave(here('Plots/Best','2022.YolkProp.BySpecies.png'), dpi = 300, width = 40, height = 20, units = "cm")  




##################################################################################################################
##Ship-genetics comparison plot

genetics.age1 <- genetics %>%
  subset(SPECIES != 217) %>%
  group_by(GENETICS_NAME) %>%
  tally() %>%
  ungroup() %>%
  left_join(genetics)

ggplot(subset(genetics.age1, GENETICS != 217 &  SPECIES != 217), aes(x=GENETICS_NAME, fill=COMMON_NAME)) +
  geom_bar(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.1) ),  
            stat="count", position=position_dodge(0.9), vjust=-0.1, size = 8, family = 'serif') +  
  geom_text(aes(x=GENETICS_NAME, y=-0.04, label = n), size=8, family='serif') +  
  geom_text(label = "n =", x = 0.6,  y = -0.04,  size=10, family='serif') +  
  scale_y_continuous(labels = percent) +
  plot_theme +
  geom_hline(yintercept=0, color='black', size=.5) +
  scale_fill_brewer(palette="Pastel1", name = "Ship identification") +
  theme(legend.position = c(.15, .85)) + 
  labs(y='Ship identification proportion', x='Genomic identification') 
    #    title='Lake Superior Ciscoe Identifications',
    #   subtitle='Comparing genomic to ship board identification') +

ggsave(here('Plots/Best','Genetics.Ship.IDCompare.png'), 
       dpi = 300, width = 40, height = 20, units = "cm")   



##Length frequency with Color code bars by ship identification
ggplot(subset(genetics, GENETICS != 217 & SPECIES != 217), 
       aes(x=Length, fill = COMMON_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1', name = "Ship identification") +
  theme(legend.position =  c(.85, .3)) +
  labs(y='Count', x='Length (mm)',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~GENETICS_NAME, scales = "free_y")

ggsave(here('Plots/Best','Genetics.Ship.IDComparebyLength.png'), dpi = 150, width = 40, height = 20, units = "cm")  



###########################################################################################################################
##Genetic - ship ID maps
genetics.map <- genetics %>%
  subset(GENETICS != 217)%>%
  subset(SPECIES != 217) %>%
  group_by(OP_ID, GENETICS_NAME, Match) %>%
  tally() %>%
  pivot_wider(names_from = Match, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(genetics.sites) %>%
  mutate(Fish = Yes + No)

##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##Genetics sampling sites
ggplot(genetics.map) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(data=genetics.map, aes(x=Mid.Long.DD, y=Mid.Lat.DD, size = Fish), color = "blue") +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.1, .8)) + 
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Genetics Locations')

ggsave(here('Plots/Best', 'IDcompareSamples.Map.png'), 
       height=20, width=40, dpi=300, units='cm')

##Pie map showing match - no match locations
genetics.sites <- genetics %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  select(OP_ID, LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth)


###Match Yes - No Map
ggplot(subset(genetics.map, GENETICS_NAME != 'ciscoe hybrid')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(genetics.map, GENETICS_NAME != 'ciscoe hybrid'),
                  cols= c("Yes", "No"),
                  pie_scale = 0.75, 
                  legend_name = "Match") +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.1, .9)) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification') +
  facet_wrap(~ GENETICS_NAME, nrow = 2)

ggsave(here('Plots/Best', 'IDcompareMapBySpecies.png'), 
       height=30, width=40, dpi=300, units='cm')


#################################################################################################################
## Genetically identified ciscoe growth rates
##Growth August - October

genetics.names <- genetics %>%
  select(GENETICS, GENETICS_NAME) %>%
  distinct(GENETICS, .keep_all = TRUE)

genetics.growth.data <- genetics %>% 
  drop_na(GENETICS, Length) %>%
  subset(GENETICS == '202' |
           GENETICS == '204' |
           #       GENETICS == '208' |
           GENETICS == '206') %>%
  subset(Length <=80 & Month <=7 | Month >=8) %>%
  subset(Length <140) %>%
  subset(Year == 2022) %>%
  left_join(genetics.names)

genetics.growth.sum <- genetics.growth.data %>%
  group_by(GENETICS_NAME) %>% 
  do({
    mod = lm(Length ~ Date, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  }) %>%
  mutate(Slope = round(Slope, 2),
         text1 = "mm per day") %>%
  unite("rate", GENETICS_NAME, Slope, sep = ", ", remove = FALSE) %>%
  unite("Legend", rate, text1, sep = " ", remove = FALSE)

genetics.growth.data <- genetics.growth.data %>%
  left_join(genetics.growth.sum)

ggplot(genetics.growth.data, 
       aes(x=as.Date(Date), y = Length, color = Legend, fill = Legend)) +
  geom_jitter(shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", size = 2, se = FALSE) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Mean late summer growth rate"),
         color=guide_legend(title="Mean late summer growth rate")) +
  scale_fill_brewer(palette="Pastel1") +
  scale_colour_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title.align=0.5,
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.17, .8)) + 
  labs( x='Date', 
        y='Length (mm)',
        title='2022 Lake Superior Age-0 Ciscoe Growth',
        subtitle='Identifications based on genomics', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022Age0CiscoeDailyGrowth.png'), dpi = 150, width = 40, height = 20, units = "cm")  

#################################################################################################
##Growth May-October

genetics.names <- genetics %>%
  select(GENETICS, GENETICS_NAME) %>%
  distinct(GENETICS, .keep_all = TRUE)

genetics.growth.data <- genetics %>% 
  drop_na(GENETICS, Length) %>%
  subset(GENETICS == '202' |
           GENETICS == '204' |
           #       GENETICS == '208' |
           GENETICS == '206') %>% 
  subset(Length <=80 & Month <=7 | Month >=8) %>% 
  subset(Length <140) %>% 
  subset(Year == 2022) %>% 
  left_join(genetics.names)

genetics.growth.sum <- genetics.growth.data %>%
  group_by(GENETICS_NAME) %>% 
  do({
    mod = lm(Length ~ Date, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  }) %>%
  mutate(Slope = round(Slope, 2),
         text1 = "mm per day") %>%
  unite("rate", GENETICS_NAME, Slope, sep = ", ", remove = FALSE) %>%
  unite("Legend", rate, text1, sep = " ", remove = FALSE)

genetics.growth.data <- genetics.growth.data %>%
  left_join(genetics.growth.sum)

ggplot(genetics.growth.data, 
       aes(x=as.Date(Date), y = Length, color = Legend, fill = Legend)) +
  geom_jitter(shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", size = 2, se = FALSE) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Mean late summer growth rate"),
         color=guide_legend(title="Mean late summer growth rate")) +
  scale_fill_brewer(palette="Pastel1") +
  scale_colour_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title.align=0.5,
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.17, .8)) + 
  labs( x='Date', 
        y='Length (mm)',
        title='2022 Lake Superior Ciscoe Growth',
        subtitle='Identifications based on genomics', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022CiscoeDailyGrowth.png'), dpi = 150, width = 40, height = 20, units = "cm")  










####################################################################################################
genetic.dates <- genetics %>%
  select(Date, Year, Month, Week, Month.short, Month.long, Date1) %>%
  distinct(Date, .keep_all = TRUE)


##Length by yolksac by week
genetic.dailylengths <- genetics %>%
  drop_na(GENETICS, Length) %>%
  subset(GENETICS == '202' |
           GENETICS == '204' |
    #       GENETICS == '208' |
           GENETICS == '206') %>%
  subset(Length <=80 & Month <=7 | Month >=8) %>%
  subset(Length <140) %>%
  group_by(Date, GENETICS_NAME) %>%
  summarise(n = n(), 
            min.Length = min(Length), max.Length = max(Length),
            mean.Length = mean(Length), median.Length = median(Length),
            sd.Length = sd(Length), se.Length = sd.Length/sqrt(n)) %>%
  left_join(genetic.dates)


ggplot(subset(genetic.dailylengths, Year == 2022 & Month >=8), 
       aes(x=as.Date(Date), y = mean.Length, color = GENETICS_NAME)) +
  geom_pointrange(data=subset(genetic.dailylengths, Year == 2022 & Month >=8), 
                  aes(ymin = min.Length, ymax=max.Length, color= GENETICS_NAME), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_smooth(data=subset(genetic.dailylengths, Year == 2022 & Month >=8), 
              aes(x=as.Date(Date), y=mean.Length, color = GENETICS_NAME), size = 2, se = FALSE) +
  geom_text_repel(data = subset(genetic.dailylengths, Year == 2022 & Month >=8), 
                  aes(x=as.Date(Date), y=mean.Length, label = n), 
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Species")) +
  scale_color_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_blank(),
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.1, .85)) + 
  labs( x='Date', 
        y='Length (mm), mean & range',
        title='2022 Lake Superior Age-0 Ciscoe Lengths',
        subtitle='Identifications based on genomics', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022Age0CiscoeMeanDailyLength.png'), dpi = 150, width = 40, height = 20, units = "cm")  



###############################################################################################################
##Get BT temperature data
##load data 
BTdata1 <- read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTData")

BTdata1 <- BTdata1 %>%
  select(Sample, Depth, Temperature)


BTdata2 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample")
BTdata2$Date<-as.character(BTdata2.data$Date)
BTdata2$Date<-parse_date(BTdata2$Date, format='%d-%b-%y')

BTdata2 <- BTdata2 %>%
  select(Sample, Year, Date, 'd<3mT', ChlSurf, 'Chl3m') %>%
  subset(Year == 2011 |
           Year == 2016 |
           Year == 2022) %>%
  rename(Temperature = 'd<3mT')  %>%
  mutate(Month = month(Date)) %>%
  subset(Month >5) %>%
  subset(Month <11) %>%
  mutate(Group = case_when(
    Month == 6 ~ 'June', 
    Month == 7 ~ 'July', 
    Month >7 ~ 'Aug/Sep')) 

BTdata2$Group <- factor(BTdata2$Group,
                        levels = c("June","July","Aug/Sep"))

##ggridges plot with leopard look, median lines inside distribution not very visible
ggplot(BTdata2, aes(x = Temperature, y = as.factor(Year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01,
                               quantile_lines  = TRUE, quantiles = 2,    
                               gradient_lwd = 0, jittered_points = TRUE, point_size = 1)  +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  plot_theme + 
  theme(axis.text.x=element_text(size=16),
        legend.position=c(0.37,0.5),
        legend.title.align = 0.5, 
        legend.text.align = 0.8, 
        legend.direction="vertical",
        legend.text=element_text(size=20, family='serif')) +
  labs(title = "Lake Superior Near Surface Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption=ann_data_access, 
       y = "Year", x = "Water temperature, <3 m deep (C)") +
  facet_wrap(~Group, scales = "free_x")

ggsave(here('Plots/Best','CSMI_SurfaceWaterTemp.png'), dpi = 300, width = 40, height = 20, units = "cm")  


#######################
#Get daily water temps to put into age-0 fish plot

BTdata3 <- BTdata2 %>%
  group_by(Date) %>%
  summarise(mean.Temp = mean(Temperature), 
            mean.SurfChl = mean(ChlSurf),
            mean.Chl3m = mean(Chl3m)) %>%
  ungroup()


############################################################################################################

BTdata4 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample")
BTdata4$Date<-as.character(BTdata4.data$Date)
BTdata4$Date<-parse_date(BTdata4$Date, format='%d-%b-%y')
BTdata4 <- BTdata4 %>%
  select(Sample, Year, Date, 'd<3mT', ChlSurf, 'Chl3m') %>%
  subset(Year == 2022) %>%
  rename(Temperature = 'd<3mT')  %>%
  mutate(Month = month(Date)) 

BTdata5 <- BTdata4 %>%
  group_by(Date) %>%
  summarise(mean.Temp = mean(Temperature)) %>%
  ungroup() %>%
  left_join(yolk.dates.1)


ggplot(BTdata5, aes(x=as.Date(Date), y = yolks, fill = mean.Temp)) +
  geom_point(shape = 21, size=6)   +
  geom_smooth(se=FALSE, colour = 'black') +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_text_repel(data = BTdata5, aes(x=as.Date(Date), y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  labs( x='Date', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Coregonus Yolk Sac Occurrence and Surface Water Temperatures',
        subtitle='2022 fish yet to be identified', 
        caption=ann_data_access)

ggsave(here('Plots/Best','2022LarvalYolkPropTemperatures.png'), dpi = 300, width = 40, height = 20, units = "cm")  



############################################################################################################################  



##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##2022 age0 collections from midwater trawl
##load data for ship identifications

age0<-read_excel(here('Data','2022labsheet.xlsx'))
ShoreDistance <- read_excel(here('Data','ShoreDistance.xlsx'))

##Make species a factor
age0$SPECIES<-as.factor(age0$SPECIES)

## get rid of records with no SPECIES code
age0 <- age0 %>%
  drop_na(SPECIES)

##calculate the mid-point (average) latitude and longitude for each trawl where beg and end lat/longs are known
age0$Mid.Lat.DD <- rowMeans(age0[,c("BEG_LATITUDE_DD","END_LATITUDE_DD")], na.rm=TRUE)
age0$Mid.Long.DD <- rowMeans(age0[,c("BEG_LONGITUDE_DD","END_LONGITUDE_DD")], na.rm=TRUE)
age0$Mid.Depth <- rowMeans(age0[,c("BEG_DEPTH","END_DEPTH")], na.rm=TRUE)


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

######################################################################################

age0.effort <- age0 %>%
  select(OP_ID, OP_DATE, LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, TR_DESIGN) %>%
  mutate(Year = year(OP_DATE), 
         Month = month(OP_DATE),
         Week = week(OP_DATE), 
         Habitat = ifelse(LOCATION <500, "nearshore", "offshore")) %>%
  left_join(ShoreDistance) %>% 
  distinct(OP_ID, .keep_all = TRUE)


## pie map for age-0 bloater, cisco, kiyi for current year based on ship IDs
age0.fish <- age0 %>%
  mutate(Month = month(OP_DATE)) %>%
  subset(Month >= 8) %>%
  subset(LENGTH < 150) %>%
  left_join(sci.names) %>%
  select(OP_ID, COMMON_NAME) %>%
  group_by(OP_ID, COMMON_NAME) %>%
  tally() %>%
  pivot_wider(names_from = COMMON_NAME, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(age0.effort) 

##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(age0.fish) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=age0.fish,
                  cols= c("Bloater", "Cisco", "Kiyi", "Unidentified Coregonid"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.2, .85),
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-0 Ciscoe Collections Based on Ship Identifications',
       subtitle='Collections made August-October 2022')

ggsave(here('Plots/2022Age0_ciscoes_shipID_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


## Age-0 Genetics samples to do
age0.samples <- age0 %>%
  mutate(Month = month(OP_DATE)) %>%
  #subset(Month >= 8) %>%
  #subset(LENGTH < 200) %>%
  left_join(sci.names) %>%
  group_by(OP_ID) %>%
  tally() %>%
  ungroup() %>%
  left_join(age0.effort) 


ggplot(age0.samples) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(age0.samples,
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = n), fill = "pink", shape = 21, stroke = 1.8, alpha = 0.6) +  
  theme_bw() +
  map_theme +
  scale_size_continuous(name = "Fish collected", limits = c(1, 220), range = c(0, 12)) +
  guides(size = guide_legend()) +
  theme(legend.position = c(.15, .75),
        legend.text=element_text(size=24, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Age-0 and Age-1 Ciscoe Sample Sizes',
       subtitle='1773 fish collected May - October', 
       caption=ann_data_access) 

ggsave(here('Plots/2022Age0Collections.png'), 
       height=20, width=40, dpi=300, units='cm')




##########################################################################################################
##Larvae genetics 
##Get  genetic identifications, join with effort
data_path <- here('Data', 'genetics.xlsx')
larval<-read_excel(data_path, sheet = 'Larvae')
age1<-read_excel(data_path, sheet = 'Age0+')

##Make species a factor
larval$SPECIES<-as.factor(larval$SPECIES)
age1$SPECIES<-as.factor(age1$SPECIES)
age1$GENETICS<-as.factor(age1$GENETICS)


## get rid of records with no SPECIES code, tally individuals per species per collection
larval.1 <- larval %>%
  drop_na(SPECIES) %>%
  group_by(OP_ID, SPECIES) %>%
  tally %>%
  ungroup() %>%
  left_join(neuston.effort) %>%
  left_join(sci.names) %>%
  subset(!is.na(n)) %>%
  distinct(OP_ID, SPECIES, .keep_all = TRUE)


####################################################################################################################NEARSHORE DATA####
## stacked bar histogram by week of the year
larval.2 <- larval.1 %>%
  subset(SPECIES == '202' |
           SPECIES == '203' |
           SPECIES == '204' |
           SPECIES == '217' |
           SPECIES == '206' ) %>%
  group_by(Year, Week, COMMON_NAME) %>%
  summarise(fish = sum(n)) %>%
  ungroup()



larval.weeks <- larval.1 %>%
  left_join(neuston.effort) %>%
  distinct(Week, .keep_all = TRUE)


ggplot(larval.2, aes(x=as.factor(Week), weights = fish, fill = COMMON_NAME))+
  geom_bar() + 
  plot_theme +
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=18, family='serif'), 
        legend.title = element_blank(), 
        #legend.direction = 'horizontal', 
        legend.position = c(0.8, 0.5)) +
  labs(y='Number collected', x='Week',
       title='Lake Superior Larval Coregonus Collections',
       subtitle=' ', 
       caption=ann_data_access) +
  scale_x_discrete(breaks=larval.weeks$Week, labels=larval.weeks$Date1, guide = guide_axis(n.dodge = 1)) +
  scale_y_continuous() +
  facet_wrap(.~Year, nrow=3, scales = "free_y")
  
  
ggsave(here('Plots','LarvalCiscoesbyWeek.png'), dpi = 300, width = 40, height = 40, units = "cm")  


######################################################################################################################################
###Map of larval ciscoe occurences

larval.3 <- larval.1 %>%
  pivot_wider(names_from = COMMON_NAME, values_from = n, values_fill = 0) %>%
  drop_na()


##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(subset(larval.3, Year != 2022)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(larval.3, Year != 2022),
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish", "Unidentified Coregonid"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.55, .8),
        legend.text=element_text(size=20, family='serif')) +
  labs(x='Longitude', y='Latitude') +
  facet_wrap(.~Year, nrow=1) 

ggsave(here('Plots/2019_20LarvalCiscoes_piemap.png'), 
       height=15, width=40, dpi=300, units='cm')

ggplot(larval.3) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=larval.3,
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish", "Unidentified Coregonid"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .3),
        legend.text=element_text(size=24, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Larval Ciscoe Occurrences',
       subtitle ='Collections made May-July') +
  facet_wrap(.~Year, nrow=2) 

ggsave(here('Plots/2019_22LarvalCiscoes_piemap.png'), 
       height=25, width=40, dpi=300, units='cm')

###Species occurrence maps by species
ggplot(larval.1) +
#  ggplot(subset(larval.1, COMMON_NAME != "Unidentified Coregonid")) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(aes(x=Mid.Long.DD, y=Mid.Lat.DD), color="blue", size = 4) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.7, .4),
        legend.text=element_text(size=24, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Larval Coregonus Occurrences',
       subtitle = 'Collections made May-July') +
    facet_grid(COMMON_NAME ~ Year) 
##facet_wrap(.~COMMON_NAME) 

ggsave(here('Plots/2019_22LarvalCiscoesOccurrences.png'), 
       height=40, width=40, dpi=300, units='cm')


##2022 larval samples processed to date map
neuston.22 <- neuston.1 %>%
  subset(Year == 2022) %>%
  subset(SPECIES == 217 |
           SPECIES == 0) %>%
  group_by(Date, LOCATION) %>% 
  summarise(Collected = sum(Fish)) %>%
  left_join(neuston.1) %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  ungroup() 
  
larval.done <- larval.1 %>%
  subset(Year == 2022) %>%
  group_by(Date, LOCATION) %>% 
  summarise(Processed = sum(n)) %>%
  ungroup() %>%
  right_join(neuston.22) %>%
  select(Date, LOCATION, Mid.Lat.DD, Mid.Long.DD, Collected, Processed) %>%
  replace(is.na(.), 0) %>% 
  mutate(ToDo = Collected - Processed) %>%
  mutate(Work = case_when(
    ToDo <= 0 ~ "Processed",
    ToDo >= 1 & LOCATION > 500 ~ "Processed",
    ToDo <= 15 ~ "Processed",
    Collected <= 200  ~ "Processed",
    ToDo > 0 & LOCATION < 500  ~ "To do")) %>%
  mutate(CouldDo = case_when(
    Collected > 200 & LOCATION < 500 ~ Collected - 200,
    Collected < 200 ~ 0)) %>%
  mutate(Remaining = ToDo) %>%
  mutate(Remaining = replace(Remaining, Remaining <= 15, 0)) %>%
  mutate(Remaining = replace(Remaining, Work == 'Processed', 0)) %>%
  mutate(Remaining2 = case_when(
  Remaining > 0 ~ Remaining)) %>%
  mutate(larval.PA = case_when(
    Collected > 0 ~ 'present',
    Collected == 0 ~ 'absent')) 

larval.done[is.na(larval.done)] <- 0


FishDone = sum(larval.done$Processed)
FishCatch = sum(larval.done$Collected)+5
FishExtra = sum(larval.done$CouldDo)
FishToDo = sum(larval.done$Remaining)


today.date <- Sys.Date()
today.date <- format(as.Date(today.date, format = "%Y-%m-%d"), "%A, %B %d, %Y") 


ggplot(subset(larval.done, Work != "No fish")) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(subset(larval.done, Work != "No fish"),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Collected, fill = Work), shape = 21, alpha = 0.6) +  
#  geom_text(mapping=aes(Mid.Long.DD, Mid.Lat.DD, label = Remaining2), nudge_x = -0.1, nudge_y = 0.15, size = 6, family='serif') +  
  annotate("text", x=-90, y = 46.4, label = paste(FishDone, "fish processed of", FishCatch, "fish collected"), 
           size = 8, family='serif') +
  theme_bw() +
  map_theme +
  scale_size_continuous(range = c(2, 12), breaks=pretty_breaks(5), name = "Count") +
  guides(fill= guide_legend(override.aes = list(size = 7), name = " "), 
         size = guide_legend(), name = " ") +
  theme(legend.title = element_blank(), 
    legend.position = c(.065, .67),
    legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Larval Coregonus Pre-Genetics Sample Processing Status',
       subtitle=paste('Work completed as of', today.date, sep = " "), 
       caption=ann_data_access) 

ggsave(here('Plots/2022LarvalCiscoeWork.png'), 
       height=20, width=40, dpi=300, units='cm')


##2022 larval collections
ggplot() + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(subset(larval.done, larval.PA = 'present'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Collected), fill = 'violet', shape = 21, alpha=0.6) +  
#  geom_point(subset(larval.done, larval.PA = 'absent'),
#             mapping=aes(Mid.Long.DD, Mid.Lat.DD), fill = 'black', shape = 21, size = 1) +  
  annotate("text", x=-90, y = 46.4, label = paste(FishDone, "total fish"), 
             size = 8, family='serif') +
  theme_bw() +
  map_theme +
  scale_size_continuous(range = c(3, 12), breaks=pretty_breaks(6), name = "Count") +
  theme(legend.position = c(.12, .7),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude') 

ggsave(here('Plots/2022LarvalCollections.png'), 
       height=20, width=40, dpi=300, units='cm')

##Extra nearshore Larvae that could be processed
ggplot(subset(larval.done, CouldDo > 0)) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(subset(larval.done, CouldDo > 0),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = CouldDo), fill = "pink", shape = 21, alpha = 1) +  
  annotate("text", x=-90, y = 46.4, label = paste(FishExtra, "additional fish could be processed in interesting areas"), 
           size = 6, family='serif') +
  geom_text(mapping=aes(Mid.Long.DD, Mid.Lat.DD, label = CouldDo), nudge_x = -0.1, nudge_y = 0.1, size = 6, family='serif')+  
  theme_bw() +
  map_theme +
  scale_size_continuous(breaks = c(0, 100, 500, 1000), name = "Extra Fish\nto Potentially\nProcess") +
  theme(legend.title.align = 0.5, 
        legend.position = c(.15, .7),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Larval Coregonus From High Catch Nearshore Sites',
       subtitle=paste('As of', today.date, sep = " "), 
       caption=ann_data_access) 

ggsave(here('Plots/2022ExtraLarvalCiscoes.png'), 
       height=20, width=40, dpi=300, units='cm')


##2022 Larvae remaining to process
ggplot(subset(larval.done, Work == 'To do')) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(subset(larval.done, Work == 'To do'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = Remaining), fill = "pink", shape = 21, alpha = 1) +  
  geom_text(mapping=aes(Mid.Long.DD, Mid.Lat.DD, label = Remaining), nudge_x = -0.1, nudge_y = 0.1, size = 6, family='serif') +  
  annotate("text", x=-90, y = 46.4, label = paste(FishToDo, "fish to process"), 
           size = 10, family='serif') +
  theme_bw() +
  map_theme +
  scale_size_continuous(breaks = c(0, 100, 500, 1000), name = "Fish to Process") +
  theme(legend.title.align = 0.5, 
        legend.position = c(.2, .8),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Larval Coregonus To Process for Genetics',
       subtitle=paste('As of', today.date, sep = " "), 
       caption=ann_data_access) 

ggsave(here('Plots/2022LarvalCiscoesToDo.png'), 
       height=20, width=40, dpi=300, units='cm')

##########################################################################################################################
##Larval lengths
larval.4 <- larval %>%
  drop_na(SPECIES, Length) %>%
  subset(SPECIES == '202' |
         #  SPECIES == '203' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '217' ) %>%
  left_join(sci.names) %>%
  left_join(neuston.effort)


yolk.dates <- larval.4 %>%
  filter(!is.na(YolkSac)) %>% 
  group_by(Date, COMMON_NAME, YolkSac) %>%
  tally() %>%
  pivot_wider(names_from = YolkSac, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(yolks = (Y/(Y+N))*100) %>%
  left_join(neuston.effort) %>%
  distinct(Date, COMMON_NAME, .keep_all = TRUE) %>%
  ungroup() 

yolk.dates.1 <- yolk.dates %>%
  distinct(Week, .keep_all = TRUE) 


ggplot(subset(yolk.dates, Year == 2022),
       aes(x=Date, y = yolks)) +
  geom_point(shape = 21, size=6, fill = "yellow")  +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_text_repel(data = subset(yolk.dates, Year == 2022), aes(x=Date, y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Yolk sac")) +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  #axis.text.x=element_text(size=16, family='serif', angle = 90, vjust=0, hjust=0)) +
  labs( x='Date', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Coregonus Yolk Sac Occurrence',
        subtitle='2022 fish yet to be identified', 
        caption=ann_data_access)

ggsave(here('Plots','2022LarvalCiscoesYolkProp.png'), dpi = 300, width = 40, height = 20, units = "cm")  

ggplot(yolk.dates, aes(x=as.factor(Week), y = yolks, fill = COMMON_NAME)) +
  geom_point(shape = 21, size=6) +
#  geom_smooth(color = "black", se=FALSE) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=yolk.dates.1$Week, labels=yolk.dates.1$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.title = element_blank(), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  #axis.text.x=element_text(size=16, family='serif', angle = 90, vjust=0, hjust=0)) +
  labs( x='Week', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Coregonus Yolk Sac Occurrence',
        subtitle='Samples collected 2019-22', 
        caption=ann_data_access)

ggsave(here('Plots','LarvalCiscoesYolkProp.png'), dpi = 300, width = 40, height = 20, units = "cm")  


##Larval length and yolk sac P/A
yolk.dates.2 <- larval.4 %>%
  filter(!is.na(YolkSac)) %>% 
  group_by(Date, COMMON_NAME, YolkSac) %>%
  tally() %>%
  replace(is.na(.), 0) %>%
  left_join(neuston.effort) %>%
  distinct(Date, COMMON_NAME, YolkSac, .keep_all = TRUE) %>%
  mutate(Yolk = case_when(
    YolkSac == 'Y' ~ "Present",
    YolkSac == 'N' ~ "Absent"))
  


larval.5 <- larval %>%
  drop_na(SPECIES, Length) %>%
  subset(SPECIES == '202' |
           #  SPECIES == '203' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '217' ) %>%
  left_join(neuston.effort) %>%
  group_by(Year, Week, SPECIES) %>%
  summarise(n = n(), 
            min.Length = min(Length), max.Length = max(Length),
            mean.Length = mean(Length), median.Length = median(Length),
            sd.Length = sd(Length), se.Length = sd.Length/sqrt(n)) %>%
  left_join(sci.names) %>%
  left_join(neuston.effort) %>%
  distinct(Week, COMMON_NAME, .keep_all = TRUE)


ggplot(larval.5, aes(x=as.factor(Week), y = mean.Length)) +
  geom_point(shape = 21, size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_segment(aes(x=as.factor(Week), xend=as.factor(Week), y=min.Length, yend=max.Length), size=1, color='black')+
  geom_text(data = larval.5, aes(x=as.factor(Week), y=5, label = n), size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=larval.5$Week, labels=larval.5$Date1, guide = guide_axis(n.dodge = 3)) +
  theme_bw() +
  plot_theme +
  scale_fill_grey(start = 0.1, end = 0.9) +
  theme(axis.text.x=element_text(size=15, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.6, .3)) +
  labs( x='Week', 
        y='Length (mm)',
        title='Lake Superior Larval Coregonus Length',
        caption=ann_data_access) +
  facet_grid(COMMON_NAME ~ Year) 

ggsave(here('Plots','LarvalCiscoesMeanLengthbyWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  

ggplot(larval.4, aes(x=as.factor(Week), y = Length)) +
  geom_point(shape = 21, size=5) +
  theme_bw() +
  plot_theme +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=larval.5$Week, labels=larval.5$Date1, guide = guide_axis(n.dodge = 3)) +
  theme(axis.text.x=element_text(size=15, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.title = element_blank(),
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.83, .9)) +
  labs( x='Week', 
        y='Length (mm)',
        title='Lake Superior Larval Coregonus Length',
        caption=ann_data_access) +
  facet_grid(COMMON_NAME ~ Year) 

ggsave(here('Plots','LarvalCiscoesLengthbyWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  



larval.22 <- larval.5 %>%
  subset(Year == 2022)

ggplot(larval.22, aes(x=as.factor(Week), y = mean.Length)) +
  geom_point(shape = 21, size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_segment(aes(x=as.factor(Week), xend=as.factor(Week), y=min.Length, yend=max.Length), size=1, color='black')+
  geom_text(data = larval.22, aes(x=as.factor(Week), y=5, label = n), size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=larval.22$Week, labels=larval.22$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  scale_fill_grey(start = 0.1, end = 0.9) +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.6, .3)) +
  labs( x='Week', 
        y='Length (mm)',
        title='Lake Superior Larval Coregonus Lengths',
        subtitle='2022 fish yet to be identified', 
        caption=ann_data_access)
 

ggsave(here('Plots','2022LarvalCiscoesMeanLengthWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  


##Length by yolksac by week
larval.6 <- larval %>%
  drop_na(SPECIES, Length) %>%
  subset(SPECIES == '202' |
           #  SPECIES == '203' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '217' ) %>%
  left_join(neuston.effort) %>%
  group_by(Year, Week, SPECIES, YolkSac) %>%
  summarise(n = n(), 
            min.Length = min(Length), max.Length = max(Length),
            mean.Length = mean(Length), median.Length = median(Length),
            sd.Length = sd(Length), se.Length = sd.Length/sqrt(n)) %>%
  left_join(sci.names) %>%
  left_join(neuston.effort) %>%
  distinct(Week, COMMON_NAME, YolkSac, .keep_all = TRUE)

larval.22b <- larval.6 %>%
  subset(Year == 2022)



ggplot(larval.22b, aes(x=as.factor(Week), y = mean.Length, color = YolkSac)) +
  geom_pointrange(data=larval.22b, aes(ymin = min.Length, ymax=max.Length, color=YolkSac), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_text_repel(data = larval.22b, aes(x=as.factor(Week), y=mean.Length, label = n), 
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_discrete(breaks=larval.22b$Week, labels=larval.22b$Date1, guide = guide_axis(n.dodge = 1)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Yolk sac present")) +
  scale_color_brewer(palette="Pastel1", labels = c("No", "Yes")) +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_text(size=24, family='serif'),
        legend.text = element_text(size=24, family='serif'), 
       # legend.direction = "horizontal", 
        legend.position = c(.2, .8)) + 
  labs( x='Week', 
        y='Length (mm)',
        title='Lake Superior Larval Coregonus Lengths and Yolk Sac Occurrence',
        subtitle='2022 fish yet to be identified', 
        caption=ann_data_access)


ggsave(here('Plots','2022LarvalCiscoesMeanLengthYolksbyWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  



###Map of yolk P/A
larval.7 <- larval.1 %>%
  group_by(Year, LOCATION) %>%
  summarise(fish = sum(n)) 

larval.8 <- larval.4 %>%
  group_by(Year, LOCATION, YolkSac) %>%
  tally() %>%
  na.omit(Species, YolkSac) %>%
  pivot_wider(names_from = YolkSac, values_from = n, values_fill = 0) %>%
  left_join(neuston.effort) %>%
  ungroup() %>%
  rename(No = N, Yes = Y) %>%
  left_join(larval.7)

ggplot(larval.8) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=larval.8,
                  cols= c("Yes", "No"),
                  color = NA, 
                  pie_scale = 0.75, 
                  legend_name = "Present") +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.7, .3)) +
  labs(x = 'Longitude', 
       y = 'Latitude',
       title = 'Lake Superior Larval Coregonus Yolk Sac Occurrence', 
       subtitle = 'Samples collected May-July', 
       caption = ann_data_access) +
  facet_wrap(~ Year, nrow = 2)


ggsave(here('Plots','LarvalCiscoeYolkMap.png'), dpi = 300, width = 40, height = 20, units = "cm")  

ggplot(subset(larval.8, Year == 2022)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(larval.8, Year == 2022), 
                  cols= c("Yes", "No"),
                  color = NA, 
                  pie_scale = 0.4,
                  legend_name = 'Present') +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.1, .8)) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Larval Coregonus Yolk Sac Occurrence', 
       subtitle='Samples collected May-August 2022') 
  
ggsave(here('Plots','2022LarvalCiscoeYolkMap.png'), dpi = 300, width = 40, height = 20, units = "cm")  

####################################
#larvae distance from shore by species
larval.distance <- larval.1 %>%
  group_by(OP_ID, SPECIES) %>%
  summarise(Tfish = sum(n)) %>%
  ungroup() %>%
  left_join(neuston.effort) %>%
  left_join(sci.names)
  


ggplot(subset(larval.distance, SPECIES != 217)) + 
  geom_jitter(aes(x=ShoreDistance/1000, y = Tfish, fill = COMMON_NAME), shape = 21, size = 6) + 
  scale_x_continuous(breaks = pretty_breaks(), name = 'Distance to Shore (km)') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Fish collected')+
  scale_fill_brewer(palette="Pastel1") +
  plot_theme +
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Distributions',
       subtitle='USGS Surface Trawl Surveys, May-July') 
#+
#  facet_wrap(~ Month.long)

ggsave(here('Plots','Larval.ShoreDistance.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##########################################################################################################
##########################################################################################################
##########################################################################################################
##Age-1 genetics 2019, 2021, and 2022
##Get age1 genetic identifications

data_path <- here('Data', 'genetics.xlsx')
age1<-read_excel(data_path, sheet = 'Age0+')

##Make SPECIES and GENETICS factors
#age1$SPECIES<-as.factor(age1$SPECIES)
#age1$GENETICS<-as.factor(age1$GENETICS)

##load the raw RVCAT effort data file to get effort data
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
trawl.data<-read.csv(here('Data','RVCAT.csv'))
trawl.data$SPECIES<-as.factor(trawl.data$SPECIES) 


##change date format into usable form
trawl.data$OP_DATE<-as.character(trawl.data$OP_DATE)
trawl.data$OP_DATE<-parse_date(trawl.data$OP_DATE, format='%d-%b-%y')

trawl.data <- trawl.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Month.short = month.abb[Month],
         Month.long = month.name[Month],
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, Day, sep=" ", remove = FALSE) %>%
  left_join(ShoreDistance) %>%
  left_join(sci.names)

trawl.data[is.na(trawl.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- trawl.data[is.na(trawl.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
trawl.data[is.na(trawl.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- trawl.data[is.na(trawl.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

trawl.data[is.na(trawl.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- trawl.data[is.na(trawl.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
trawl.data[is.na(trawl.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- trawl.data[is.na(trawl.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

trawl.data$Mid.Lat.DD<-(trawl.data$BEG_LATITUDE_DD+trawl.data$END_LATITUDE_DD)/2
trawl.data$Mid.Long.DD<-(trawl.data$BEG_LONGITUDE_DD+trawl.data$END_LONGITUDE_DD)/2

##Replace zeros which are known to be wrong to na for all variables other than SPECIES, NUM, WEIGHT
trawl.data$TIME <- replace(trawl.data$TIME, trawl.data$TIME == 0, NA)             
trawl.data$TOW_TIME <- replace(trawl.data$TOW_TIME, trawl.data$TOW_TIME == 0, NA)             
trawl.data$FISHING_DEPTH <- replace(trawl.data$FISHING_DEPTH, trawl.data$FISHING_DEPTH == 0, NA)
trawl.data$SURF_TEMP <- replace(trawl.data$SURF_TEMP, trawl.data$SURF_TEMP == 0, NA)
trawl.data$BEG_SURF <- replace(trawl.data$BEG_SURF, trawl.data$BEG_SURF == 0, NA)
trawl.data$END_SURF <- replace(trawl.data$END_SURF, trawl.data$END_SURF == 0, NA)
trawl.data$BEG_BOTTOM <- replace(trawl.data$BEG_BOTTOM, trawl.data$BEG_BOTTOM == 0, NA)
trawl.data$END_BOTTOM <- replace(trawl.data$END_BOTTOM, trawl.data$END_BOTTOM == 0, NA)

trawl.data$STATE <- replace(trawl.data$STATE, trawl.data$STATE == 'E', 'ONT.E')
trawl.data$STATE <- replace(trawl.data$STATE, trawl.data$STATE == 'W', 'ONT.W')
trawl.data$STATE <- str_trim(trawl.data$STATE)
trawl.data$M_UNIT <- str_trim(trawl.data$M_UNIT)

##calculate the mid-point (average) trawl depth
trawl.data$Mid.Depth <- rowMeans(trawl.data[,c("BEG_DEPTH","END_DEPTH")], na.rm=TRUE)

##Calculate an average Surface and bottom temp
trawl.data$Surface.Temp <- rowMeans(trawl.data[,c("SURF_TEMP", "BEG_SURF", "END_SURF")], na.rm = TRUE)
trawl.data$Bottom.Temp <- rowMeans(trawl.data[,c("BEG_BOTTOM", "END_BOTTOM", "TEMP_BEG_BOTTOM", "TEMP_END_BOTTOM")], na.rm = TRUE)


##Replace FISHING_DEPTH with END_DEPTH for bottom trawls and keep as is for mid-water trawls
trawl.data <- trawl.data %>%
  mutate(FISHING_DEPTH = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ Mid.Depth, 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ FISHING_DEPTH))

##add country based on states
trawl.data <- trawl.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'ONT.E'  ~ "Canada",
    STATE == 'ONT.W'  ~ "Canada"))


trawl.effort <- trawl.data %>%
  select(OP_ID, Date, Date1, Day, Week, Month, Month.short, Month.long, Year, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, FISHING_DEPTH, ShoreDistance) %>%
  distinct(OP_ID, .keep_all = TRUE) 


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)


#######################################################################################################################
##Join age1 table with effort data
age1 <- age1 %>%
  drop_na() %>%
  select(OP_ID, SPECIES, GENETICS, Length) %>%
  mutate(Match = case_when(
    SPECIES == GENETICS  ~ "Yes", 
    SPECIES != GENETICS  ~ "No")) %>%
  mutate(Comparison = case_when(
    SPECIES == 202 & GENETICS == 202 ~ "Cisco/Cisco",
    SPECIES == 202 & GENETICS == 204 ~ "Cisco/Bloater",
    SPECIES == 202 & GENETICS == 206 ~ "Cisco/Kiyi",
    SPECIES == 202 & GENETICS == 208 ~ "Cisco/Shortnose",
    SPECIES == 202 & GENETICS == 218 ~ "Cisco/Unknown",
    SPECIES == 204 & GENETICS == 202 ~ "Bloater/Cisco",
    SPECIES == 204 & GENETICS == 204 ~ "Bloater/Bloater",
    SPECIES == 204 & GENETICS == 206 ~ "Bloater/Kiyi",
    SPECIES == 204 & GENETICS == 208 ~ "Bloater/Shortnose",
    SPECIES == 204 & GENETICS == 218 ~ "Bloater/hybrid",
    SPECIES == 206 & GENETICS == 202 ~ "Kiyi/Cisco",
    SPECIES == 206 & GENETICS == 204 ~ "Kiyi/Bloater",
    SPECIES == 206 & GENETICS == 206 ~ "Kiyi/Kiyi",
    SPECIES == 206 & GENETICS == 208 ~ "Kiyi/Shortnose",
    SPECIES == 206 & GENETICS == 218 ~ "Kiyi/hybrid")) %>% 
  drop_na(GENETICS) %>%
  left_join(trawl.effort) %>%
  mutate(GENETICS_NAME = case_when(
    GENETICS == 202 ~ "Cisco",
    GENETICS == 204 ~ "Bloater",
    GENETICS == 206 ~ "Kiyi",
    GENETICS == 208 ~ "Shortnose Cisco", 
    GENETICS == 218 ~ "ciscoe hybrid",
    GENETICS == 217 ~ "unidentified ciscoe")) 

age1$SPECIES<-as.factor(age1$SPECIES)
age1$GENETICS<-as.factor(age1$GENETICS)

age1 <- age1 %>%
  left_join(sci.names) %>%
  subset(Length < 200) 
  

  
###########################################################################################################
##Revised ship-genetics comparison plot
age1.genetics.sum <- age1 %>%
  group_by(GENETICS_NAME) %>%
  tally() %>%
  ungroup() 

#%>%
#  subset(GENETICS_NAME != "Unknown")

age1 <- age1 %>%
  left_join(age1.genetics.sum)
  
ggplot(subset(age1, GENETICS != 217 &  SPECIES != 217), aes(x=GENETICS_NAME, fill=COMMON_NAME)) +
  geom_bar(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.1) ),  
            stat="count", position=position_dodge(0.9), vjust=-0.1, size = 8, family = 'serif') +  
  geom_text(aes(x=GENETICS_NAME, y=-0.04, label = n), size=10, family='serif') +  
  geom_text(label = "n =", x = 0.6,  y = -0.04,  size=10, family='serif') +  
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette="Pastel1", name = "Ship identification") +
  theme(legend.position = c(.2, .85)) + 
  labs(y='Ship identification proportion', x='Genomic Identification',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification') +
  scale_y_continuous(labels = percent) 

ggsave(here('Plots','Genetics.Ship.IDCompare.png'), dpi = 300, width = 40, height = 20, units = "cm")   

##Color code bars by ship identification
ggplot(subset(age1, GENETICS != 217 &  SPECIES != 217), aes(x=Length, fill = COMMON_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1', name = "Ship identification") +
  theme(legend.position = 'bottom') +
  labs(y='Count', x='Length (mm)',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~GENETICS_NAME, scales = "free_y")

ggsave(here('Plots','Genetics.Ship.IDComparebyLength.png'), dpi = 150, width = 40, height = 20, units = "cm")  

##Color code bars by ship identification with "Unknown putative hybrids"
ggplot(age1, aes(x=Length, fill = COMMON_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1', name = "Ship identification") +
  theme(legend.position = "bottom") + #c(0.8, 0.55)) +
  labs(y='Count', x='Length (mm)',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~GENETICS_NAME, nrow = 4, scales = "free_y")

ggsave(here('Plots','Genetics.Ship.IDComparebyLengthUNKNOWNS.png'), dpi = 150, width = 40, height = 20, units = "cm")  

#################################################################################################################
##Age-0 species length distributions 2022

ggplot(subset(age1, Year == 2022 & GENETICS != 217), aes(x=Length, fill = GENETICS_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1') + 
  theme(legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(y='Count', x='Length (mm)',
       title='2022 Age-0 and Age-1 Lake Superior Ciscoe Lengths',
       subtitle='Identifications based on genomics', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) 

ggsave(here('Plots','2022Age-0Lengths.png'), dpi = 150, width = 40, height = 20, units = "cm")  

#Includes Unknowns, putative hybrids
ggplot(age1, aes(x=Length, fill = GENETICS_NAME)) +
  geom_bar() +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  scale_fill_brewer(palette = 'Pastel1') + 
  theme(legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(y='Count', x='Length (mm)',
       title='2022 Age-0 Lake Superior Ciscoe Lengths',
       subtitle='Identifications based on genomics', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) 

ggsave(here('Plots','2022Age-0LengthsUNKNOWNS.png'), dpi = 150, width = 40, height = 20, units = "cm")  


####Age-0 and Age-1 Distance from shore
#Includes Unknowns, putative hybrids
ggplot(subset(age1, Length <= 100), aes(x=ShoreDistance/1000, y=Length, fill = GENETICS_NAME)) +
  geom_point(shape = 21, size = 5) +
  plot_theme+
  scale_fill_brewer(palette = 'Pastel1') + 
  theme(legend.title = element_blank(), 
        legend.direction = 'horizontal',
        legend.position = 'bottom') +
  labs(y='Length (mm)', x='Distance from shore (km)',
       title='2022 Lake Superior Ciscoe Spatial Distribution',
       subtitle='Identifications based on genomics', 
       caption=ann_data_access) +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(breaks = pretty_breaks()) 

ggsave(here('Plots','2022Age0.DistanceShore.png'), dpi = 150, width = 40, height = 20, units = "cm")  

###################################
##Age-0 genetically identified fish, length by week

age0.dates <- age1 %>%
  select(Date, Year, Month, Week, Month.short, Month.long, Date1) %>%
  distinct(Date, .keep_all = TRUE)


##Length by yolksac by week
age0.dailylengths <- age1 %>%
  drop_na(GENETICS, Length) %>%
  subset(GENETICS == '202' |
           GENETICS == '204' |
           GENETICS == '208' |
           GENETICS == '206' & Length <120) %>%
  subset(Length <=80 & Month <=7 | Month >=8) %>%
  group_by(Date, GENETICS_NAME) %>%
  summarise(n = n(), 
            min.Length = min(Length), max.Length = max(Length),
            mean.Length = mean(Length), median.Length = median(Length),
            sd.Length = sd(Length), se.Length = sd.Length/sqrt(n)) %>%
  left_join(age0.dates)


## Ciscoe growth rates
genetics.names <- age1 %>%
  select(GENETICS, GENETICS_NAME) %>%
  distinct(GENETICS, .keep_all = TRUE)

ciscoe.growth.data <- age1 %>% 
  drop_na(GENETICS, Length) %>%
  subset(GENETICS == '202' |
           GENETICS == '204' |
    #       GENETICS == '208' |
           GENETICS == '206' & Length <120) %>%
  subset(Length <=80 & Month <=7 | Month >=8) %>%
  subset(Year == 2022) %>%
  left_join(genetics.names)



ciscoe.growth.sum <- ciscoe.growth.data %>%
  group_by(GENETICS_NAME) %>% 
  do({
    mod = lm(Length ~ Date, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  }) %>%
  mutate(Slope = round(Slope, 2),
         text1 = "mm per day") %>%
  unite("rate", GENETICS_NAME, Slope, sep = ", ", remove = FALSE) %>%
  unite("Legend", rate, text1, sep = " ", remove = FALSE)

ciscoe.growth.data <- ciscoe.growth.data %>%
  left_join(ciscoe.growth.sum)

ggplot(ciscoe.growth.data, 
       aes(x=as.Date(Date), y = Length, color = Legend, fill = Legend)) +
  geom_jitter(shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", size = 2, se = FALSE) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(fill=guide_legend(title="Mean late summer growth rate"),
         color=guide_legend(title="Mean late summer growth rate")) +
  scale_fill_brewer(palette="Pastel1") +
  scale_colour_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title.align=0.5,
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.17, .8)) + 
  labs( x='Date', 
        y='Length (mm)',
        title='2022 Lake Superior Age-0 Ciscoe Growth',
        subtitle='Identifications based on genomics', 
        caption=ann_data_access)


ggsave(here('Plots','2022Age0CiscoeMeanDailyGrowth.png'), dpi = 150, width = 40, height = 20, units = "cm")  



ggplot(subset(age0.dailylengths, Year == 2022 & Month >=8), 
       aes(x=as.Date(Date), y = mean.Length, color = GENETICS_NAME)) +
  geom_pointrange(data=subset(age0.dailylengths, Year == 2022 & Month >=8), 
                  aes(ymin = min.Length, ymax=max.Length, color= GENETICS_NAME), 
                  size=1.5, position=position_dodge2(width=0.5, padding=0.5)) +
  geom_smooth(data=subset(age0.dailylengths, Year == 2022 & Month >=8), 
              aes(x=as.Date(Date), y=mean.Length, color = GENETICS_NAME), size = 2, se = FALSE) +
  geom_text_repel(data = subset(age0.dailylengths, Year == 2022 & Month >=8), 
                  aes(x=as.Date(Date), y=mean.Length, label = n), 
                  nudge_x = 0, nudge_y = -0.5, segment.color = NA, size=8, family='serif', color="black") +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(8)) +
  theme_bw() +
  plot_theme +
  guides(color=guide_legend(title="Species")) +
  scale_color_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'), 
        legend.title = element_blank(),
        legend.text = element_text(size=24, family='serif'), 
        legend.position = c(.1, .85)) + 
  labs( x='Date', 
        y='Length (mm), mean & range',
        title='2022 Lake Superior Age-0 Ciscoe Lengths',
        subtitle='Identifications based on genomics', 
        caption=ann_data_access)


ggsave(here('Plots','2022Age0CiscoeMeanDailyLength.png'), dpi = 150, width = 40, height = 20, units = "cm")  

##################################################################################################################
#########################################################################################################################
##2022 age0 and age1 map genetics identifications
##################################################################################################################
########
##Pie map showing match - no match locations
genetics.sites <- age1 %>%
  distinct(OP_ID, .keep_all = TRUE)

genetics.map <- age1 %>%
  group_by(OP_ID, GENETICS_NAME, Match) %>%
  tally() %>%
  pivot_wider(names_from = Match, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  right_join(genetics.sites) %>%
  select(-Match) %>%
  mutate(Fish = Yes + No)


##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##Genetics sampling sites
ggplot(genetics.map) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(data=genetics.map, aes(x=Mid.Long.DD, y=Mid.Lat.DD, size = Fish), color = "blue") +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.1, .8)) + 
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Genetics Locations')

ggsave(here('Plots/IDcompareMap.png'), 
       height=20, width=40, dpi=300, units='cm')

###Match Yes - No Map
ggplot(subset(genetics.map, GENETICS != 217)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(genetics.map, GENETICS != 217),
                  cols= c("Yes", "No"),
                  pie_scale = 0.75, 
                  legend_name = "Match") +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = c(.8, .3)) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Identifications',
       subtitle='Comparing genomic to ship board identification') +
  facet_wrap(~ GENETICS_NAME, nrow = 2)

ggsave(here('Plots/IDcompareMapBySpecies.png'), 
       height=20, width=40, dpi=300, units='cm')



## pie map for age-0 bloater, cisco, kiyi for current year based on ship IDs
genetics.map.pies <- age1 %>%
  subset(Month >= 7) %>%
  subset(Length < 150) %>%
  group_by(OP_ID, Month, GENETICS_NAME) %>%
  tally() %>%
  pivot_wider(names_from = GENETICS_NAME, values_from = n, values_fill = 0) %>%
  ungroup() %>%
  left_join(trawl.effort) %>%
  mutate(Group = case_when(
    Month == 7 ~ 'July Bottom Trawl Survey',
    Month >=8  ~  'August-October Mid-Water Trawl Survey'))

##LS outline map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(genetics.map.pies) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=genetics.map.pies,
                  cols= c("Bloater", "Cisco", "Kiyi", "Shortnose Cisco", "ciscoe hybrid"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.2, .85),
        legend.text=element_text(size=18, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-0 Ciscoe Genetic Samples',
       subtitle='Collections made July-October 2022')

ggsave(here('Plots/2022Age0_ciscoes_genetics_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


genetics.map.pies$Group <- factor(genetics.map.pies$Group,
                        levels = c("July Bottom Trawl Survey",
                        "August-October Mid-Water Trawl Survey"))

ggplot(genetics.map.pies) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=genetics.map.pies,
                  cols= c("Bloater", "Cisco", "Kiyi", "Shortnose Cisco", "ciscoe hybrid"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=24, family='serif'),
        axis.text=element_text(size=24, family='serif'),
        strip.text=element_text(size=24, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-0 and Age-1 Ciscoe Genetic Samples',
       subtitle='Collections made July-October 2022') +
  facet_wrap(~Group, nrow = 2)


ggsave(here('Plots/2022Age0_ciscoes_genetics_piemap_survey.png'), 
       height=40, width=40, dpi=300, units='cm')

############################################################################################################
##Compare match by trawl sample size
age1.trawl <- age1 %>%
  group_by(OP_ID, Match) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = Match, values_from = n, values_fill = 0)

ggplot(age1.trawl) +
  aes(x=No+Yes, y = (No/(Yes+No)) *100) +
  geom_point(size = 6, color = 'olivedrab', alpha = 0.8)+ 
  geom_smooth(se = FALSE, color = 'black') +
  theme_bw() +
  map_theme +
  theme(legend.position = c(.1, .85)) + 
  labs(x='Ciscoe trawl catch', y='% not matching',
       title='Lake Superior Ciscoe Identification')

ggsave(here('Plots/IDbyTrawlCatch.png'), 
       height=20, width=40, dpi=300, units='cm')



##2022 Age-0 lengths combine with larval lengths to look at rapid growth

##Make larval effort and larval length file and combine
larval.lengths <- larval %>%
  select(OP_ID, SPECIES, Length, YolkSac) %>%
  rename(LENGTH = Length) %>%
  subset(SPECIES == '202' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '217') %>%
  left_join(neuston.effort) %>%
  subset(Year == 2022)
  

###########################
##load Fish Lengths and effort data for bottom and mid-water trawls

raw.lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.lengths<-subset(raw.lengths, EXP_N>0)
raw.lengths$SPECIES<-as.factor(raw.lengths$SPECIES) 
reprows<-rep(1:nrow(raw.lengths), raw.lengths$EXP_N)
lengths <- raw.lengths[reprows,] %>%
  as.data.frame()

lengths.22 <- lengths %>%
  select(OP_ID,SPECIES,LENGTH) %>%
  subset(SPECIES == '202' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '217') %>%
  left_join(trawl.effort) %>%
  subset(Year == 2022 & Month >7) %>%
  bind_rows(larval.lengths) %>%
  subset(LENGTH < 130)
            
lengths.22.effort <- lengths.22 %>%
  select(-SPECIES,-LENGTH, -YolkSac) %>%
  distinct(OP_ID, .keep_all = TRUE)
  
  
lengths.22.mean <- lengths.22 %>%
  group_by(Date) %>%
  summarise(n = n(), 
            min.Length = min(LENGTH), max.Length = max(LENGTH),
            mean.Length = mean(LENGTH), median.Length = median(LENGTH),
            sd.Length = sd(LENGTH), se.Length = sd.Length/sqrt(n)) %>%
  ungroup() %>% 
  right_join(lengths.22.effort) %>%
  distinct(Date, .keep_all = TRUE)



##Plot all 2022 length data, fish <130 mm
############
ggplot(lengths.22, aes(x=as.Date(Date), y = LENGTH)) +
  geom_jitter(colour = 'pink') +
  geom_point(data = lengths.22.mean, aes(x= as.Date(Date), y = mean.Length), color = 'lightskyblue', size = 5) +
  geom_smooth(se=FALSE, colour = 'lightskyblue') +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif')) +
  labs(x='Date', 
       y='Length (mm)',
       title='Lake Superior 2022 Age-0 Ciscoe Lengths',
       subtitle='Data from surface and mid-water trawls',
       caption=ann_data_access)

ggsave(here('Plots','2022_CiscoeLengths_byDate.png'), dpi = 300, width = 40, height = 20, units = "cm")  


############
ggplot(lengths.22.mean, aes(x=as.Date(Date), y = mean.Length)) +
  geom_pointrange(data=lengths.22.mean, aes(ymin = min.Length, ymax=max.Length), size = 1.5, color = 'olivedrab', alpha=0.8) +
  geom_smooth(data=lengths.22, aes(x=as.Date(Date), y = LENGTH), se=FALSE, colour = 'black') +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif')) +
  labs(x='Date', 
       y='Length (mm)',
       title='Lake Superior 2022 Age-0 Ciscoe Lengths',
       subtitle='Data from surface and mid-water trawls',
       caption=ann_data_access)

ggsave(here('Plots','2022_DailyCiscoeLengths.png'), dpi = 300, width = 40, height = 20, units = "cm")  



###length map
ggplot(subset(lengths.22, Week >=30)) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_jitter(subset(lengths.22, Week >=30), 
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = LENGTH, fill = LENGTH), shape = 21) +  
#  geom_text(mapping=aes(Mid.Long.DD, Mid.Lat.DD, label = CouldDo), nudge_x = -0.1, nudge_y = 0.1, size = 6, family='serif')+  
  scale_fill_viridis(name = "Length", option = "C", alpha=0.6) +
  theme_bw() +
  map_theme +
  scale_size_continuous(name = "Length") +
  guides(fill= guide_legend(), name = "Length", 
         size = guide_legend(), name = "Length") +
  theme(legend.title.align = 0.5, 
        legend.position = c(.15, .8),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='2022 Age-0 Coregonus Lengths',
       subtitle = 'August-October', 
       caption=ann_data_access) 

ggsave(here('Plots/2022Age0.Length.Map.png'), 
       height=20, width=40, dpi=300, units='cm')


###############################################################################################################
##Get BT temperature data

##load data 
BTdata1 <- read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTData")

BTdata1 <- BTdata1 %>%
  select(Sample, Depth, Temperature)
  
  
BTdata2 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample")
BTdata2$Date<-as.character(BTdata2.data$Date)
BTdata2$Date<-parse_date(BTdata2$Date, format='%d-%b-%y')

BTdata2 <- BTdata2 %>%
  select(Sample, Year, Date, 'd<3mT', ChlSurf, 'Chl3m') %>%
  subset(Year == 2011 |
           Year == 2016 |
           Year == 2022) %>%
  rename(Temperature = 'd<3mT')  %>%
  mutate(Month = month(Date)) %>%
  subset(Month >5) %>%
  subset(Month <11) %>%
  mutate(Group = case_when(
    Month == 6 ~ 'June', 
    Month == 7 ~ 'July', 
    Month >7 ~ 'Aug/Sep')) 

BTdata2$Group <- factor(BTdata2$Group,
                        levels = c("June","July","Aug/Sep"))

##ggridges plot with leopard look, median lines inside distribution not very visible
ggplot(BTdata2, aes(x = Temperature, y = as.factor(Year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01,
                               quantile_lines  = TRUE, quantiles = 2,    
                               gradient_lwd = 0, jittered_points = TRUE, point_size = 1)  +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  plot_theme + 
  theme(axis.text.x=element_text(size=16),
        legend.position=c(0.37,0.5),
        legend.title.align = 0.5, 
        legend.text.align = 0.8, 
        legend.direction="vertical",
        legend.text=element_text(size=20, family='serif')) +
  labs(title = "Lake Superior Near Surface Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption=ann_data_access, 
       y = "Year", x = "Water temperature, <3 m deep (C)") +
  facet_wrap(~Group, scales = "free_x")

  ggsave(here('Plots','CSMI_SurfaceWaterTemp.png'), dpi = 300, width = 40, height = 20, units = "cm")  


#######################
#Get daily water temps to put into age-0 fish plot
  
BTdata3 <- BTdata2 %>%
    group_by(Date) %>%
    summarise(mean.Temp = mean(Temperature), 
              mean.SurfChl = mean(ChlSurf),
              mean.Chl3m = mean(Chl3m)) %>%
    ungroup()
  
  
############################################################################################################

BTdata4 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample")
BTdata4$Date<-as.character(BTdata4.data$Date)
BTdata4$Date<-parse_date(BTdata4$Date, format='%d-%b-%y')
BTdata4 <- BTdata4 %>%
    select(Sample, Year, Date, 'd<3mT', ChlSurf, 'Chl3m') %>%
    subset(Year == 2022) %>%
    rename(Temperature = 'd<3mT')  %>%
    mutate(Month = month(Date)) 

BTdata5 <- BTdata4 %>%
  group_by(Date) %>%
  summarise(mean.Temp = mean(Temperature)) %>%
  ungroup() %>%
  left_join(yolk.dates)
  
  
ggplot(BTdata5, aes(x=as.Date(Date), y = yolks, fill = mean.Temp)) +
  geom_point(shape = 21, size=6)   +
  geom_smooth(se=FALSE, colour = 'black') +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_text_repel(data = BTdata5, aes(x=as.Date(Date), y=yolks, label = N+Y), size=6, family='serif') +  
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.8, .8)) +
  labs( x='Date', 
        y='Proportion of catch with yolk sac',
        title='Lake Superior Larval Coregonus Yolk Sac Occurrence and Surface Water Temperatures',
        subtitle='2022 fish yet to be identified', 
        caption=ann_data_access)

ggsave(here('Plots','2022LarvalCiscoesYolkPropTemperatures.png'), dpi = 300, width = 40, height = 20, units = "cm")  

  
  
  
  
  
  
  
############################################################################################################################  

lengths.22.mean.temperature <- lengths.22.mean %>%
    left_join(BTdata3) 
  

############
ggplot(lengths.22.mean.temperature, aes(x=as.Date(Date), y = mean.Length)) +
  geom_pointrange(data=lengths.22.mean.temperature, aes(ymin = min.Length, ymax=max.Length, color = mean.Temp), size = 1.5, ) +
  scale_color_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_smooth(data=lengths.22, aes(x=as.Date(Date), y = LENGTH), se=FALSE, colour = 'black') +
  geom_text(aes(x = as.Date('2022-06-15'), y = 30), label = "Surface trawl, < 1 m deep", size = 8, family='serif', colour = "black") + 
  geom_text(aes(x = as.Date('2022-09-10'), y = 12), label = "Mid-water trawl, 5-20 m deep", size = 8, family='serif', colour = "black") + 
  geom_text(aes(x = as.Date('2022-09-10'), y = 22), label = "Surface trawl, < 1 m deep", size = 8, family='serif', colour = "black") + 
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        legend.position=c(0.2,0.7),
        legend.title.align = 0.3) +
  labs(x='Date', 
       y='Length (mm)',
       title='Lake Superior 2022 Age-0 Ciscoe Lengths',
       subtitle='Average surface to 3 m deep water temperatures the day of fish collection',
       caption=ann_data_access)

ggsave(here('Plots','2022_DailyCiscoeLenghtsWithTemperature.png'), dpi = 300, width = 40, height = 20, units = "cm")  

#show only larvae
ggplot(subset(lengths.22.mean.temperature, Month <8), aes(x=as.Date(Date), y = mean.Length)) +
  geom_pointrange(data=subset(lengths.22.mean.temperature, Month <8), aes(ymin = min.Length, ymax=max.Length, color = mean.Temp), size = 1.5, ) +
  scale_color_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_smooth(data=subset(lengths.22, Month <8),  aes(x=as.Date(Date), y = LENGTH), se=FALSE, colour = 'black') +
  geom_text(aes(x = as.Date('2022-06-01'), y = 8), label = "Surface trawl, < 1 m deep", size = 8, family='serif', colour = "black") + 
 # geom_text(aes(x = as.Date('2022-09-10'), y = 12), label = "Mid-water trawl, 5-20 m deep", size = 8, family='serif', colour = "black") + 
 # geom_text(aes(x = as.Date('2022-09-10'), y = 22), label = "Surface trawl, < 1 m deep", size = 8, family='serif', colour = "black") + 
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        legend.position=c(0.15,0.8),
        legend.title.align = 0.3) +
  labs(x='Date', 
       y='Length (mm)',
       title='Lake Superior 2022 Age-0 Ciscoe Lengths',
       subtitle='Average surface to 3 m deep water temperatures the day of fish collection',
       caption=ann_data_access)

ggsave(here('Plots','2022_DailyCiscoeLenghtsWithTemperature2.png'), dpi = 300, width = 40, height = 20, units = "cm")  


############
ggplot(lengths.22.mean.temperature, aes(x=as.Date(Date), y = mean.Length)) +
# geom_pointrange(data=lengths.22.mean.temperature, aes(ymin = min.Length, ymax=max.Length, color = mean.Chl3m), size = 1.5, ) +
  geom_pointrange(data=lengths.22.mean.temperature, aes(ymin = min.Length, ymax=max.Length, color = mean.SurfChl), size = 1.5, ) +
  scale_color_viridis(name = "chlorophyll a", option = "viridis", alpha=0.6) +
  geom_smooth(data=lengths.22, aes(x=as.Date(Date), y = LENGTH), se=FALSE, colour = 'black') +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(axis.text.x=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        legend.position=c(0.3,0.65),
        legend.title.align = 0.3) +
  labs(x='Date', 
       y='Length (mm)',
       title='Lake Superior 2022 Age-0 Ciscoe Lengths',
       subtitle='Surface chlorophyll a the day of fish collection',
       caption=ann_data_access)

ggsave(here('Plots','2022_DailyCiscoeLenghtsWithChlorophyll.png'), dpi = 300, width = 40, height = 20, units = "cm")  


###########################################################################################################################
###Sydneys work flow

data_path <- here('Data', 'Metedata_GLSC_MEL_withWells.xlsx')
SPwork<-read_excel(data_path, sheet = 'Sample Info')

##change date format into usable form
SPwork$Date<-as.character(SPwork$Date)
#SPwork$Date<-parse_date(SPwork$Date, format='%Y-%b-%d')
SPwork$Date<-as.POSIXct(SPwork$Date, format="%Y-%m-%d")


SPwork.1 <-SPwork %>%
  subset(Date >= "2022-10-01") %>%
  group_by(Date) %>%
  summarise(processed = n()) %>%
  ungroup() %>%
  mutate(Day = weekdays(Date)) 
         

processed = sum(SPwork.1$processed)


ggplot(SPwork.1) +
  geom_point(aes(x=as.Date(Date), y = processed), shape = 21, size=6, fill = "violet")  +
  geom_line(aes(x=as.Date(Date), y = processed), size=1, color="black")  +
  geom_hline(aes(yintercept=mean(processed))) + 
#  annotate("text", x=as.Date("2022-11-10"), y = 50, label = paste(processed), size = 10, family='serif') +
#  annotate("text", x=as.Date("2022-12-23"), y = 50, label = "samples processed through February 3, 2023", size = 10, family='serif') +
  #annotate("text", x=as.Date("2023-1-15"), y = 50, paste(today.date), size = 12, family='serif') +
  scale_x_date(date_labels = "%b %d", breaks = pretty_breaks(10)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  labs( x='Date', 
        y='Samples per day',
        title='Sydney`s Sample Processing')

ggsave(here('Plots','2022LarvalCiscoesProcessed.png'), dpi = 300, width = 40, height = 20, units = "cm")  

SPwork.1 <- SPwork.1 %>%
  mutate(across(Day, factor, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))

ggplot(SPwork.1, aes(x=Day, y=processed)) +
  stat_summary(fun = "mean", geom = "bar", fill = 'violet', alpha = .7) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  theme(legend.position="none",
        axis.text=element_text(size=28, family='serif'),
        axis.title=element_text(size=28, family='serif')) +
  labs( x='Date', 
        y='Samples per day, mean & 95% CI',
        title='Sydney`s Sample Processing')

ggsave(here('Plots','2022LarvalCiscoesProcessedDayofWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  



