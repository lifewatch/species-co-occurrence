library(tidyverse)
library(lubridate)
library(dplyr)
library(tibble)
library(readr)
library(etn)

con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

setwd("~/lifewatch_speciescooccurrence/")
plot_loc <- "~/lifewatch_speciescooccurrence/plots/"

df <- read_csv("csv/DPH_final.csv")
detections_merge <- read_csv("csv/DPH_cod_sb.csv")
df_dol <- read_csv("csv/DPH_dolphins.csv")
df_porpoise2 <- read_csv("csv/DPH_porpoise.csv")

detect <- get_acoustic_detections(acoustic_project_code = "cpodnetwork", start_date ="2018-10-09", end_date = "2021-12-03") 
deploy <- get_acoustic_deployments(acoustic_project_code = "cpodnetwork", open_only = FALSE)

#recode station names
detect$station_name <- recode(detect$station_name,"bpns-Cpowerreefballs-CPOD"="Cpowerreefballs",
                              "bpns-Nauticaena" = "Nauticaena", "bpns-Grafton"="Grafton", "bpns-G-88" ="G-88",
                              "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel",
                              "bpns-Fairplay" = "Fairplay", "bpns-Gardencity" = "Gardencity", "bpns-Belwindreefballs-CPOD"= "Belwindreefballs",
                              "bpns-Birkenfels" = "Birkenfels")

#check detections not within deploy period
detect = as.data.frame(detect)
detect$deploy_date_time = as.Date(deploy$deploy_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect$recover_date_time = as.Date(deploy$recover_date_time[match(detect$deployment_id,deploy$deployment_id)])
detect_deploy_issues <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                                     as.Date(date_time,"UTC") <= recover_date_time | is.na(recover_date_time)~ "YES", 
                                                                                   TRUE ~ "NO")) %>% 
  filter(within_deploy_recover_period=="NO")

#remove detections outside of deployment period
detect <- detect %>% mutate(within_deploy_recover_period = case_when(as.Date(date_time,"UTC") >= deploy_date_time  & 
                                                                       as.Date(date_time,"UTC") <= recover_date_time | is.na(recover_date_time)~ "YES", 
                                                                     TRUE ~ "NO")) %>% 
  filter(within_deploy_recover_period=="YES")

##########################
#ABACUS PLOT OF DETECTIONS
##########################
library(gcookbook)

season <- read_csv("csv/seasons.csv") %>% slice(3:16)

df_plot = df %>% 
  select(
    location_col,
    Diurnal,
    Season,
    day_time,
    por_DPH,
    sb_DPH,
    dol_DPH,
    cod_DPH) 

df_plot$location_col <- recode(df_plot$location_col,"bpns-Cpowerreefballs-CPOD"="Cpowerreefballs",
                               "bpns-Nauticaena" = "Nauticaena", "bpns-Grafton"="Grafton", "bpns-G-88" ="G-88",
                               "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel",
                               "bpns-Fairplay" = "Fairplay", "bpns-Gardencity" = "Gardencity", "bpns-Belwindreefballs-CPOD"= "Belwindreefballs",
                               "bpns-Birkenfels" = "Birkenfels")

#convert df from wide to long
df_long <- gather(as.data.frame(df_plot), species, dph,por_DPH:cod_DPH)
df_long$day_time <- as.Date(df_long$day_time)

#relevel
#df_long$Season  <- factor(df_long$Season , levels=c("September Equinox", "December Solstice", "March Equinox", "June Solstice"))
df_long$species  <- factor(df_long$species , levels=c("cod_DPH", "sb_DPH", "por_DPH", "dol_DPH"))
#df_long$location_col <- fct_relevel(df$location_col, rev)

#for annotation of Seasons
winter_min <- season %>% filter(Season == "December Solstice") %>% select(Date) %>% filter(Date != max(Date)) %>% 
                as.list() %>% lapply(as.Date) %>%  do.call("c",.) 
winter_max <- season %>% filter(Season == "March Equinox") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>% do.call("c",.)
spring_min <- season %>% filter(Season == "March Equinox") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>%  do.call("c",.) 
spring_max <- season %>% filter(Season == "June Solstice") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>% do.call("c",.)
summer_min <- season %>% filter(Season == "June Solstice") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>%  do.call("c",.) 
summer_max <- season %>% filter(Season == "September Equinox") %>% select(Date) %>% filter(Date != min(Date)) %>% 
                  as.list() %>% lapply(as.Date) %>% do.call("c",.)
autumn_min <- season %>% filter(Season == "September Equinox") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>%  do.call("c",.) 
autumn_max <- season %>% filter(Season == "December Solstice") %>% select(Date) %>% as.list() %>% lapply(as.Date) %>% do.call("c",.)

#plot
df_long %>% filter(dph==1) %>% ggplot(aes(x = day_time, y = location_col))+
  geom_point(size=0.5)+theme_classic()+theme(axis.title=element_blank(),strip.text.x = element_text(size = 15),axis.text.x=element_text(size =7, angle=20))+
  scale_x_date(date_labels = "%b-%Y",date_breaks = "3 months") +
  facet_grid(rows=vars(species),labeller = labeller(species = c("por_DPH" = "Harbour porpoise (PAM)",
                                                      "sb_DPH" = "European seabass (AT)",
                                                      "cod_DPH" = "Atlantic cod (AT)",
                                                      "dol_DPH" = "Dolphins (PAM)")))+
  scale_y_discrete(limits=rev)+
  annotate("rect", xmin = winter_min, xmax = winter_max, ymin =-Inf, ymax=Inf,alpha = .2,fill = "#481567FF")+
  annotate("rect", xmin = spring_min, xmax = spring_max, ymin =-Inf, ymax=Inf,alpha = .2,fill = "#FDE725FF")+
  annotate("rect", xmin = summer_min, xmax = summer_max, ymin =-Inf, ymax=Inf,alpha = .2,fill = "#3CBB75FF")+
  annotate("rect", xmin = autumn_min, xmax = autumn_max, ymin =-Inf, ymax=Inf,alpha = .2,fill = "#2D708EFF")

ggsave(paste0(plot_loc,"dph_year.png"), device='png', dpi=300, width=6, height=10)

#############
#MAP STATIONS
#############
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(rgdal)
library(broom)
library(maptools)
library(ggspatial)
library(rgeos)
library(raster) #to define extent of polygon
library(ggsn) #for north arrow and scale bar

stn_list <- as.data.frame(read_csv("csv/stn_list.csv"))

bpns <- readOGR( 
  dsn= "~/lifewatch_speciescooccurrence/shp/belgium_eez/", 
  layer="eez",
  verbose=FALSE)
#bpns_fortified <- tidy(bpns, region = "geoname")
bpns <- spTransform(bpns, CRS("+init=epsg:4326"))

europe <- readOGR( 
  dsn= "~/lifewatch_speciescooccurrence/shp/europe/", 
  layer="Europe",
  verbose=FALSE)
europe <- spTransform(europe, CRS("+init=epsg:4326"))
#eur_fortified <- tidy(europe, region = "NAME") 

#create the clipping polygon
CP <- as(extent(2.2,3.45, 51,51.9), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(europe))

out <- gIntersection(europe, CP)

#active stations

bpan <- ggplot()+
  geom_polygon(data = bpns, aes(x = long, y = lat, group = group), fill="lightblue", alpha=0.75)+
  geom_polygon(data=out, aes(x = long, y = lat, group = group), fill="lightgrey", colour="black")+
  coord_cartesian(xlim = c(2.26, 3.45), ylim = c(51.05,51.9))+ 
  geom_point(data=stn_list, aes(x=longitude, y=latitude), size = 2, color="darkblue")+
  geom_text_repel(data=stn_list, aes(x=longitude, y=latitude, label=location_col), size=4, color = "darkblue")+
  theme_classic()+theme(axis.title = element_blank())+
  ggsn::scalebar(dist = 10, dist_unit="km",st.size=3,transform = TRUE, x.min = 2.2, x.max=3.45, y.min=51, y.max=51.9,location="topleft")
  #annotation_north_arrow(location = "br", which_north = "true", 
                         #pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
                         #style = north_arrow_fancy_orienteering)

bpan + coord_quickmap()

library(grid)
#inset map
inset <- ggplot() + 
  geom_polygon(data=eur_fortified, aes(x = long, y = lat, group = group), fill="darkgrey")+
  coord_cartesian(xlim = c(-10.878, 20), ylim = c(36.195,55.597))+
  geom_rect(aes(xmin = 2.2, xmax = 3.45, ymin = 51.05, ymax = 51.9), color = "red", fill = NA)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.grid = element_blank())
  
print(inset, vp = viewport(0.75, 0.85, width = 0.25, height = 0.25))


###################
#PLOT DATA ACTIVITY
###################

#---visualize data availability
df$location_col <- fct_relevel(df$location_col, rev)

df_activity <- df %>% group_by(location_col,day_time) %>% summarise(PAM = if_else((!is.na(por_DPH)|!is.na(dol_DPH)), 1,0),
                                                                    AT = if_else((!is.na(sb_DPH)|!is.na(cod_DPH)), 1,0))

#convert to long
df_activity <- gather(df_activity, technique, activity, PAM:AT, factor_key=TRUE) %>% filter(activity==1)

df_activity %>% 
  ggplot(aes(as.Date(day_time), location_col, colour = technique)) +
  geom_point(size = 0.7)  + scale_x_date(date_labels = "%Y")+ 
  ggtitle("AT & PAM Data Availability") +theme(axis.text.y=element_text(size=12), axis.text.x=element_text(size=9), axis.title=element_blank())

df_activity$location_col <- recode(df_activity$location_col, "bpns-Belwindreefballs"="Belwindreefballs","bpns-Cpowerreefballs"="Cpowerreefballs",
                                   "bpns-Cpowerreefballs-CPOD"="Cpowerreefballs", "bpns-Nauticaena" = "Nauticaena", "bpns-G-88" ="G-88",
                                   "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel","bpns-Grafton"="Grafton",
                                   "bpns-Gardencity" = "Gardencity", "bpns-Belwindreefballs-CPOD"= "Belwindreefballs", "bpns-Birkenfels" = "Birkenfels")

#Table of hours of activity
x <- df_activity %>% group_by(location_col) %>% summarise(PAM=sum(PAM), AT = sum(AT))
write_csv(x, "csv/hours_activity_per_station.csv")

ggplot(df_activity, aes(x = as.Date(day_time), y = location_col)) +
  geom_point(
    aes(color=technique),
    stat = "identity", position = position_dodge(0.8)) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))+
  theme_linedraw()+theme(axis.title = element_blank())

ggsave("plots/ATdata_availability.png", device='png', dpi =300, width=7, height=7)

################################
#PLOT INDIVIDUAL FISH DETECTIONS
################################

df <- read_csv("csv/detections.csv")

df %>% filter(scientific_name=="Gadus morhua") %>% 
  ggplot(aes(x = as.Date(date, format=c("%Y-%m-%d")), y = as.factor(animal_id))) +
  geom_point() +
  theme_linedraw()+ggtitle("Tagged Atlantic cod detected")+
  theme(axis.title.x = element_blank())+labs(y= "animal ID")+
  scale_x_date(date_labels = "%b-%Y",date_breaks = "3 months")

ggsave("plots/cod_individuals.png", device='png', dpi =300, width=10, height=6)

#############################
#HEAT MAP of species detected
#############################

detect$date_hour <- format(detect$date_time,format='%Y-%m-%d %H')

unique_fish <- detect %>% group_by(station_name,scientific_name) %>% summarise(no_individuals = length(unique(animal_id)), DPH = length(unique(date_hour))) %>% 
  filter(scientific_name!="Built-in", station_name!="Fairplay") %>% rename("species"="scientific_name","station"= "station_name") %>% as.data.frame()

#add PAM data

load("cpod_df_20180701_20220801_week_hour.Rdata")
cpod1_df <- as.data.frame(cpod1_df)
cpod1_df[,2] <- as.POSIXct(cpod1_df[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")

cpod1_df$station <- recode(cpod1_df$station,"bpns-Reefballs Belwind"="Belwindreefballs","bpns-Reefballs-cpower"="Cpowerreefballs",
                           "bpns-Cpowerreefballs-CPOD"="Cpowerreefballs", "AP_bpns-Grafton" = "Grafton", 
                           "bpns-Nautica Ena" = "Nauticaena", "bpns-Grafton"="Grafton", "bpns-G88" ="G-88",
                           "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel",
                           "bpns-Gardencity" = "Gardencity", "bpns-Reefballs belwind"= "Belwindreefballs",
                           "bpns-Birkenfels" = "Birkenfels", "AP-bpns-Birkenfels"="Birkenfels", "AP-bpns-Belwind" = "Belwindreefballs", "AP-bpns-Cpower"="Cpowerreefballs")

unique_cpod <- cpod1_df %>% group_by(station,species)  %>% summarise(no_individuals = NA, DPH = sum(dph)) %>% filter(species!="sonar") %>% as.data.frame()
unique_cpod$species <- recode(unique_cpod$species, "NBHF"="Phocoena phocoena", "Dolphins" = "Delphinidae")

write_csv(unique_cet_cpod,"C:/Workspace_2MA1/Thesis/Fish/csv/unique_cet_BPAN.csv")

#organise labels
unique_fish$species <- fct_relevel(unique_fish$species, rev)

#merge 2 datasets
unique_animals <- unique_fish %>% rbind(unique_cpod) %>% filter(species!="Abramis brama",species!="Salmo salar") #filter to show only species we got permission from

ggplot(unique_animals, aes(station, species, fill= DPH)) + 
  geom_tile() + scale_fill_gradient(low="light blue", high="blue", trans="log1p", breaks = c(1000000, 100000,10000,1000,100,10)) +
  geom_text(aes(label = no_individuals), size = 2)+
  theme_classic() + theme(axis.text.x=element_text(size = 9, angle = 90,hjust=0.95,vjust=0.2),axis.text.y = element_text(face="italic"),axis.title = element_blank())  

ggsave("plots/species_stations_heatmap.png", device='png', dpi = 300, width=13, height=7)
