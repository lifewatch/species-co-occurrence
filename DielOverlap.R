library(overlap)
library(tidyverse)
setwd("~/lifewatch_speciescooccurrence")
df_merged <- as.data.frame(read_csv("csv/DPH_final.csv"))

#filter stations >= 10 hours
location_sb10 = c("bpns-Belwindreefballs-CPOD","bpns-Birkenfels","bpns-Faulbaums","bpns-Gardencity","bpns-Nauticaena")
location_cod10 = c("bpns-Cpowerreefballs-CPOD")
location_dol = c("bpns-Birkenfels","bpns-Buitenratel","bpns-Gardencity","bpns-Westhinder" )

Seasons <- c("September Equinox", "December Solstice", "March Equinox", "June Solstice")

porObs <- df_merged %>% select(location_col, por_DPH,Season, hour) %>% filter(por_DPH == 1, location_col %in% location_dol, Season==Seasons[1]) #filter(dol_DPH == 1, location_col %in% location_dol, Season==Seasons[1])
porObs <- porObs[grepl("^NA", rownames(porObs))==F,]
porObs$Time.rad <- (as.numeric(as.POSIXct(strptime(porObs$hour, format = "%H", tz = "UTC"))) - as.numeric(as.POSIXct(strptime("0", format = "%S", tz = "UTC")))) / 3600 * (pi/12)
porObs<-na.omit(porObs)

codObs <- df_merged %>% select(location_col, cod_DPH,Season, hour) %>% filter(cod_DPH == 1, location_col %in% location_cod10)
codObs <- codObs[grepl("^NA", rownames(codObs))==F,]
codObs$Time.rad <- (as.numeric(as.POSIXct(strptime(codObs$hour, format = "%H", tz = "UTC"))) - as.numeric(as.POSIXct(strptime("0", format = "%S", tz = "UTC")))) / 3600 * (pi/12)
codObs<-na.omit(codObs)

sbObs <- df_merged %>% select(location_col, sb_DPH,Season, hour) %>% filter(sb_DPH == 1, location_col %in% location_sb10)
sbObs <- sbObs[grepl("^NA", rownames(sbObs))==F,]
sbObs$Time.rad <- (as.numeric(as.POSIXct(strptime(sbObs$hour, format = "%H", tz = "UTC"))) - as.numeric(as.POSIXct(strptime("0", format = "%S", tz = "UTC")))) / 3600 * (pi/12)
sbObs<-na.omit(sbObs)

dolObs <- df_merged %>% select(location_col,dol_DPH,Season, hour) %>% filter(dol_DPH == 1, location_col %in% location_dol, Season==Seasons[1])
dolObs <- dolObs[grepl("^NA", rownames(dolObs))==F,]
dolObs<-na.omit(dolObs)
dolObs$Time.rad <- (as.numeric(as.POSIXct(strptime(dolObs$hour, format = "%H", tz = "UTC"))) - as.numeric(as.POSIXct(strptime("0", format = "%S", tz = "UTC")))) / 3600 * (pi/12)


# Do basic plot with defaults:
est <- overlapEst(dolObs$Time.rad, porObs$Time.rad, kmax = 3,type="Dhat4")
overlapPlot(dolObs$Time.rad, porObs$Time.rad, linet = c(1,1), linec = c("blue", "purple"), linewidth=c(2,2),olapcol="aquamarine",
            rug=TRUE, extend="grey", main="Dolphins & Harbour Porpoise (Autumn)", kmax=3)
legend(x=7.5, y=0.11, c("Dolphins", "Harbour Porpoise"), lty=1, col=c("blue", "purple"), bg="white",cex=0.65)
mtext(line=0.7,cex=0.95, paste("Diel overlap coefficient = ",round(est,2)))
mtext(line=0.00005,cex=0.895, "Birkenfels")

png("CP_cpower_autumn.jpeg",width=300, height=300, units="px")

###---------CALCULATION OF OVERLAP ESTIMATES-----------##

#create empty data frames 
overlapData2 <- data.frame()

sp1 <- dolObs
sp2 <- porObs
name <- "Dolphins & Porpoise"

for (y in 1:length(unique(df_merged$Season))){
  season <- unique(df_merged$Season)[y]
    for (i in 1:length(unique(df_merged$location_col))){
      location_col <- unique(df_merged$location_col)[i]
        if (nrow(sp1[sp1$location_col==location_col & sp1$Season==season,])>0 & nrow(sp2[sp2$location_col==location_col & sp2$Season==season,])>0){
          sp1_ <- sp1[sp1$location_col==location_col & sp1$Season==season, "Time.rad"]
          sp2_ <- sp2[sp2$location_col==location_col & sp2$Season==season, "Time.rad"]
          overlap_coef <- overlapEst(sp1_, sp2_, kmax = 3,type="Dhat4")
          overlap_ <- data.frame(season, location_col, overlap_coef)
          overlapData2 <- rbind(overlapData2, overlap_)
          }}
  }

#change for every species pair run
colnames(overlapData2)[3] <- name 
overlapData <- overlapData2 #RUN THIS ONLY ONCE TO CREATE BASE DATA FRAME; run line below from the 2nd time to merge data frames
overlapData <- merge(overlapData, overlapData2, by = c("season","location_col"), all=TRUE)

write_csv(overlapData,"csv/overlap_coef_stations.csv")


