setwd("C:/Users/usuario/Bats_California/Rbats/T & H")


filenames <- list.files(path = "C:/Users/usuario/Bats_California/Rbats/T & H", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)

library(plyr)
import.list <- llply(filenames, read.csv)

read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename #EDIT
  ret
}




, obsCovs=Dailycov

Julian + Maxhum + Maxtemp + Meanhum + Meantemp+ Minhum + Mintemp + sdhum + sdtemp

import.list <- ldply(filenames, read_csv_filename)

setwd("C:/Users/usuario/Bats_California/Rbats")
library(dplyr)

temp <- subset(import.list, grepl("Tem.csv", import.list$Source))
temp <- temp[,-c(2,4,6)]
colnames (temp) <- c("Date.Time", "Temperature", "ID")
temp<-filter (temp, Temperature >= 0)
temp$ID <- substr(temp$ID, 1, nchar(temp$ID)-7)
temp$ID <- as.factor(temp$ID)

hum <- subset(import.list, grepl("Hum.csv", import.list$Source))
hum <- hum[,-c(2,4,6)]
colnames (hum) <- c("Date.Time", "Humidity", "ID")
hum<-filter (hum, Humidity >= 0)
hum$ID <- substr(hum$ID, 1, nchar(hum$ID)-7)
hum$ID <- as.factor(hum$ID)

hum.and.temp<-left_join(temp, hum)

library(lubridate)

hum.and.temp$Date.Time<-dmy_hms(hum.and.temp$Date.Time)

hum.and.temp$Date.Time<- hum.and.temp$Date.Time+ hours(1)
###################



###############################
library (dplyr)

RESULTS <- read.csv("~/Missouri post doc/Results/RESULTS.csv")
BasalArea <- read.csv("~/Missouri post doc/Results/BasalArea.csv")
BasalArea<-mutate(BasalArea, Area = (((DIAMETER*0.01)/2)^2)*pi)
BasalArea<-summarise(group_by(BasalArea, ID), AreaPerHa=sum(Area)*10)

RESULTS$CANOPY.COVER.N<-as.numeric(RESULTS$CANOPY.COVER.N)
RESULTS$CANOPY.COVER.S<-as.numeric(RESULTS$CANOPY.COVER.S)
RESULTS$CANOPY.COVER.E<-as.numeric(RESULTS$CANOPY.COVER.E)
RESULTS$CANOPY.COVER.W<-as.numeric(RESULTS$CANOPY.COVER.W)
RESULTS$CANOPY.COVER.S<-as.numeric(RESULTS$CANOPY.COVER.S)

RESULTS <- mutate(RESULTS, CANOPY.COVER=((1.04*CANOPY.COVER.N)+(1.04*CANOPY.COVER.E)+(1.04*CANOPY.COVER.W)+(1.04*CANOPY.COVER.S)+(CANOPY.COVER.C*1.04))/5)
RESULTS<-RESULTS[,-c(6:10)]

RESULTS <- mutate(RESULTS, WOODY= 100*((WOOD.N+WOOD.E+WOOD.S+WOOD.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, HERBACIOUS= 100*((HERB.N+HERB.E+HERB.S+HERB.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, GRASS= 100*((GRASS.N+GRASS.E+GRASS.S+GRASS.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, NAKED.SOIL= 100*((NAKED.SOIL.N+NAKED.SOIL.E+NAKED.SOIL.S+NAKED.SOIL.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, ROCKY= 100*((ROCKY.SCREE.N+ROCKY.SCREE.E+ROCKY.SCREE.S+ROCKY.SCREE.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, DOWN.WOOD= 100*((DOWN.WOOD.N+DOWN.WOOD.E+DOWN.WOOD.S+DOWN.WOOD.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS <- mutate(RESULTS, LEAF.LITTER= 100*((LEAF.LITTER.N+LEAF.LITTER.E+LEAF.LITTER.S+LEAF.LITTER.W)/200))
RESULTS<-RESULTS[,-c(6:9)]

RESULTS$START.DATE<-dmy(RESULTS$START.DATE)

RESULTS$START.DATE<-RESULTS$START.DATE + hours(19) + minutes(30)

RESULTS$END.DATE<-dmy(RESULTS$END.DATE)

RESULTS$END.DATE<-RESULTS$END.DATE + hours(6) + minutes(30)

Times <- RESULTS[,c(1,4,5)]

Times$Start.1 <- Times$START.DATE
Times$End.1 <- Times$END.DATE - days (2)
Times$Start.2 <- Times$Start.1 + days (1)
Times$End.2 <- Times$End.1 + days (1)
Times$Start.3 <- Times$Start.2 + days (1)
Times$End.3 <- Times$End.2 + days (1)
Times <- Times[,-c(2,3)]

Daily.cov<-left_join(hum.and.temp, Times)

day1 <- filter(Daily.cov, Date.Time >= Start.1)
day1 <- filter(day1, Date.Time <= End.1)
day1$ID <- as.factor(day1$ID)

library(dplyr)

day1 <-summarise(group_by(day1, ID), mean.temp1 = mean(Temperature), sd.temp1 = sd(Temperature), max.temp1= max(Temperature), min.temp1= min(Temperature), mean.hum1 = mean(Humidity), sd.hum1 = sd(Humidity), max.hum1= max(Humidity), min.hum1= min(Humidity), Julian1 = mean(as.numeric(julian(Date.Time, origin = as.Date ("2015-01-01")))))

day2 <- filter(Daily.cov, Date.Time >= Start.2)
day2 <- filter(day2, Date.Time <= End.2)
day2$ID <- as.factor(day2$ID)

day2 <-summarise(group_by(day2, ID), mean.temp2 = mean(Temperature), sd.temp2 = sd(Temperature), max.temp2= max(Temperature), min.temp2= min(Temperature), mean.hum2 = mean(Humidity), sd.hum2 = sd(Humidity), max.hum2= max(Humidity), min.hum2= min(Humidity), Julian2 = mean(as.numeric(julian(Date.Time, origin = as.Date ("2015-01-01")))))

day3 <- filter(Daily.cov, Date.Time >= Start.3)
day3 <- filter(day3, Date.Time <= End.3)
day3$ID <- as.factor(day3$ID)

day3 <-summarise(group_by(day3, ID), mean.temp3 = mean(Temperature), sd.temp3 = sd(Temperature), max.temp3= max(Temperature), min.temp3= min(Temperature), mean.hum3 = mean(Humidity), sd.hum3 = sd(Humidity), max.hum3= max(Humidity), min.hum3= min(Humidity), Julian3 = mean(as.numeric(julian(Date.Time, origin = as.Date ("2015-01-01")))))

Daily.cov <- left_join(day1, day2)
Daily.cov <- left_join(Daily.cov, day3)
Daily.cov<- data.frame(Daily.cov)
Daily.cov <- Daily.cov[,order(names(Daily.cov))]

(Daily.cov)
summary(Daily.cov)
str(Daily.cov)
