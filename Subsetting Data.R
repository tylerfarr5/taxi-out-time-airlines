###################################################################################################
################## Reading in the data #######################################
###############################################################################

airline.jan <- read.csv("~/Air Transportation/jan.csv")
airline.feb <- read.csv("~/Air Transportation/feb.csv")
airline.mar <- read.csv("~/Air Transportation/march.csv")
airline.apr <- read.csv("~/Air Transportation/april.csv")
airline.may <- read.csv("~/Air Transportation/may.csv")
airline.jun <- read.csv("~/Air Transportation/june.csv")
airline.july <- read.csv("~/Air Transportation/july.csv")
airline.aug <- read.csv("~/Air Transportation/august.csv")
airline.sept <- read.csv("~/Air Transportation/sept.csv")
airline.oct <- read.csv("~/Air Transportation/october.csv")
airline.nov <- read.csv("~/Air Transportation/november.csv")
airline.dec <- read.csv("~/Air Transportation/dec.csv")

air.dat <- rbind(airline.jan, airline.feb, airline.mar, airline.apr, airline.may, airline.jun, airline.july, airline.aug, airline.sept, airline.oct, airline.nov, airline.dec)

#################################################################################################
################ Subset the data ######################################
###########################################################################
library(tidyverse)

temp <- air.dat %>%
  filter(Operating_Airline. %in% c("AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9", "G4", "HA") &
           (Origin %in% c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA") |
              Dest %in% c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA")))

temp <- temp %>% #subset variables that look important
  select(Year, Month, DayOfWeek, FlightDate, Operating_Airline., Origin, Dest, DepDel15, DepDelay, TaxiOut, Distance, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay, DepartureDelayGroups, DistanceGroup, ActualElapsedTime, AirTime, DepTime)

temp <- temp[complete.cases(temp),]

write.csv(temp, "D:/b19/FinalData.csv", row.names=FALSE)
