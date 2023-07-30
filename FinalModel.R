###################################################################################################
################## Reading in the data #######################################
###############################################################################
library(tidyverse)
library(AppliedPredictiveModeling)
library(ggplot2)
library(stringr)
library(car)
library(nortest)
library(MASS)
library(grDevices)
library(paletteer)
library(RColorBrewer)

airport <- read.csv("FinalData.csv")


######################################################################
################## EDA ######################################
#############################################################

dim(airport)
str(airport)
summary(airport)
sum(is.na(airport))

cor(airport[c("TaxiOut", "DepDelay", "Distance", "CarrierDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay", "ActualElapsedTime", "AirTime")])
#AirTime, ActualElapsedTime, and Distance are all highly correlated

###################################################################
########### Data Cleaning ##################
###############################################################

#convert categories to factor
airport$Year <- as.factor(airport$Year)
airport$Month <- as.factor(airport$Month)
airport$DayOfWeek <- as.factor(airport$DayOfWeek)
airport$FlightDate <- as.Date(airport$FlightDate)
airport$Operating_Airline. <- as.factor(airport$Operating_Airline.)
airport$Origin <- as.factor(airport$Origin)
airport$Dest <- as.factor(airport$Dest)
airport$DepDel15 <- as.factor(airport$DepDel15)
airport$DepartureDelayGroups <- as.factor(airport$DepartureDelayGroups)
airport$DistanceGroup <- as.factor(airport$DistanceGroup)

str(airport)

#####################################################
#################### Holidays #####################
####################################################

#New Year's Day, MLK Day, Washington BDay, Memorial Day, Juneteenth, Independence Day, Labor day, Columbus Day, Veteran's Dya, Thanksgiving, Christmas

#federal holidays
holidays.2022 <- c("2022-01-01", "2022-01-17", "2022-02-21", "2022-05-30", "2022-06-20", "2022-07-04", "2022-09-05", "2022-10-10", "2022-11-11", "2022-11-24", "2022-12-26")

#actual travel days (incl holiday)
holidays.2022.travel <- c("2022-01-01", "2022-01-02", "2022-01-03", "2022-05-26", "2022-05-27", "2022-05-28", "2022-05-29", "2022-05-30", "2022-06-18", "2022-06-19", "2022-06-20", "2022-07-01", "2022-07-02", "2022-07-03", "2022-07-04", "2022-09-01", "2022-09-02", "2022-09-03", "2022-09-04", "2022-09-05", "2022-11-23", "2022-11-24", "2022-11-27", "2022-12-22", "2022-12-23", "2022-12-25", "2022-12-26", "2022-12-27")

#federal holidays
holidays.2023 <- c("2023-01-02", "2023-01-16", "2023-02-20")

#actual travel days (incl holiday)
holidays.2023.travel <- c("2022-12-31", "2023-01-01", "2023-01-02")

airport$fedholiday <- ifelse(airport$FlightDate %in% c(holidays.2023, holidays.2022), 1, 0)
airport$fedholiday.travel <- ifelse(airport$FlightDate %in% c(holidays.2023.travel, holidays.2022.travel), 1, 0)

airport$fedholiday <- as.factor(airport$fedholiday)
airport$fedholiday.travel <- as.factor(airport$fedholiday.travel)

str(airport)


######################################################
########### Budget Airlines ######################
###########################################################

budget.airlines <- c("G4", "F9", "B6", "WN", "NK")

airport$budget.airlines <- ifelse(airport$Operating_Airline. %in% budget.airlines, 1, 0)

airport$budget.airlines <- as.factor(airport$budget.airlines)

str(airport)
############################################################
############# End of Month ##########################
###########################################


airport <- airport %>%
  rowwise() %>%
  mutate(dayOfMonth = str_split(FlightDate, "-")[[1]][3])

airport$dayOfMonth <- as.numeric(airport$dayOfMonth)

airport <- airport %>%
  rowwise() %>%
  mutate(endOfMonth = ifelse(dayOfMonth >= 25, 1, 0)) %>%
  mutate(endOfMonth = ifelse(dayOfMonth <= 5, 2, endOfMonth))
  ifelse((airport$dayOfMonth >= 25) | (airport$dayOfMonth <= 5), 1,0)

airport$endOfMonth <-  as.factor(airport$endOfMonth)

airport <- airport[,-25]

#######################################################
############# Weekend #############################
#####################################################

airport$weekend <- ifelse(airport$DayOfWeek %in% c(6,7), 1, 0)

airport$weekend <- as.factor(airport$weekend)

#################################################
##################### Day/Night + TimeOfDay ###################
##############################################

#Create Variable for Departure Hours Night vs Day

airport$DepTime <- formatC(airport$DepTime, width =4, format = "d", flag = "0")
airport$DepTime <- substr(airport$DepTime, start = 1, stop = 2)
airport$DepTime <- as.numeric(airport$DepTime)
airport$DepTime[airport$DepTime == 24] <- 00

airport <- airport %>% 
  rowwise() %>%
  mutate(night = ifelse((DepTime < 06 | DepTime > 19),1,0))


airport <- airport %>%
  rowwise() %>%
  mutate(TimeOfDay = ifelse((DepTime >= 02 & DepTime < 08), "Morning", 
                            ifelse((DepTime >= 08 & DepTime < 14), "Day", 
                                   ifelse((DepTime >= 14 & DepTime < 20), "Evening", 
                                          ifelse((DepTime %in% c(20,21,22,23,24,01)), "Night", NA)))))

#########################################################################
############### Quantitative Graphs ################################
#######################################################################

str(airport) #DepDelay, TaxiOut, Distance, CarrierNASWeatherSecurityLateAircraftDelay, AirTime, ActualElapsedTime,DepTi

ggplot(data = airport, aes(x = Month, y = TaxiOut, fill = Month)) +
  geom_boxplot() +
  labs(x = "Month of Year", y = "Taxi Out Time (minutes)") +
  theme_classic() + 
  scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 12)) +
  theme(legend.position = "none")

ggplot(data = airport, aes(x = TaxiOut)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", alpha = 0.5) +
  geom_density(fill = "purple", alpha = 0.35)+
  labs(x = "Taxi Out Time (minutes)", y = "Density of Taxi Out Times") +
  theme_classic() +
  geom_vline(aes(xintercept=median(TaxiOut), color = "red"), linetype="dashed", linewidth = 1.5) +
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18))

####################################################
############# Statistical Testing - Correlations ################
###################################################
cor.test(airport$NASDelay, airport$TaxiOut) #reject, significant correlation
cor.test(airport$WeatherDelay, airport$TaxiOut)
cor.test(airport$CarrierDelay, airport$TaxiOut)
cor.test(airport$SecurityDelay, airport$TaxiOut)
cor.test(airport$LateAircraftDelay, airport$TaxiOut)
cor.test(airport$Distance, airport$TaxiOut)
cor.test(airport$DepDelay, airport$TaxiOut)
cor.test(airport$ActualElapsedTime, airport$TaxiOut)
cor.test(airport$AirTime, airport$TaxiOut)


####################################################
############# Statistical Testing - Holidays ################
###################################################
#Performing Two Sample T-Test

#Independence Assumed

#Normality
ggplot(data = airport, aes(sample = TaxiOut, color = fedholiday)) +
  stat_qq() +
  stat_qq_line() #does not look normal

ggplot(data = airport, aes(sample = TaxiOut, color = fedholiday.travel)) +
  stat_qq() + 
  stat_qq_line() #does not look normal

wilcox.test(TaxiOut ~ fedholiday, data = airport) #have to go nonparametric
wilcox.test(TaxiOut~fedholiday, data = airport, alternative = 'less') #have to go nonparametric

#Fail to reject, so there is not a significant difference in the median TaxiOut time of planes on/off a holiday
#However, we reject for greater... So non Federal holidays have a significantly greater median TaxiOut time of planes than reg holiday

wilcox.test(TaxiOut~fedholiday.travel, data = airport) #have to go nonparametric
wilcox.test(TaxiOut~fedholiday.travel, data = airport, alternative = 'greater') #have to go nonparametric

#We reject! So there is significant evidence for a difference in the median TaxiOut time of planes on/off a holiday with travel
#Also reject! Significant evidence that the median TaxiOut time of not on a holiday is greater than the median TaxiOut time of planes on a holiday

airport %>%
  group_by(fedholiday) %>%
  summarise(meanTaxiOut = mean(TaxiOut))

airport %>%
  group_by(fedholiday.travel) %>%
  summarise(meanTaxiOut = mean(TaxiOut))

ggplot(data = airport, aes(x = fedholiday, y = TaxiOut, fill = fedholiday)) +
  geom_boxplot() + 
  labs(x = "Federal Holiday", y = "Taxi Out Time (minutes)") +
  scale_x_discrete(labels = c("Non-Holiday", "Holiday")) +
  theme_classic() +
  scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 3)) +
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  coord_flip()


ggplot(data = airport, aes(x = fedholiday.travel, y = TaxiOut, fill = fedholiday.travel)) +
  geom_boxplot(fill = c("#CFCAF5", "#8C81D1")) + 
  labs(x = "Federal Holiday with Travel Days", y = "Taxi Out Time (minutes)") +
  scale_x_discrete(labels = c("Non-Holiday", "Holiday with Travel")) +
  theme_classic() +
 # scale_fill_manual(values = paletteer_c("grDevices::Purples 2", 3)) +
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  coord_flip() 


########################################################
################ Checking for Multicollinearity ########
########################################################

str(airport) # this would be our full model

newMod <-lm(TaxiOut ~ CarrierDelay + WeatherDelay + NASDelay + LateAircraftDelay + SecurityDelay + DepDel15 + Month + Operating_Airline. + Origin + fedholiday.travel + endOfMonth + weekend + ActualElapsedTime + night + TimeOfDay, data = airport)

vif(newMod) #Removed for multicollinearity: AirTime, DepartureDelayGroups, Year, DayOfWeek, FlightDate, DepDelay, Distance, DistanceGroup, budget.airlines

###########################################################
############## train / test split ##############################
################################################################
set.seed(123)

sample <- sample.int(n = nrow(airport), size = floor(.70*nrow(airport)), replace = F)
train <- airport[sample, ]
test  <- airport[-sample, ]

################################################################
############################# Model Selection ################
##################################################################
empty.mod <- lm(TaxiOut ~1, data = train)
full.mod <- lm(TaxiOut ~ CarrierDelay + WeatherDelay + NASDelay + LateAircraftDelay + SecurityDelay + DepDel15 + Month + Operating_Airline. + Origin + fedholiday.travel + endOfMonth + weekend + ActualElapsedTime + night + TimeOfDay, data = train)

forward.sel.mod <- step(empty.mod, scope = list(lower = empty.mod, upper = full.mod),
                direction = "forward", k =2 )

#keeps all variables except SecurityDelay. AIC 2285653

step.sel.mod <- step(empty.mod, scope = list(lower = empty.mod, upper = full.mod),
                direction = "both", k =2)

#keeps all variables except Security Delay as well. Same AIC

################################### #####################
########### Testing Model Assumptions ####################
###########################################################

air.mod <- lm(log(TaxiOut) ~ sqrt(NASDelay) + DepDel15 + Origin + Operating_Airline. + 
                sqrt(ActualElapsedTime) + sqrt(WeatherDelay) + Month + TimeOfDay + 
                weekend + sqrt(CarrierDelay) + fedholiday.travel + endOfMonth + 
                sqrt(LateAircraftDelay) + Operating_Airline.:endOfMonth + Month:TimeOfDay, data = train)

summary(air.mod)

anova(air.mod)

# Normality Check
qqnorm(resid(air.mod)) #before transformation: not normal
qqline(resid(air.mod)) #after transformation: left side skews off, but looks much more normal

ad.test(resid(air.mod)) #test says non normal

boxcox(air.mod) #shows that we need a transformation... Will use 

#Equal Variance + Linearity of Mean
plot(air.mod, 1) #not bad, looks fairly balanced and homoskedastic

cor.test(abs(resid(air.mod)), fitted.values(air.mod), method = "spearman", exact = F)
#cor test still claims heteroskedastic, but we could argue against this

ggplot(data = train, aes(x = ActualElapsedTime , y = resid(air.mod))) + 
  geom_point() #looks so much better after transformation!

ggplot(data = train, aes(x = WeatherDelay, y = resid(air.mod))) +
  geom_point() #looks so much better after transformation!

ggplot(data = train, aes(x = NASDelay, y = resid(air.mod))) +
  geom_point() #looks so much better after transformation!

ggplot(data = train, aes(x = CarrierDelay, y = resid(air.mod))) +
  geom_point() #looks so much better after transformation!

ggplot(data = train, aes(x = LateAircraftDelay, y = resid(air.mod))) +
  geom_point() #looks so much better after transformation!

#Independence Assumed/Correlated Errors?
dwtest(air.mod)
durbinWatsonTest(air.mod)

#Perfect Collinearity
#satisfied earlier, looks good
vif(air.mod) #removes interaction terms cause they obvi cause multicollinearity in this vif test

###################################################
##############Test for Outliers/Influential Observations###############
#####################################################

#Studentized Residuals

train$n.index <- seq(1, nrow(train))

ggplot(air.mod, aes(x = n.index, y = rstudent(air.mod),color = "red")) +
  geom_point() +
  geom_line(y = -3.5) +
  geom_line(y = 3.5)

#Influential Observations

hat.cut <- 2*(3)/nrow(train)

ggplot(air.mod, aes(x = n.index, y = hatvalues(air.mod), color = "orange")) +
  geom_point() +
  geom_line(y = hat.cut)
