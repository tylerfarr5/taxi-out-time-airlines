library(ggplot2)
library(dplyr)
library(paletteer)

Air_data <- read.csv("~/FinalData.csv")


airlines <- c("AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9", "G4", "HA")

airports <- c("ATL","DFW","DEN","ORD","LAX","CLT","MCO","LAS","PHX","MIA")

Air_data <- mutate(Air_data, weekend = (DayOfWeek == 6 | DayOfWeek == 7))

aov_weekend <- aov(TaxiOut ~ weekend, data = Air_data)
summary(aov_weekend)

Air_data %>% group_by(weekend) %>% summarize(mean(TaxiOut))

fligner.test(TaxiOut ~ weekend, data = Air_data)

ggplot(data = Air_data, aes(sample = TaxiOut, color = weekend)) + 
  stat_qq() + 
  stat_qq_line()



#Weekday average is higher 

Air_data_avg_day <- Air_data %>%
  group_by(DayOfWeek) %>%
  summarise(mean_Taxi = mean(TaxiOut, na.rm = TRUE))

ggplot(data=Air_data_avg_day, aes(x=DayOfWeek, y=mean_Taxi, fill = DayOfWeek)) +
  geom_col(stat="identity") + 
  geom_text(aes(label = round(mean_Taxi,2), vjust = -.2)) + 
  labs(title = "Average Taxi Out Time by Day of Week", x = "Day of Week", y = "Taxi Out Time")

ggplot(data = Air_data_avg_day, aes(x = factor(DayOfWeek), y = mean_Taxi, fill = factor(DayOfWeek), paletteer_c("grDevices::Purples 3", 30))) +
  geom_col(fill = '#4a3d8d') + 
  theme_classic() +
  labs(x = "Day of Week", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 10)) +
  scale_x_discrete(labels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

Air_data_avg_weekend <- Air_data %>%
  group_by(weekend) %>%
  summarise(mean_Taxi = mean(TaxiOut, na.rm = TRUE))

ggplot(data = Air_data_avg_weekend, aes(x = weekend, y = mean_Taxi, fill = weekend, paletteer_c("grDevices::Purples 3", 30))) +
  geom_col() + 
  theme_classic() +
  labs(x = "", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 3)) +
  scale_x_discrete(labels = c("Weekday","Weekend"))


t.test(TaxiOut ~ weekend, alternative = c("greater"), data = Air_data)

wilcox.test(TaxiOut ~ weekend, alternative = c("greater"), data = Air_data)

#One sided t test - weekday taxi out time is significantly higher

#By month
Air_data_avg_month <- Air_data %>%
  group_by(Month) %>%
  summarise(mean_Taxi = mean(TaxiOut, na.rm = TRUE))

ggplot(data=Air_data_avg_month, aes(x=factor(Month), y=mean_Taxi, fill = factor(Month))) +
  geom_col(stat="identity") + 
  geom_text(aes(label = round(mean_Taxi,2), vjust = -.2)) + 
  labs(x = "Month", y = "Taxi Out Time")


ggplot(data = Air_data_avg_month, aes(x = factor(Month), y = mean_Taxi)) +
  geom_col(fill = '#4a3d8d') + 
  theme_classic() +
  labs(x = "Month", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  #scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 300)) +
  scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))





#Is Q1 significantly greater than average?

Air_data <- mutate(Air_data, Q1 = (Month < 4))
t.test(TaxiOut ~ Q1, alternative = c("less"), data = Air_data)
ggplot(data = Air_data, aes(sample = TaxiOut, color = Q1)) + 
  stat_qq() + 
  stat_qq_line()
wilcox.test(TaxiOut ~ Q1, alternative = c("less"), data = Air_data)

#Taxi times are significantly greater in Q1 


#Getting average taxi time by airline
Air_data_avg_airl <- Air_data %>%
  group_by(Operating_Airline.) %>%
  summarise(mean_Taxi = mean(TaxiOut, na.rm = TRUE))

Air_data_avg_airl <- arrange(Air_data_avg_airl, mean_Taxi)

Air_data_avg_airl <- filter(Air_data_avg_airl, Operating_Airline. %in% airlines)



#Getting average taxi time by airport

Air_data_avg_airp <- Air_data %>%
  group_by(Origin) %>%
  summarise(mean_Taxi = mean(TaxiOut, na.rm = TRUE))

Air_data_avg_airp <- filter(Air_data_avg_airp, Origin %in% airports)

Air_data_avg_airp <- arrange(Air_data_avg_airp, mean_Taxi)



# Basic barplot
library(paletteer)

Air_data_avg_airl <- Air_data_avg_airl %>%
  mutate(budgetYN = (Operating_Airline. %in% budget)) 

Air_data_avg_airl <- Air_data_avg_airl %>% Operating_Airline. %>%
  arrange(budgetYN)

ordered_airlines <- Air_data_avg_airl$Operating_Airline.[order(Air_data_avg_airl$budgetYN)]
Air_data_avg_airl$Operating_Airline. <- factor(Air_data_avg_airl$Operating_Airline., levels = unique(ordered_airlines))

# Create a custom color palette matching the order of airlines
custom_palette <- paletteer_c("grDevices::Purples 3", 15)[ordered_airlines]

ggplot(data = Air_data_avg_airl, aes(x = Operating_Airline., y = mean_Taxi, fill = Operating_Airline.)) +
  geom_col(fill = '#4a3d8d') + 
  theme_classic() +
  labs(x = "Airline", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=16)) +
  scale_x_discrete(labels = c("Hawaiian", "Delta", "American", "United", "Alaska", "Southwest", "Spirit", "Allegiant", "Frontier", "JetBlue"))



ggplot(data = Air_data_avg_airl, aes(x = reorder(Operating_Airline.,budgetYN), y = mean_Taxi)) +
  geom_col('#4a3d8d') + 
  theme_classic() +
  labs(x = "Airline", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18))+
  scale_x_discrete(labels = c("American", "Alaska", "Delta", "Hawaiian", "United", "Allegiant", "Frontier", "JetBlue", "Spirit", "Southwest"))



# Reorder the levels of 'Operating_Airline' based on 'binary_var'
Air_data_avg_airl$Operating_Airline. <- factor(Air_data_avg_airl$Operating_Airline., levels = Air_data_avg_airl$Operating_Airline.[order(Air_data_avg_airl$budgetYN)])

# Create a custom color palette matching the order of airlines
custom_palette <- paletteer_c("grDevices::Purples 3", 15)[Air_data_avg_airl$Operating_Airline.]

ggplot(data = Air_data_avg_airl, aes(x = Operating_Airline., y = mean_Taxi, fill = Operating_Airline.)) +
  geom_col() + 
  theme_classic() +
  labs(x = "Airline", y = "Taxi Out Time") + 
  theme(legend.position = "none") +
  scale_fill_manual(values = custom_palette) + 
  scale_x_discrete(labels = c("American", "Alaska", "Delta", "Hawaiian", "United", "Allegiant", "Frontier", "JetBlue", "Spirit", "Southwest"))



budget_order = c("G4", "F9", "B6", "WN", "NK","AA","DL","UA")
airlines <- c("AA", "AS","DL","HA","UA","G4","F9","B6","NK","WN")


ggplot(data = Air_data_avg_airp, aes(x = Origin, y = mean_Taxi)) +
  geom_col(fill = "#4a3d8d") + 
  theme_classic() +
  labs(x = "Airport", y = "Taxi Out Time (minutes)") + 
  theme(legend.position = "none", axis.title = element_text(size= 20, face= "bold"), axis.text=element_text(size=18)) +
  scale_fill_manual(values = paletteer_c("grDevices::Purples 3", 15))


#Is Phoenix significantly less than average? Are Chicago and Miami significatly more than average? 

#PHX
Air_data <- mutate(Air_data, PHX = (Origin == "PHX"))

wilcox.test(TaxiOut ~ PHX, alternative = c("greater"), data = Air_data)

#Significantly less than average


#ORD
Air_data <- mutate(Air_data, ORD = (Origin == "ORD"))

wilcox.test(TaxiOut ~ ORD, alternative = c("less"), data = Air_data)

#Significantly greater than average


#MIA
Air_data <- mutate(Air_data, MIA = (Origin == "MIA"))

wilcox.test(TaxiOut ~ MIA, alternative = c("less"), data = Air_data)

#Significantly greater than average 


#Budget airlines
budget <- c("G4", "F9", "B6", "WN", "NK")

Air_data <- mutate(Air_data, budgetYN = (Operating_Airline. %in% budget))

t.test(TaxiOut ~ budgetYN, alternative = c("greater"), data = Air_data)

ggplot(data = Air_data, aes(sample = TaxiOut, color = budgetYN)) + 
  stat_qq() + 
  stat_qq_line()

wilcox.test(TaxiOut ~ budgetYN, alternative = c("greater"), data = Air_data)

#Taxi time is less for budget airlines

#ANOVA for airports
airp_lm <- lm(TaxiOut ~ Origin, data = Air_data)
anova(airp_lm)
kruskal.test(TaxiOut ~ Origin, data = Air_data)
#There is significant difference
airp_aov <- aov(TaxiOut ~ Origin, data = Air_data)
tukey_airp = TukeyHSD(airp_aov)
print(tukey_airp)

#ANOVA for airlines
airl_lm <- lm(TaxiOut ~ Operating_Airline., data = Air_data)
anova(airl_lm)
kruskal.test(TaxiOut ~ Operating_Airline., data = Air_data)

#There is significant difference
airl_aov <- aov(TaxiOut ~ Operating_Airline., data = Air_data)
tukey_airl = TukeyHSD(airl_aov)
print(tukey_airl)
#All significantly different from each other except NK-G4 




ggplot(log_model, aes(x = fitted(log_model), y = resid(log_model)))+
  geom_point(color="blue", size=3)+labs(title="Redisual plot",
                                        x="Amount",
                                        y='Residuals')


#Add end of month variable
library(stringr)
Air_data <- Air_data %>%
  rowwise() %>%
  mutate(dayOfMonth = str_split(FlightDate, "-")[[1]][3])

Air_data$dayOfMonth <- as.numeric(Air_data$dayOfMonth)

Air_data$endOfMonth <- ifelse((Air_data$dayOfMonth >= 25) | (Air_data$dayOfMonth <= 5), 1,0)

Air_data$endOfMonth <-  as.factor(Air_data$endOfMonth)

Air_data <- Air_data %>%
  select(-dayOfMonth)

#Federal holiday variable
holidays.2022 <- c("2022-01-01", "2022-01-17", "2022-02-21", "2022-05-30", "2022-06-20", "2022-07-04", "2022-09-05", "2022-10-10", "2022-11-11", "2022-11-24", "2022-12-26")

#actual travel days (incl holiday)
holidays.2022.travel <- c("2022-01-01", "2022-01-02", "2022-01-03", "2022-05-26", "2022-05-27", "2022-05-28", "2022-05-29", "2022-05-30", "2022-06-18", "2022-06-19", "2022-06-20", "2022-07-01", "2022-07-02", "2022-07-03", "2022-07-04", "2022-09-01", "2022-09-02", "2022-09-03", "2022-09-04", "2022-09-05", "2022-11-23", "2022-11-24", "2022-11-27", "2022-12-22", "2022-12-23", "2022-12-25", "2022-12-26", "2022-12-27")

#federal holidays
holidays.2023 <- c("2023-01-02", "2023-01-16", "2023-02-20")

#actual travel days (incl holiday)
holidays.2023.travel <- c("2022-12-31", "2022-01-01", "2022-01-02")

Air_data$fedholiday <- ifelse(Air_data$FlightDate %in% c(holidays.2023, holidays.2022), 1, 0)

Air_data$fedholiday.travel <- ifelse(Air_data$FlightDate %in% c(holidays.2023.travel, holidays.2022.travel), 1, 0)

Air_data$fedholiday <- as.factor(Air_data$fedholiday)
Air_data$fedholiday.travel <- as.factor(Air_data$fedholiday.travel)



air.mod <- lm(log(TaxiOut) ~ sqrt(CarrierDelay) + sqrt(ActualElapsedTime) + endOfMonth + sqrt(WeatherDelay) + sqrt(NASDelay) + sqrt(LateAircraftDelay) + Month + DepDel15 + Operating_Airline. + Origin + weekend + fedholiday.travel + DepTime + DepTime*Origin, data = Air_data)
summary(air.mod)

Air_data%>%
  cor(Origin*DepTime, TaxiOut)


test <- lm(TaxiOut ~ Origin*DepTime, data = Air_data)
summary(test)


avgs <- Air_data %>% 
  group_by(weekend) %>%
  summarize(total = n())



