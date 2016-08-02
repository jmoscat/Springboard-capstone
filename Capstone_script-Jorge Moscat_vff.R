#Jorge Moscat 
#Capstone project - Draft code
#Note: Clean code was include in the r-markdown file, this file was use as the working file.


install.packages("tseries")
library("tseries")
library("dplyr")
library("tidyr")
library("ggplot2")
library("cluster")
library("NbClust")
energy_data = read.csv("capstone_dataset_large.csv", sep = ";", header = TRUE)
str(energy_data)
head(energy_data)


#energy_data cleaning
#Convert DAY column to a date format
energy_data$DAY <- sapply(energy_data$DAY, toString)
energy_data$DAY <- as.Date(energy_data$DAY, "%Y%m%d")
energy_data$DAY <- as.Date(energy_data$DAY, "%Y%m%d")
#Create additional column that indicates the day of the week
energy_data <- energy_data %>% 
  mutate(DOW = toupper(substr(weekdays(as.Date(DAY)), 1,2)))

#Create additional column that indicates the week in the year

energy_data <- energy_data %>% 
  mutate(week_in_year = strftime(DAY, format = "%W"))

#energy_data Preparation
#Based on some exploratory and preliminary analysis,
#I have realized that I need to create two versions of my energy_datasets reshaping the layout for a more  efficient
#use of memory and better plotting speed.

#energy_dataset-1: Daily view with no hourly detail
#-Get rid of the hour by hour columns and just leave a total consumption on the day and the average consumption per hour

daily_energy_dataset <- energy_data %>% 
  select(-MARKET, -TARGET_TENENCIA_CUPS,-CNAE) %>%
  mutate(AVG_CONS_PER_HR= ACTIVA_HT/24) %>%
  mutate(TOTAL_DAY_CONS = ACTIVA_HT) %>%
  select(-starts_with("ACTIVA_H")) 


#energy_dataset-2: Daily view with hourly detail that need to be 

hourly_energy_dataset <- energy_data %>% 
  select(-MARKET, -TARGET_TENENCIA_CUPS,-CNAE, -ACTIVA_HT) %>%
  gather(key=hour_of_day, value = active_consumption, ACTIVA_H1, ACTIVA_H2,ACTIVA_H3,ACTIVA_H4,ACTIVA_H5,ACTIVA_H6,ACTIVA_H7,ACTIVA_H8,ACTIVA_H9,ACTIVA_H10,ACTIVA_H11,ACTIVA_H12,ACTIVA_H13,ACTIVA_H14,ACTIVA_H15,ACTIVA_H16,ACTIVA_H17,ACTIVA_H18,ACTIVA_H19,ACTIVA_H20,ACTIVA_H21,ACTIVA_H22,ACTIVA_H23,ACTIVA_H24, ACTIVA_H25)


#Count how many total customer do we have:

#a) Total number of customers

energy_data %>% 
  summarise(n_distinct(CLIENT_ID))

#b)  Unique customers by month by Month
energy_data %>% 
  group_by(MONTH) %>%
  summarise(unique_client_count= n_distinct(CLIENT_ID)) %>%
  ggplot(data=., aes(x=factor(MONTH), y = unique_client_count, fill = factor(MONTH))) + geom_bar(stat="identity") +
  geom_text(aes(label = unique_client_count), size = 3, position = position_dodge(0.6),vjust = -0.3) +
  labs(title = "Unique customer per month", x = "Month in 2015", y = "Unique customers") +
  scale_fill_discrete(guide=guide_legend(title = "Months"))

#b) By Region
energy_data %>% 
  group_by(CITY) %>%
  summarise(unique_client_count = n_distinct(CLIENT_ID)) %>%
  ggplot(data=., aes(x=CITY, y = unique_client_count, fill = CITY)) + geom_bar(stat="identity") +
  geom_text(aes(label = unique_client_count), size = 3, position = position_dodge(0.6),vjust = -0.3) +
  labs(title = "Unique customer per CITY", x = "Cities") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#c) Understand how many distinct customer we have by product type
energy_data %>% 
  group_by(PRODUCT) %>%
  summarise(unique_client_count = n_distinct(CLIENT_ID)) %>%
  ggplot(data=., aes(x=PRODUCT, y = unique_client_count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2. Calculate the average consumption per hour 

bw <- diff(range(daily_energy_dataset$TOTAL_DAY_CONS)) / (2 * IQR(daily_energy_dataset$TOTAL_DAY_CONS) / length(daily_energy_dataset$TOTAL_DAY_CONS)^(1/3))
daily_energy_dataset %>%
ggplot(data = ., aes(x = TOTAL_DAY_CONS, fill = PRODUCT)) +
  geom_histogram(binwidth = bw/4) + 
  xlim(c(0,150000))


#5. Since P13, P17 and P30 are the most commun products, lets check
#how customers electricity average usage looks like by hour of the day for these
#three products. To do so, lets calculate the average total consumption by day hour of
#the day, just for the month of january. 

hourly_usage_product <- hourly_energy_dataset %>%
  filter(MONTH == 7, PRODUCT %in% c("P13", "P30", "P17"), hour_of_day != "ACTIVA_H25") %>%
  group_by(PRODUCT, hour_of_day) %>%
  summarise(avg_usage = mean(active_consumption))


hourly_usage_city <- hourly_energy_dataset %>%
  filter(MONTH == 7, PRODUCT %in% c("P13", "P30", "P17"), hour_of_day != "ACTIVA_H25") %>%
  group_by(CITY, hour_of_day) %>%
  summarise(avg_usage = mean(active_consumption))




#To ensure that the x-axis of the following plot shows the hours of the day 
#in the correct order, I am modifying the format of the "hour of the day" column
hourly_usage_product$hour_of_day <- gsub("ACTIVA_H", "", hourly_usage_product$hour_of_day)
hourly_usage_city$hour_of_day <- gsub("ACTIVA_H", "", hourly_usage_city$hour_of_day)


ggplot(data = hourly_usage_product, aes(x = as.integer(hour_of_day), y = avg_usage, group = PRODUCT, shape=factor(PRODUCT), colour=factor(PRODUCT))) + 
  geom_line(size=1) +  geom_point(size=2) + xlab("Hour of the day") + ylab("Avg hourly consumption") + 
  ggtitle("Average hourly consumption for clients in products P13, P30, P17") +
  scale_x_continuous(breaks=seq(0, 25, 1))


ggplot(data = hourly_usage_city, aes(x = as.integer(hour_of_day), y = avg_usage, group = CITY, shape=factor(CITY), colour=factor(CITY))) + 
  geom_line(size=1) +  geom_point(size=2) + xlab("Hour of the day") + ylab("Avg hourly consumption") + 
  ggtitle("Average hourly consumption by City") +
  scale_x_continuous(breaks=seq(0, 25, 1))





energy_data <- energy_data %>% 
  mutate(AVG_CONS_PER_HR= ACTIVA_HT/24)

energy_data %>% 
  group_by(CITY, MONTH) %>%  
  summarise(mean_per_hr = mean(AVG_CONS_PER_HR), n_distinct(CLIENT_ID))%>%
  ggplot(data=., aes(x = factor(MONTH), y = mean_per_hr)) + geom_bar(stat="identity") + facet_grid(. ~ CITY)



#----------------- DEEP DIVE ON Madrid ---------------

#After some preliminary analysis in the whole dataset, we are going to deep dive into better understanding the behaviour of consumers in Madrid
#In this section we will do:
#1. There are more than 30 different energy products in the dataset, so we are going to try to determine in which product type 
#there are more customer and which one show the lowest variability (based on std deviation)

#2. Determine if in Madrid and for customers with the most relevant products, there is clear difference between the
#energy consumption in summer (july) vs. winter (january) due to the usage of AC. 



#Wintetime dataset exploration

#1. Determine for a given month which day of the week is on average the one with highest energy consumption
#take 3 winter months of data

#2. Histogram to determine the distribution of customers total energy consumptions for Wintertime 


#Histogram of Madrid
temp_data_jitter <- 
  daily_energy_dataset %>% filter(CITY == "MADRID", MONTH == "7") %>%
  group_by(PRODUCT, CLIENT_ID) %>%
  summarise(av_hourly_consumption = mean(AVG_CONS_PER_HR))

daily_energy_dataset %>% filter(CITY == "MADRID", MONTH == "7") %>%
  group_by(PRODUCT) %>%
  summarise(av_hourly_consumption = mean(AVG_CONS_PER_HR), std = sd(AVG_CONS_PER_HR), num_clients = n_distinct(CLIENT_ID)) %>%
  ggplot(data=., aes(x = factor(PRODUCT), y = av_hourly_consumption)) +
  geom_jitter(data = temp_data_jitter, mapping=aes(x = factor(PRODUCT), y = av_hourly_consumption, colour = factor(PRODUCT)), alpha = 0.3) +
  geom_errorbar( aes(ymin=av_hourly_consumption-std, ymax=av_hourly_consumption+std),
                 width = .2,                    # Width of the error bars
                 position = position_dodge(.9)) +
  geom_point(shape = 21, size = 3, fill = "white") +
  xlab("Product Type") +
  ylab("Average Hourly Consumption in the Month") +
  ggtitle("Avg. Hourly Consumption per product type with SD error bars")



# By just looking at customers in Product group (P13, P30, P17), lets check if there is 
# any difference in consumption between January and July

daily_energy_dataset %>% 
  filter(CITY == "MADRID", PRODUCT %in% c("P13", "P30", "P17"))  %>%
  ggplot(data = ., aes(x=factor(MONTH), y = TOTAL_DAY_CONS)) + geom_point()  

daily_energy_dataset %>% 
  filter(CITY == "MADRID", PRODUCT %in% c("P13", "P30", "P17"))  %>%
  group_by(PRODUCT, MONTH) %>%
  summarise(av_hourly_consumption = mean(TOTAL_DAY_CONS), std = sd(TOTAL_DAY_CONS))




#------ Difference between winter and summer weather ----- 

#Null hypothesis: u_january - u_may = 0
#Hypothesis: u_jan - u_may > 0
#Significance level  = 95%
#

#First testing whether the two samples have a normal distribution
january <- daily_energy_dataset %>%
  filter(CITY == "MADRID", MONTH == "1", PRODUCT %in% c("P13", "P30", "P17")) %>%
  select(TOTAL_DAY_CONS)
ggplot(january, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,50000))
ggplot(january, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,100))

january_cleaned <- daily_energy_dataset %>%
  filter(CITY == "MADRID", MONTH == "1", PRODUCT %in% c("P13", "P30", "P17")) %>%
  filter(TOTAL_DAY_CONS >= 20) %>%
  select(TOTAL_DAY_CONS)
ggplot(january_cleaned, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,50000))

jarque.bera.test(july_cleaned$TOTAL_DAY_CONS)

july <- daily_energy_dataset %>%
  filter(CITY == "MADRID", MONTH == "7", PRODUCT %in% c("P13", "P30", "P17")) %>%
  select(TOTAL_DAY_CONS)
ggplot(july, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,50000))
ggplot(july, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,100))


july_cleaned <- daily_energy_dataset %>%
  filter(CITY == "MADRID", MONTH == "7", PRODUCT %in% c("P13", "P30", "P17")) %>%
  filter(TOTAL_DAY_CONS >= 20) %>%
  select(TOTAL_DAY_CONS)

ggplot(january_cleaned, aes(x=TOTAL_DAY_CONS)) + geom_density(alpha=.3) + xlim(c(0,50000)) +
  ggtitle("Daily energy usage in July (w/out outliers")



t.test(january, july,) # Clear difference as p-value < 2.2e-16
wilcox.test(january_cleaned$TOTAL_DAY_CONS, july_cleaned$TOTAL_DAY_CONS,  alternative = "less") 




temp_data_jitter <- 
  daily_energy_dataset %>% filter(CITY == "MADRID", PRODUCT %in% c("P13", "P30", "P17")) %>%
  group_by(MONTH, CLIENT_ID) %>%
  summarise(av_hourly_consumption = mean(TOTAL_DAY_CONS))

daily_energy_dataset %>% filter(CITY == "MADRID", PRODUCT %in% c("P13", "P30", "P17")) %>%
  group_by(MONTH) %>%
  summarise(av_hourly_consumption = mean(TOTAL_DAY_CONS), std = sd(TOTAL_DAY_CONS)) %>%
  ggplot(data=., aes(x = factor(MONTH), y = av_hourly_consumption)) +
  geom_jitter(data = temp_data_jitter, mapping=aes(x = factor(MONTH), y = av_hourly_consumption, colour = factor(MONTH)), alpha = 0.3) +
  geom_errorbar( aes(ymin=av_hourly_consumption-std, ymax=av_hourly_consumption+std),
                 width = .2,                    # Width of the error bars
                 position = position_dodge(.9)) +
  geom_point(shape = 21, size = 3, fill = "white") + geom_text(aes(label=round(av_hourly_consumption, 3)),hjust=0, vjust=0, size = 5, fontface = "bold") +
  xlab("Month") +
  ylab("Total Daily Energy consumption") +
  ggtitle("Total Daily Energy consumption by Month SD error bars")




#Now, we see that in July the diffference in energy consumption is significantly higher in summer season, most certainly
#due to the use of AC units. Now, I am going to take a new sample of the large dataset with the following criteria
#products (P13,P30, P17), location = MADRID, MONTHS=  MAY, JUNE, JULY. 
#In addition, I have donwloaded the historical weather data for MAY, JUNE, JULY in Madrid in 2015 and I am going to 
#check if there is a correlation between energy consumption and the weather parameters (temp, humidity, rain). 
#I dedcided to include June too as from a weather perspective it shows a bit more variability in temperatures.
#Hypothesis trying to test -> is there correlation between energy consumption and temperature (i.o.w, use of AC)?
weather_madrid = read.csv("TemperatureData/Madrid_weather_may_jun_jul.csv", sep = ";", header = TRUE)
str(weather_madrid)
weather_madrid$CEST <- sapply(weather_madrid$CEST, toString)
weather_madrid$CEST <- as.Date(weather_madrid$CEST, "%d/%m/%Y")

#New dataset with data from Madrid, for 3 specific products (P13, P30, P17) and for Jan, Feb, March, May, June, July
energy_madrid = read.csv("MadridSubset/Madrid_deep_dive_v2.csv", sep = ";", header = TRUE)


energy_madrid$DAY <- sapply(energy_madrid$DAY, toString)
energy_madrid$DAY <- as.Date(energy_madrid$DAY, "%Y%m%d")
energy_madrid$DAY <- as.Date(energy_madrid$DAY, "%Y%m%d")
#Create additional column that indicates the day of the week
energy_madrid <- energy_madrid %>% 
  mutate(DOW = as.POSIXlt(DAY)$wday)# TO-DO: CONVERT TO NUMBER as.POSIXlt(D)$wday


energy_madrid <- energy_madrid %>% 
  mutate(week_in_year = strftime(DAY, format = "%W"))


daily_energy_madrid <- energy_madrid %>% 
  select(-MARKET, -TARGET_TENENCIA_CUPS,-CNAE) %>%
  mutate(AVG_CONS_PER_HR= ACTIVA_HT/24) %>%
  mutate(TOTAL_DAY_CONS = ACTIVA_HT) %>%
  select(-starts_with("ACTIVA_H")) 

daily_energy_madrid <- daily_energy_madrid %>%
  mutate(SEASON = ifelse(MONTH <= 3, "winter","summer"))


###Histogram to see the distribution of total energy consumption 

#determine optimal number of bins using Freedman-Diaconis rule 


temp_madrid <- daily_energy_madrid %>%
  filter (MONTH <= 2) %>%
  group_by(MONTH, CLIENT_ID) %>%
  summarise(total_energy = mean(AVG_CONS_PER_HR))



bw <- diff(range(temp_madrid$total_energy)) / (2 * IQR(temp_madrid$total_energy) / length(temp_madrid$total_energy)^(1/3))

ggplot(temp_madrid, aes(x=total_energy, fill= factor(MONTH))) + geom_density(alpha=.3)

ggplot(temp_madrid, aes(x=total_energy, fill=SEASON)) + geom_histogram(binwidth= bw, alpha=.5, position="identity")



#Create two subsets: one for winter time (jan, feb, mar) and other for summer time (may, june, july)

daily_energy_madrid_winter <- daily_energy_madrid %>%
  filter(MONTH <= 3)

daily_energy_madrid_summer <- daily_energy_madrid %>%
  filter(MONTH >= 3)







#CORRELATION BETWEEN TEMPERATURE AND ENERGY CONSUMED IN Summer in Madrid

daily_energy_madrid_summer_weather <- inner_join(daily_energy_madrid_summer, weather_madrid,by = c("DAY" = "CEST"))

str(daily_energy_madrid_summer_weather)

sat.mod.energy <- lm(TOTAL_DAY_CONS ~ Max.TemperatureC,
                     data=daily_energy_madrid_summer_weather)

summary(sat.mod.energy)
#Averga energy consumed per hour in the region per day
madrid_avg_hourly_consumption <- daily_energy_madrid_summer_weather %>%
  group_by(DAY) %>%
  summarise(avg_hourly = mean(AVG_CONS_PER_HR), temp = mean(Max.TemperatureC), humidity = mean(Max.Humidity), wind = mean( Max.Wind.SpeedKm.h), precip = mean(Precipitationmm))


#Clear correlation between average hourly consumption and temperature
ggplot(madrid_avg_hourly_consumption, aes(x = avg_hourly , y = temp)) +  geom_point() + geom_smooth(method=lm)

#Clear negative correlation between average hourly consumption and humidity
ggplot(madrid_avg_hourly_consumption, aes(x = avg_hourly , y = humidity)) +  geom_point() + geom_smooth(method=lm)

#...this is trivial as there is a strong negative correlation between temperature and humidity. More temp means less humidity and therefore, higher use of AC
ggplot(madrid_avg_hourly_consumption, aes(x = temp , y = humidity)) +  geom_point() + geom_smooth(method=lm)

sat.mod.energy_2 <- lm(avg_hourly ~ temp,
                     data=madrid_avg_hourly_consumption)
summary(sat.mod.energy_2)
plot(sat.mod.energy_2, which = c(1))

#Residual plots show that at lower levels of energy consumption the fit is skewed and therefore not that relevant. 
#That is because there are a subtantial number of days with low energy consumption that where not affected by changes in
#in temperature. 
#Below the 30 degree line, the energy consumption does not seem to be affected by temperature. We can conclude that
#below 30 degree, clients do not turn their AC units, however above 30 degrees the usage of electricity (i.e. AC) is correlates
#with the temp variation between 30 and 40 degrees. 

#If we take a subset of the days with temp > 30 degrees we see also a strong positive correlation between 
#hourly energy consumption and the temp but also the residual plot looks better as residual do not follow a specific pattern
days_high_temps <- madrid_avg_hourly_consumption %>%
  filter(temp >= 30)
ggplot(days_high_temps, aes(x = avg_hourly , y = temp)) +  geom_point() + geom_smooth(method=lm)

sat.mod.energy_hightemps <- lm(avg_hourly ~ temp,
                               data=days_high_temps)
summary(sat.mod.energy_hightemps)
plot(sat.mod.energy_hightemps, which = c(1))



# ------ CLUSTERING CUSTOMER WEEKLY BEHAVIOUR ----

#Premises #1 -> get rid of any customers with energy consumption total for day = 0
#Premise #2 -> To have a more precise data set without exceptional user behavior I am going to 
#get a new data sets for customers in Madrid from the months of January, Feb, March. These three months have a couple 
#of characteristics that will reduce the impact of out-of-the-norm user behaviour:
#-> No holiday season
#-> No AC consumption due to high temperatures
#-> Since most houses in Madrid have natural gas heaters, the impact of low temperatures in electricity consumption
#would none

clean_madrid_data <- daily_energy_madrid %>%
  filter (SEASON == "winter") %>%
  group_by(CLIENT_ID, DOW) %>%
  summarise(avg_hourly_season = mean(AVG_CONS_PER_HR), std = sd(AVG_CONS_PER_HR))
n_distinct(clean_madrid_data$CLIENT_ID)

#Find out Which customers have no consumption in all three months
clients_cero <- daily_energy_madrid %>%
  filter (SEASON == "winter") %>%
  group_by(CLIENT_ID) %>%
  summarise(avg_hourly_season = mean(AVG_CONS_PER_HR)) %>%
  filter(avg_hourly_season <= 0)
n_distinct(clients_cero$CLIENT_ID)

#Identify those customers from which we don't have a full 3 months of data. Therefore, customers will be included
#if they have 90 datapoints (30days x 3 months)
missing_client_data <- daily_energy_madrid %>%
  filter (SEASON == "winter") %>%
  group_by(CLIENT_ID) %>%
  summarise(readings = n_distinct(DAY)) 
n_distinct(missing_client_data$CLIENT_ID)

#From the density plot we see that there is a significant number of customers that have at least 10-15
#days with out data readings. To have enough population to run our clustering algorithm but at the sametime
#ensure that the customers included have enough data to extrapolate their weekly behavoir, we are going to 
#get rid of all customers with less than 80 days of readings. In addition, we are also getting rid 
#of those customer with 0 energy consumed in the last 3 months
ggplot(missing_client_data, aes(x=readings)) + geom_density(alpha=.3) + xlim(c(70,100)) +
  xlab("Frequency") +
  ylab("Readings per customers") + ggtitle("Distr. of number of readings per customer in dataset")



missing_client_data2 <- missing_client_data %>%
  filter(readings >= 75)
n_distinct(missing_client_data2$CLIENT_ID)

#Get rid of customers with no energy consumption
clean_madrid_data <- clean_madrid_data %>%
  filter (!CLIENT_ID %in% c(clients_cero$CLIENT_ID))

clean_madrid_data <- clean_madrid_data %>%
  filter (CLIENT_ID %in% c(missing_client_data2$CLIENT_ID))
n_distinct(clean_madrid_data$CLIENT_ID)


#Now, after taking the average and std of the average hourly consumption per day of the week for each of
#the customers in scope, I believe it is also important to leave out of our clustering those customers
#that exihibit a lot of weekly variability in their weekly data. To do so, lets check the standard deviation
#by day of the week

ggplot(clean_madrid_data, aes(x=std)) + geom_density(alpha=.3) + xlim(c(0,2000)) + facet_grid (.~DOW)

#zooming in to determine a reasanable cut-off
ggplot(clean_madrid_data, aes(x=std)) + geom_density(alpha=.3) + xlim(c(0,750)) + facet_grid (.~DOW)

#By looking at the frequency plot, we can eye-ball that most of the customers have a std below 
#200 for each day of the week. Therefore, we could consider all the DOWs with higher than 200 std
#as outliers. We should then get rid of customers that have at least one of their days of the week 
#average hourly consumption with standard deviation higher that 200. 

#Lets see which customers have at least one of their DOWs with stad higher than 200. To do so, 
#lets add up their weekly standard deviation and filter out those that their sums of weekly std is higher
#than 1400 (200x7)

outliers <- clean_madrid_data %>% 
  group_by(CLIENT_ID) %>%
  summarize(sum_std = sum(std)) %>%
  filter(sum_std > 1400)
n_distinct(outliers$CLIENT_ID)
#Get rid of outlier clients
clean_madrid_data <- clean_madrid_data %>%
  filter (!CLIENT_ID %in% c(outliers$CLIENT_ID))

#After all cleanup the final number of customers that we are going to try to run through our clustering 
#algorithm is 1109
n_distinct(clean_madrid_data$CLIENT_ID)


clean_madrid_data_c1 <- clean_madrid_data
clean_madrid_data_c2 <- clean_madrid_data
clean_madrid_data_c3 <- clean_madrid_data




#Clustering 1: Clustering algorithm
clean_madrid_data_cluster <- clean_madrid_data %>%
  select(-std) %>%
  spread(DOW, avg_hourly_season)

set.seed(1234)
nc <- NbClust(t, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
fit.km <- kmeans(t, centers = 6)
clusplot(t,fit.km$cluster)


wssplot <- function(data, nc, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="WSS")
}

wssplot(t)


#We see from the cluster plot that there seems to be several oulier instances, so lets reduce a bit our 
#dataset and just include the analysis all that have a very similar weekly energy consumption. Hopefully,
#our clusters would look a bit better 
ggplot(clean_madrid_data, aes(x=avg_hourly_season)) + geom_density(alpha=.3) + xlim(c(0,1200)) 

test_outlier <- clean_madrid_data %>%
  group_by(CLIENT_ID) %>%
  summarise(total = sum(avg_hourly_season))

ggplot(test_outlier, aes(x=total)) + geom_density(alpha=.3) + xlim(c(0,8816.371)) 


std <- sd(test_outlier$t)
average <- mean(test_outlier$t)
d = average - 0.5*std


outliers_energy <- test_outlier %>%
  filter(total <= average - 0.5*std | total >= average + 0.5*std )

clean_madrid_data <- clean_madrid_data %>%
  filter (!CLIENT_ID %in% c(outliers_energy$CLIENT_ID))

n_distinct(clean_madrid_data$CLIENT_ID)


#Clustering 2: Run clustering again with customer with low variability

clean_madrid_data_cluster <- clean_madrid_data %>%
  select(-std) %>%
  spread(DOW, avg_hourly_season)

set.seed(1234)
nc <- NbClust(clean_madrid_data_cluster, min.nc=2, max.nc=20, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
fit.km <- kmeans(clean_madrid_data_cluster, centers = 5)
clusplot(clean_madrid_data_cluster,fit.km$cluster)

clean_madrid_data_cluster$cluster <- fit.km$cluster

clean_madrid_data_results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  group_by(DOW, cluster) %>%
  summarise(dow_avg_consumption = mean(avg_hourly_season), standDev = sd(avg_hourly_season))



clean_madrid_data_results %>% 
  ggplot(., aes(x = DOW, y = dow_avg_consumption, group = cluster, shape=factor(cluster), colour=factor(cluster))) + geom_line(size=1) +  geom_point(size=2) + xlab("Day of the Week (0=sunday)") + ylab("Avg hourly consumption") + # Set axis labels
  ggtitle("Clusters") +  scale_linetype_discrete(name="Sex") + facet_grid( . ~ cluster) + 
  geom_errorbar(aes(x = DOW, ymin = dow_avg_consumption-standDev, ymax = dow_avg_consumption+standDev, color = NULL, linetype = NULL),
                width=.1, position=position_dodge(width = .1))

clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(., aes(x = DOW, y = avg_hourly_season, group = CLIENT_ID, colour=factor(cluster))) + geom_line() +  geom_point(size=2) + facet_grid( . ~ cluster)


clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(.,aes(x=DOW , y=avg_hourly_season)) +
  scale_x_discrete() +
  geom_jitter(aes(colour = cluster, x = DOW), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(colour = cluster), outlier.colour = NA, position = "dodge") +
  facet_wrap(~ cluster)


clean_madrid_data %>% 
  filter (CLIENT_ID == "27577") %>%
  select(avg_hourly_season, DOW) %>%
  ggplot(data = ., aes(x = DOW, y = avg_hourly_season)) + geom_point()

#################!!######################Clustering 1: ##############

ratio_plot <- function(data, nc, seed=1234){
  ratio <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    ratio[i] <- kmeans(data, centers=i)$tot.withinss / (kmeans(data, centers=i)$tot.withinss + kmeans(data, centers=i)$betweenss)
  }
  
  plot(1:nc, ratio, type="b", xlab="Number of Clusters",
       ylab="WSS /(WSS + BSS")
}


clean_madrid_data_c1


clean_madrid_data_cluster <- clean_madrid_data_c1 %>%
  select(-std) %>%
  spread(DOW, avg_hourly_season) 

data_scaled <- clean_madrid_data_cluster[,2:8]
data_scaled <- scale(data_scaled, center = TRUE)

wssplot(data_scaled,20)
ratio_plot(data_scaled, 20)
set.seed(1234)

nb <- NbClust(data_scaled, distance = "euclidean", min.nc = 2,
              max.nc = 6, method = "kmeans", index ="all")

nc <- NbClust(data_scaled, min.nc=2, max.nc=10, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

fit.km_1 <- kmeans(data_scaled, centers = 12)

par(mar = rep(2, 4))
clusplot(data_scaled,fit.km_1$cluster)

#WSS (measure of compactness of clusters -> minimize)
fit.km_1$tot.withinss

#BSS (measure of the distance between clusters -> maximize)
fit.km_1$betweenss




clean_madrid_data_cluster$cluster <- fit.km_1$cluster

clean_madrid_data_results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  group_by(DOW, cluster) %>%
  summarise(dow_avg_consumption = mean(avg_hourly_season), standDev = sd(avg_hourly_season))


clean_madrid_data_results %>% 
  ggplot(., aes(x = DOW, y = dow_avg_consumption, group = cluster, shape=factor(cluster), colour=factor(cluster))) + geom_line(size=1) +  geom_point(size=2) + xlab("Day of the Week (0=sunday)") + ylab("Avg hourly consumption") + # Set axis labels
  ggtitle("Clusters") +  scale_linetype_discrete(name="Sex") + facet_grid( . ~ cluster) + 
  geom_errorbar(aes(x = DOW, ymin = dow_avg_consumption-standDev, ymax = dow_avg_consumption+standDev, color = NULL, linetype = NULL),
                width=.1, position=position_dodge(width = .1))

clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(., aes(x = DOW, y = avg_hourly_season, group = CLIENT_ID, colour=factor(cluster))) + geom_line() +  geom_point(size=2) + facet_grid( . ~ cluster) +
  ggtitle("Avg. Hourly energy consumption for each cluster by DOW")


clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(.,aes(x=DOW , y=avg_hourly_season)) +
  scale_x_discrete() +
  geom_jitter(aes(colour = cluster, x = DOW), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(colour = cluster), outlier.colour = NA, position = "dodge") +
  facet_wrap(~ cluster) +ggtitle("Barplot for each cluster by DOW")


clean_madrid_data %>% 
  filter (CLIENT_ID == "27577") %>%
  select(avg_hourly_season, DOW) %>%
  ggplot(data = ., aes(x = DOW, y = avg_hourly_season)) + geom_point()






######!!!#################################Clustering 2: Reducing variability  ######################


#In this run of the k-means clustering we are going to use a reduce dataset with just customers that have a very similar 
#level of energy consumtion. In other words, customers includes in the datasetspend very similar amounts of energy by week. Doing this, 
#hopefully will help us create clusters based just on the usage pattern per day of the week. 
#additional approach to trim down our dataset and select customers that have very similar levels of energy consumption
test_outlier <- clean_madrid_data_c2 %>%
  group_by(CLIENT_ID) %>%
  summarise(total = sum(avg_hourly_season))

ggplot(test_outlier, aes(x=total)) + geom_density(alpha=.3) + xlim(c(0,8816.371)) 


std <- sd(test_outlier$total)
average <- mean(test_outlier$total)
d = average - 0.5*std


outliers_energy <- test_outlier %>%
  filter(total <= average - 0.5*std | total >= average + 0.5*std )

clean_madrid_data_c2 <- clean_madrid_data_c2 %>%
  filter (!CLIENT_ID %in% c(outliers_energy$CLIENT_ID))

n_distinct(clean_madrid_data_c2$CLIENT_ID)


clean_madrid_data_cluster <- clean_madrid_data_c2 %>%
  select(-std) %>%
  spread(DOW, avg_hourly_season) 

data_scaled <- clean_madrid_data_cluster[,2:8]
data_scaled <- scale(data_scaled, center = TRUE)

wssplot(data_scaled,15)

wssplot(data_scaled,20)
ratio_plot(data_scaled, 20)



set.seed(1234)

fit.km <- kmeans(data_scaled, centers = 10)

par(mar = rep(2, 4))
clusplot(data_scaled,fit.km$cluster)


clean_madrid_data_cluster$cluster <- fit.km$cluster

clean_madrid_data_results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  group_by(DOW, cluster) %>%
  summarise(dow_avg_consumption = mean(avg_hourly_season), standDev = sd(avg_hourly_season))



clean_madrid_data_results %>% 
  ggplot(., aes(x = DOW, y = dow_avg_consumption, group = cluster, shape=factor(cluster), colour=factor(cluster))) + geom_line(size=1) +  geom_point(size=2) + xlab("Day of the Week (0=sunday)") + ylab("Avg hourly consumption") + # Set axis labels
  ggtitle("Clusters") +  scale_linetype_discrete(name="Sex") + facet_grid( . ~ cluster) + 
  geom_errorbar(aes(x = DOW, ymin = dow_avg_consumption-standDev, ymax = dow_avg_consumption+standDev, color = NULL, linetype = NULL),
                width=.1, position=position_dodge(width = .1))

clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(., aes(x = DOW, y = avg_hourly_season, group = CLIENT_ID, colour=factor(cluster))) + geom_line() +  geom_point(size=2) + facet_grid( . ~ cluster)


clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(.,aes(x=DOW , y=avg_hourly_season)) +
  scale_x_discrete() +
  geom_jitter(aes(colour = cluster, x = DOW), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(colour = cluster), outlier.colour = NA, position = "dodge") +
  facet_wrap(~ cluster)


clean_madrid_data %>% 
  filter (CLIENT_ID == "27577") %>%
  select(avg_hourly_season, DOW) %>%
  ggplot(data = ., aes(x = DOW, y = avg_hourly_season)) + geom_point()


ggplot(temp2_madrid, aes(x = factor(DOW), y = avg_hourly_season)) + geom_bar(stat="identity") + facet_grid( . ~ CLIENT_ID)


ggplot(temp2_madrid, aes(x = factor(DOW), y = avg_hourly_season)) + 
  geom_errorbar( aes(ymin=avg_hourly_season-std, ymax=avg_hourly_season+std),
                 width = .2,                    # Width of the error bars
                 position = position_dodge(.9)) +
  geom_point(shape = 21, size = 3, fill = "white")



#Customer went on vacation after 15 of july
daily_energy_madrid %>% 
  filter (CLIENT_ID == "83095") %>%
  select(DAY, TOTAL_DAY_CONS, DOW) %>%
  ggplot(data = ., aes(x = DAY, y = TOTAL_DAY_CONS, col = factor(DOW))) + geom_point()



#Customer with clear increase of energy by AC usage
daily_energy_madrid %>% 
  filter (CLIENT_ID == "84979") %>%
  select(DAY, TOTAL_DAY_CONS, DOW) %>%
  ggplot(data = ., aes(x = DAY, y = TOTAL_DAY_CONS, col = DOW)) + geom_point()


daily_energy_madrid %>% 
  filter (CLIENT_ID == "85548") %>%
  select(DAY, TOTAL_DAY_CONS, DOW) %>%
  ggplot(data = ., aes(x = DAY, y = TOTAL_DAY_CONS, col = DOW)) + geom_point()




#Based on the average consumtpion of these 1109 customer by day of the weeek,lets try to determine which days during these 3 months they 
#were out of their home.

n_distinct(clean_madrid_data$CLIENT_ID)
daily_energy_madrid

customer_in_scope <- daily_energy_madrid %>%
  filter(CLIENT_ID %in% c(unique(clean_madrid_data$CLIENT_ID))) %>%
  filter(SEASON == "winter") %>%
  mutate(check_home = "Y")

# In previous section we create the clean_madrid_data which is summary table that shows for each customer that was not flagged as an outlier,
# the average hourly consumption for each day of the week (Mon-Sun) in the winter season of 2015. To our full dataset for the customers in scope lets append the columns that indicate the average and standard deviation energy consumption
#for the day of the week in the winter season (jan, feb, march). 
customer_in_scope_DOW <- left_join(customer_in_scope, clean_madrid_data, by = c("CLIENT_ID" = "CLIENT_ID", "DOW" = "DOW"))


customer_in_scope_DOW <- customer_in_scope_DOW %>%
  mutate (check_home = ifelse(AVG_CONS_PER_HR < avg_hourly_season - std*2,"N", "Y"))

customer_in_scope_DOW %>%
  group_by(check_home) %>%
  summarise(client_count= n_distinct(CLIENT_ID), day_count = n_distinct(DAY))

summary_days_out <- customer_in_scope_DOW %>%
  group_by(CLIENT_ID, check_home) %>%
  summarise(day_count = n_distinct(DAY)) %>%
  filter(check_home == "N") %>%
  mutate (avg_days_out = day_count/90) 

ggplot(summary_days_out, aes(x=avg_days_out)) + geom_density(alpha=.3)

customer_in_scope_DOW %>%
  filter(CLIENT_ID == 24903) %>%
  ggplot(., aes(x = DAY, y = AVG_CONS_PER_HR, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)


#Cero consumption for several days. 
customer_in_scope_DOW %>%
  filter(CLIENT_ID == 77909) %>%
  ggplot(., aes(x = DAY, y = AVG_CONS_PER_HR, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)

customer_in_scope_DOW %>%
  filter(CLIENT_ID == 90717) %>%
  ggplot(., aes(x = DAY, y = AVG_CONS_PER_HR, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)


customer_in_scope_DOW %>%
  filter(CLIENT_ID == 96216) %>%
  ggplot(., aes(x = DAY, y = AVG_CONS_PER_HR, colour = factor(check_home), shape = factor(DOW)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)


#Clustering 4: 
#Check energy consumptions for the 3 different products. We find out that there is a clear difference in 
#the total energy consumption between the three. Therefore, lets run the clustering algorithm again
#just for one of the products, in this case lets do it for P30 which contains 674 customers

daily_energy_madrid_clus2 <- daily_energy_madrid %>%
  filter (!CLIENT_ID %in% c(clients_cero$CLIENT_ID))

daily_energy_madrid_clus2 <- daily_energy_madrid_clus3 %>%
  filter (CLIENT_ID %in% c(missing_client_data$CLIENT_ID))

n_distinct(daily_energy_madrid_clus2$CLIENT_ID)

daily_energy_madrid_clus2 %>%
  filter (SEASON == "winter") %>%
  group_by(PRODUCT, DAY) %>%
  summarise(avg_cons = mean(AVG_CONS_PER_HR)) %>%
  ggplot(., aes(x = DAY, y = avg_cons,colour = factor(PRODUCT))) + geom_line(size=1)

clean_madrid_data_p30 <- daily_energy_madrid_clus2 %>%
  filter (SEASON == "winter") %>%
  filter (PRODUCT == "P30") %>%
  group_by(CLIENT_ID, DOW) %>%
  summarise(avg_hourly_season = mean(AVG_CONS_PER_HR), std = sd(AVG_CONS_PER_HR))
n_distinct(clean_madrid_data_p30$CLIENT_ID)



clean_madrid_data_cluster <- clean_madrid_data_p30 %>%
  select(-std) %>%
  spread(DOW, avg_hourly_season) 

data_scaled <- clean_madrid_data_cluster[,2:8]
data_scaled <- scale(data_scaled, center = TRUE)

set.seed(1234)
wssplot(data_scaled, 20)
fit.km_2 <- kmeans(data_scaled, centers = 8)
par(mar = rep(2, 4))
clusplot(data_scaled,fit.km_2$cluster)



#WSS (measure of compactness of clusters -> minimize)
fit.km_2$tot.withinss

#BSS (measure of the distance between clusters -> maximize)
fit.km_2$betweenss

clean_madrid_data_cluster$cluster <- fit.km_2$cluster

clean_madrid_data_results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  group_by(DOW, cluster) %>%
  summarise(dow_avg_consumption = mean(avg_hourly_season), standDev = sd(avg_hourly_season))


clean_madrid_data_results %>% 
  ggplot(., aes(x = DOW, y = dow_avg_consumption, group = cluster, shape=factor(cluster), colour=factor(cluster))) + geom_line(size=1) +  geom_point(size=2) + xlab("Day of the Week (0=sunday)") + ylab("Avg hourly consumption") + # Set axis labels
  ggtitle("Clusters") +  scale_linetype_discrete(name="Sex") + facet_grid( . ~ cluster) + 
  geom_errorbar(aes(x = DOW, ymin = dow_avg_consumption-standDev, ymax = dow_avg_consumption+standDev, color = NULL, linetype = NULL),
                width=.1, position=position_dodge(width = .1))

clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(., aes(x = DOW, y = avg_hourly_season, group = CLIENT_ID, colour=factor(cluster))) + geom_line() +  geom_point(size=2) + facet_grid( . ~ cluster)


clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_hourly_season",2:8) %>%
  ggplot(.,aes(x=DOW , y=avg_hourly_season)) +
  scale_x_discrete() +
  geom_jitter(aes(colour = cluster, x = DOW), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(colour = cluster), outlier.colour = NA, position = "dodge") +
  facet_wrap(~ cluster)



#Clustering 5: Lets cluster customer based on the percentage of energy usage by DOW over the totoal week. We first 
#need to calculate for each customer and each week in the dataset, what percentage of the total energy consumed in a week
#is consumed in each day of the week. By doing this analysis we are trying to group together customers that exihibit a 
#similar behaviour during the week. The objetive is similar as the previous clustering runs, however this might  
# yield better results as the variability of the dataset fed to the clustering algorithm is lower. 
daily_energy_madrid_clus3 <- daily_energy_madrid %>%
  filter (!CLIENT_ID %in% c(clients_cero$CLIENT_ID))

daily_energy_madrid_clus3 <- daily_energy_madrid_clus3 %>%
  filter (CLIENT_ID %in% c(missing_client_data$CLIENT_ID))

n_distinct(daily_energy_madrid_clus3$CLIENT_ID)


by_week <- daily_energy_madrid_clus2 %>%
  filter(SEASON == "winter") %>%
  filter (PRODUCT == "P30") %>%
  group_by(CLIENT_ID, week_in_year) %>%
  summarise(energy_week = sum(TOTAL_DAY_CONS))

#There are some customers that have certain weeks with 0 total energy consumption.These can be considered as outliers,
#therefore, since we want to model the distribution of energy consumption across a week, I believe it is safer to 
#get rid of these customers. They might distorsion the clustering algorithm. 
customer_no_week_consump <-by_week %>% 
  filter(energy_week <= 0)

by_week <- by_week %>%
  filter(!CLIENT_ID %in% c(customer_no_week_consump$CLIENT_ID))

by_dow <- daily_energy_madrid_clus2 %>%
  filter(SEASON == "winter") %>%
  filter (PRODUCT == "P30") %>%
  select(CLIENT_ID, week_in_year, DOW,TOTAL_DAY_CONS)

by_dow <- by_dow %>%
  filter(!CLIENT_ID %in% c(customer_no_week_consump$CLIENT_ID))

by_dow_joined <- left_join(by_dow, by_week, by = c("CLIENT_ID" = "CLIENT_ID", "week_in_year" = "week_in_year"))

#Calculating the percentage energy consumed for each DOW over the total energy during the week
by_dow_joined_perc <- by_dow_joined %>%
  mutate(usage_perc = TOTAL_DAY_CONS/energy_week) 

by_dow_joined_perc <- by_dow_joined_perc %>%
  select(CLIENT_ID, DOW, week_in_year, usage_perc)


cluster_data <- by_dow_joined_perc %>%
  group_by(CLIENT_ID, DOW) %>%
  summarise(avg_usage_perc = mean(usage_perc), std = sd(usage_perc))

#Now for each client, lets check what is the average percentage consumption by day of the week. We might need to remove some 
#outliers, i.e. high standard deviation

ggplot(cluster_data, aes(x=std)) + geom_density(alpha=.3) + xlim(c(0,1)) + facet_grid (.~DOW)

#zooming in to determine a reasanable cut-off
ggplot(cluster_data, aes(x=std)) + geom_density(alpha=.3) + xlim(c(0,0.5)) + facet_grid (.~DOW)


#By looking at the frequency plot above, it seems that most of the customers have less that 0.1 of std deviation. 
#Threfore, to avoid having too many outliers to be feed into our clustering algorithm, lets get rid of all those
#clients that have more than 0.15 of standard deviation per day of the week.

#Lets identify which customer have at least one of their DOWs with std higher than 0.15. 

outliers <- cluster_data %>% 
  filter(std > 0.17)


#We are getting rid of 156 clients that have at least the average of one of the days of the weeks exhibits an std
#of higher than 0.15
n_distinct(outliers$CLIENT_ID)

#Get rid of outlier clients
cluster_data_clean <- cluster_data %>%
  filter (!CLIENT_ID %in% c(outliers$CLIENT_ID))
n_distinct(cluster_data_clean$CLIENT_ID)


ggplot(cluster_data_clean, aes(x=std)) + geom_density(alpha=.3) + xlim(c(0,0.5)) + facet_grid (.~DOW)


#Plotting, we see than on average in Madrid people tend to spend more energy on Mondays and Tuesdays. Both days, 
#people on average consume ~17% of the total energy spent in the week. 

cluster_data_clean%>%
  group_by(DOW) %>%
  summarise(perc_electr_usage = mean(avg_usage_perc), std = sd(avg_usage_perc)) %>%
  ggplot(data=., aes(x = factor(DOW), y = perc_electr_usage)) +
  geom_jitter(data = cluster_data_clean, mapping=aes(x = factor(DOW), y = avg_usage_perc, colour = factor(DOW)), alpha = 0.3) +
  geom_errorbar(aes(ymin=perc_electr_usage-std, ymax=perc_electr_usage+std),
                width = .2,                    # Width of the error bars
                position = position_dodge(.9)) +
  geom_point(shape = 21, size = 3, fill = "white") +
  xlab("Day of the Week") +
  ylab("Avg. percentage energy consumption out of total week") +
  ggtitle("Avg. percentage energy consumption out of total wee with SD error bars")


#Clustering

clean_madrid_data_cluster <- cluster_data_clean %>%
  select(-std) %>%
  spread(DOW, avg_usage_perc) 

data_scaled <- clean_madrid_data_cluster[,2:8]
data_scaled <- scale(data_scaled, center = TRUE)

wssplot(data_scaled, 20)

fit.km_3 <- kmeans(data_scaled, centers = 10)
par(mar = rep(2, 4))
clusplot(data_scaled,fit.km_3$cluster)

#WSS (measure of compactness of clusters -> minimize)
fit.km_3$tot.withinss

#BSS (measure of the distance between clusters -> maximize)
fit.km_3$betweenss


clean_madrid_data_cluster$cluster <- fit.km_3$cluster


results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8) %>%
  group_by(cluster, DOW ) %>%
  summarise(avg_weekday = mean(avg_weekday_perc_season), standDev = sd(avg_weekday_perc_season))


results %>% 
  ggplot(., aes(x = DOW, y = avg_weekday, group = cluster, shape=factor(cluster), colour=factor(cluster))) + geom_line(size=1) +  geom_point(size=2) + xlab("Day of the Week (0=sunday)") + ylab("Avg hourly consumption") + # Set axis labels
  ggtitle("Clusters") +  scale_linetype_discrete(name="x") + facet_grid( . ~ cluster) + 
  geom_errorbar(aes(x = DOW, ymin = avg_weekday-standDev, ymax = avg_weekday+standDev, color = NULL, linetype = NULL), width=.1, position=position_dodge(width = .1)) +
  ggtitle("Aggregated view of customers' weekly behaviour by clusters")


clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8) %>%
  ggplot(., aes(x = DOW, y = avg_weekday_perc_season, group = CLIENT_ID, colour=factor(cluster))) + geom_line() +  geom_point(size=2) + facet_grid( . ~ cluster) +
  ggtitle("Customer's data points for each cluster by DOW")



clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8) %>%
  ggplot(.,aes(x=DOW , y=avg_weekday_perc_season)) +
  scale_x_discrete() +
  geom_jitter(aes(colour = cluster, x = DOW), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(colour = cluster), outlier.colour = NA, position = "dodge") +
  facet_wrap(~ cluster)




tests <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8)


results_v2 <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8) %>%
  group_by(cluster, DOW, CLIENT_ID ) %>%
  summarise(avg_weekday_perc_season = mean(avg_weekday_perc_season), standDev = sd(avg_weekday_perc_season))

cluster_8 <- results_v2 %>% 
  filter(cluster == 8) %>%
  select(CLIENT_ID)


daily_energy_madrid %>%
  filter(CLIENT_ID %in% c(cluster_8$CLIENT_ID)) %>%
  filter(MONTH %in% c(1,2)) %>%
  group_by (DAY, DOW) %>%
  summarise(avg = mean(TOTAL_DAY_CONS)) %>%
  ggplot(., aes(x = DAY, y = avg, label = DOW)) + geom_line(size=0.5) + geom_point(size = 2, aes(colour = factor(DOW))) + geom_text(hjust = 0, nudge_x = 0.3)

  



results <- clean_madrid_data_cluster %>%
  gather(key = "DOW", value = "avg_weekday_perc_season",2:8) %>%
  group_by(cluster, DOW) %>%
  summarise(avg_weekday_perc_season = mean(avg_weekday_perc_season), standDev = sd(avg_weekday_perc_season))


################################# 5. Customers away from home ##########################

#Based on the average consumtpion of these 1109 customer by day of the weeek,lets try to determine which days during these 3 months they 
#were out of their home.

clean_madrid_data_p30 <- daily_energy_madrid_clus2 %>%
  filter (SEASON == "winter") %>%
  filter (PRODUCT == "P30") %>%
  group_by(CLIENT_ID, DOW) %>%
  summarise(avg_DOW_consumption = mean(TOTAL_DAY_CONS), std = sd(TOTAL_DAY_CONS))
n_distinct(clean_madrid_data_p30$CLIENT_ID)



n_distinct(clean_madrid_data_p30$CLIENT_ID)

customer_in_scope <- daily_energy_madrid %>%
  filter(CLIENT_ID %in% c(unique(clean_madrid_data_p30$CLIENT_ID))) %>%
  filter(SEASON == "winter") %>%
  filter (PRODUCT == "P30") %>%
  mutate(check_home = "Y")

# In previous section we create the clean_madrid_data which is summary table that shows for each customer that was not flagged as an outlier,
# the average total energy consumption for each day of the week (Mon-Sun) in the winter season of 2015. To our full dataset for the customers in scope lets append the columns that indicate the average and standard deviation energy consumption
#for the day of the week in the winter season (jan, feb, march). 
customer_in_scope_DOW <- left_join(customer_in_scope, clean_madrid_data_p30, by = c("CLIENT_ID" = "CLIENT_ID", "DOW" = "DOW"))


customer_in_scope_DOW <- customer_in_scope_DOW %>%
  mutate (check_home = ifelse(TOTAL_DAY_CONS < avg_DOW_consumption - std*2,"N", "Y"))

customer_in_scope_DOW %>%
  group_by(check_home) %>%
  summarise(client_count= n_distinct(CLIENT_ID), day_count = n_distinct(DAY))

summary_days_out <- customer_in_scope_DOW %>%
  group_by(CLIENT_ID, check_home) %>%
  summarise(day_count = n_distinct(DAY)) %>%
  filter(check_home == "N") %>%
  mutate (avg_days_out = day_count/90) 

ggplot(summary_days_out, aes(x=avg_days_out)) + geom_density(alpha=.3) + xlab("% of days away-from-home for selected period") + ylab("Frenquency (i.e. number of customers)") + ggtitle("Distribution of % of days away-from-home")

customer_in_scope_DOW %>%
  filter(CLIENT_ID == 24903) %>%
  ggplot(., aes(x = DAY, y = TOTAL_DAY_CONS, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)+   xlab("Days") + ylab("Total Energy consumption") + ggtitle("3 Months view for single customer")

#Cero consumption for several days. 
customer_in_scope_DOW %>%
  filter(CLIENT_ID == 78523) %>%
  ggplot(., aes(x = DAY, y = TOTAL_DAY_CONS, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)

#Cero consumption for several days. 
customer_in_scope_DOW %>%
  filter(CLIENT_ID == 78523) %>%
  ggplot(., aes(x = DAY, y = TOTAL_DAY_CONS, colour = factor(check_home)))+ geom_line(size=1, aes(group = 1)) +  geom_point(size=2)






#The plot below shows the percentage of customers that we considered "away-from-home" for each day of the period that we are analyzing.
#Clearly see that there is an unusual spike around the 27th of March of 2015. 
customer_in_scope_DOW %>%
  group_by(DAY) %>%
  filter(check_home == "N") %>%
  summarise(client_count= n()) %>%
  mutate (perc_out = client_count/3574) %>%
  ggplot(., aes(x = DAY, y = perc_out)) + geom_line(size=1, colour = I("skyblue2") ,aes(group = 1)) +  geom_point(size=2, colour = I("skyblue2")) +
  annotate("rect", xmin=as.Date("03/27/2015", "%m/%d/%Y"), xmax=as.Date("04/01/2015", "%m/%d/%Y"), ymin=0, ymax=Inf, alpha=0.2, fill="red")
  
  geom_rect(aes(xmin = as.Date("03/27/2015", "%m/%d/%Y"), xmax = as.Date("04/01/2015", "%m/%d/%Y"), ymin=0, ymax=0.12),
                color="lavenderblush4",
                alpha=0.2,
                inherit.aes = FALSE)


as.Date("03/27/2015", "%m/%d/%Y")


p2<- customer_in_scope_DOW %>%
  group_by(DAY) %>%
  summarise(total_energy = sum(TOTAL_DAY_CONS)) %>%
  ggplot(data = ., aes(x = DAY, y = total_energy)) + geom_bar(stat = "identity", aes(fill = I("skyblue2")))



