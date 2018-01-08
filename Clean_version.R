############# 2 packages required


library("data.table")
library("dplyr")
library("maps")
library("leaflet")  # allows map visualization
library("ggplot2")
library("ggthemes") # Load themes for ggplot
library("DT")  # allow customized tables

############## 3 DATA PREPARATION

#### 3.2  Import datasets

flights <- fread("flights.csv", showProgress = FALSE)
airports <- fread("airports.csv", showProgress = FALSE)
airlines <- fread("airlines.csv", showProgress = FALSE)

colnames(flights)
colnames(airports)
colnames(airlines)



## using full_joins of dplyr as well as renaming the variables in order to have consistent names for my final dataset
colnames(flights)[5] <- "AIRLINE_CODE"
colnames(airlines)[1] <- "AIRLINE_CODE"
airline_flights <- full_join(flights,airlines,by = c("AIRLINE_CODE"))

colnames(airports) <- c("DEST_CODE","DEST_AIRPORT", "DEST_CITY", "DEST_STATE", "DEST_COUNTRY", "DEST_LATITUDE", "DEST_LONGITUDE" )
colnames(airline_flights)[9] <- "DEST_CODE"
airline100 <- full_join(airline_flights,airports,by = "DEST_CODE")

colnames(airports) <- c("ORIGIN_CODE","ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE", "ORIGIN_COUNTRY", "ORIGIN_LATITUDE", "ORIGIN_LONGITUDE" )
colnames(airline100)[8] <- "ORIGIN_CODE"
airline200 <- full_join(airline100,airports,by = "ORIGIN_CODE")


# from here we will use dplyr and pipe operator to create two new varibales, re order, eliminate and arrange

colnames(airline200)

aircraft <- airline200 %>% 
  mutate(LOSS = ARRIVAL_DELAY - DEPARTURE_DELAY, AVG_SPEED = DISTANCE / (AIR_TIME/60) ) %>%  # mutate from dplyr to creeate variables, avg_speed in miles/hour
  select(1:5,32,6:8,39:44,9,33:38,10:18,46,19:23,45,24:31) %>% #select the variabels that we interested in
  select(1:46,-1,-7,-8,-13,-16,-17,-20,-27,-33) %>%  #eliminate te variables that we are not going to use...decided to eliminate year because all of the flights are in 2015
  arrange(DAY,MONTH, SCHEDULED_DEPARTURE, AIRLINE) #arrange in a date basis

colnames(aircraft)
View(aircraft)
dim(aircraft)
# from here, show the full image of our data...stating that the missing values are a consequence of delayed flights..and will be treated later
# so missing values here are totally fine and nothing to worru about

####### 3.3 FINAL DATASET

head(aircraft)
datatable(head(aircraft, n = 100), class = 'cell-border stripe') ## this is the full image of our data

#### 3.4 SUMMARY STATISTICS

aircraft_summary <- aircraft %>%
  select(DEPARTURE_DELAY, ARRIVAL_DELAY, LOSS, DISTANCE, AIR_TIME, AVG_SPEED, ELAPSED_TIME, TAXI_IN, TAXI_OUT)
  


  my.summary <- function(x, na.rm=TRUE){
    result <- c(Mean = format(round(mean(x, na.rm = na.rm),digits = 2)),
                SD = format(round(sd(x, na.rm = na.rm), digits = 2)),
                Median = format(round(median(x, na.rm = na.rm), digits = 2)),
                Min = format(round(min(x, na.rm = na.rm), digits = 2)),
                Max = format(round(max(x, na.rm = na.rm), digits = 2)), 
                N_Obs = length(x),
                N_NA = sum(is.na(x)))
  }
  summary_aircraft <- sapply(aircraft_summary, my.summary)

  datatable(summary_aircraft, class = 'cell-border stripe', options = list(dom = 't'))  # using library DT

  # comentate a little bit on the missing values

  # a further study on the missing values will be done later...once we et deeper into the delays observations, which is where the missing values are
  
######## 3.5 missing values
  
  #info about missing values
  ##column information
  
  missing_values <- data.frame(  class = sapply(aircraft, class),
                                 missing.count = colSums(is.na(aircraft)),
                                 missing.pct = paste(round(colSums(is.na(aircraft)) / nrow(aircraft) * 100, digits = 1), "%", sep = ""))
  
  datatable(missing_values, class = 'cell-border stripe', options = list(
    order = list(3, 'desc')))  # it is already in descening order based on missing values percentage...

  ## explanation avout the missing values same as for the pre-evaluation...but no need to worry
  
################## 4EDA
  
  ######4.1 INITIAL EDA

# map with airports in the U.S ( click on the icon to see what is the name of the airport)
library("leaflet") # please zoom in and select an icon to see the airport name
df = data.frame(Lat = unique(aircraft$ORIGIN_LATITUDE), Long =  unique(aircraft$ORIGIN_LONGITUDE))
leaflet(df) %>% 
  addTiles() %>% 
  addAwesomeMarkers(label = unique(aircraft$ORIGIN_AIRPORT))


###


# barplot for teh number of flights in the airlines
avg_nflights_airline <- length(aircraft$AIRLINE) / length(unique(aircraft$AIRLINE))

ggplot(aircraft, aes(x = AIRLINE)) +
  geom_bar() +
  geom_hline(yintercept = avg_nflights_airline, col = "blue") + 
  coord_flip() + 
  theme_tufte() +
  labs( x = "Airline", y = "Number of flights ")+
  ggtitle("Number of flights per Airline", subtitle = "Blue vertical line: Average number of flights of the airlines")

# barplot for the number of flights by day of the week
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK == 1] = 'Monday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK== 2] = 'Tuesday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK == 3] = 'Wednesday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK == 4] = 'Thursday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK== 5] = 'Friday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK == 6] = 'Saturday'
aircraft$DAY_OF_WEEK[aircraft$DAY_OF_WEEK == 7] = 'Sunday'

avg_nflights_day <- length(aircraft$DAY_OF_WEEK) / length(unique(aircraft$DAY_OF_WEEK))

ggplot(aircraft, aes(x = factor(DAY_OF_WEEK), fill = AIRLINE)) +
  geom_bar() +
  geom_hline(yintercept = avg_nflights_day, col = "blue") + 
  theme_tufte() +
  coord_flip() +
  labs( x = "Day of the week", y = "Number of flights ") +
  ggtitle("Number of flights per Day of the week", subtitle = "Blue line represents Average of flights in any given day")

# dot plot of speed vs distance

aircraft_small <- aircraft[sample(nrow(aircraft), 1000), ] # sample of 1000 obsevations (takes forever otherwise)
ggplot(aircraft_small, aes(x = AVG_SPEED, y = DISTANCE)) +
  geom_point() +
  geom_smooth() +
  theme_tufte() +
  scale_y_continuous(limits = c(0, 3000)) +
  labs( x = "Average Speed", y = "Distance ") +
  ggtitle("Average Speed versus Distance")

#### taxi in per airline
## TIME ELAPSED BETWEEN WHEELS OF AND ARRIVAL AT THE DESTINATION AIRPORT

ggplot(aircraft, aes(y = TAXI_IN, x = AIRLINE, fill = AIRLINE)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0, 20)) +
  geom_hline(yintercept =  mean(aircraft$TAXI_IN, na.rm = T), col = "black") + 
  theme_tufte() +
  coord_flip() + # this coord flips makes the boxplots horizontal
  guides(fill = FALSE) + #removes legend
  labs( x = "Airline", y = "Taxi in (minutes) ") +
  ggtitle("Taxi in per Airline", subtitle = "Black line represents Average taxi in time for all airlines combined") 
## interesting to see that the mean is well far to the right from the median..means there are a lot of outliers!
# also very interesting to see that even if southwest has so many flight, they are able to have on of the shortest taxi in...their 75% percentage is lower thatn the mean!
############# 4.2 deeper insights in delays...whihc is what we really want to study



#### delays....

# As we saw in the mssing values table, 81.7% missing values in the reasons for the delays...but the reason for this is that only 18% of the flights are delay
# as a consequence we are going to construct a delay flight dataframe contiaing only the delayed flights
# this is going to help us determine how much delay shoul we need to consider a flight to be delayed


#I want to build a dataset with only delayed flights...as a consequence, I want to get what is the minimum time to consider a flight as delayed
index_delay <- which(is.na(aircraft$AIR_SYSTEM_DELAY))
aircraft_delay <- aircraft[-index_delay,]
min_delay <- min(aircraft_delay$ARRIVAL_DELAY)  # GREAT! a flight is considered delayed if the delay is > 15
min_delay

aircraft_delay <- aircraft %>%
  select(18, 28:29, 33:37,1:17, 19:27,30:32) %>%  # to have the delay variables at the beginning
  filter(ARRIVAL_DELAY >= min_delay) %>%
  arrange(desc(ARRIVAL_DELAY), desc(DEPARTURE_DELAY))

datatable(head(aircraft_delay, n = 100), class = 'cell-border stripe')

### summary of the reasons
delay_reasons <- cbind(
  apply(aircraft_delay[,4:8],2,min),
  apply(aircraft_delay[,4:8],2,max),
  apply(aircraft_delay[,4:8],2,median),
  apply(aircraft_delay[,4:8],2,mean),
  apply(aircraft_delay[,4:8],2,sd)
)  
colnames(delay_reasons) <- c("Min", "max", "median", "mean", "sd")

datatable(delay_reasons, class = 'cell-border stripe', options = list(dom = 't')) 


##### lets now study the percentage of cancelled flights, diverted and delayed

percentage_delayed <- paste(round(length(aircraft_delay$DAY)*100/length(aircraft$DAY),2), "%", sep = "") 
percentage_diverted <- paste(round(mean(aircraft$DIVERTED)*100, 2), "%", sep = "") 
percentage_cancelled <- paste(round(mean(aircraft$CANCELLED)*100, 2), "%", sep = "") 
percentage_unnormal <- paste(round((length(aircraft_delay$DAY)*100/length(aircraft$DAY) + mean(aircraft$DIVERTED)*100 + mean(aircraft$CANCELLED)*100),2), "%", sep = "") 

percentage <- matrix(c(percentage_delayed,percentage_diverted, percentage_cancelled, percentage_unnormal ), ncol = 4)
colnames(percentage) <- c('percentage_delayed', 'percentage_diverted','percentage_cancelled','percentage_unnormal')

datatable(percentage, class = 'cell-border stripe', options = list(dom = 't')) 
### Unormal flights (delayed, cancelled or diverted) so 1 out of 5 flights are rather cancelled, diverted or delayed

# cancellation reaonss: 

index <- which(aircraft$CANCELLED == 1)
cancel <- aircraft[index,]
n_cancel <- length(cancel$CANCELLATION_REASON)
canc_reason <- table(cancel$CANCELLATION_REASON) / n_cancel * 100 # perc table of the cancelation
paste(round(canc_reason,2), "%", sep = "")

# run from here all the way until the plot
cancel$MONTH[cancel$MONTH == 1] = 'January'
cancel$MONTH[cancel$MONTH == 2] = 'February'
cancel$MONTH[cancel$MONTH == 3] = 'March'
cancel$MONTH[cancel$MONTH == 4] = 'April'
cancel$MONTH[cancel$MONTH == 5] = 'May'
cancel$MONTH[cancel$MONTH == 6] = 'June'
cancel$MONTH[cancel$MONTH == 7] = 'July'
cancel$MONTH[cancel$MONTH == 8] = 'August'
cancel$MONTH[cancel$MONTH == 9] = 'September'
cancel$MONTH[cancel$MONTH == 10] = 'October'
cancel$MONTH[cancel$MONTH == 11] = 'November'
cancel$MONTH[cancel$MONTH == 12] = 'December'

cancel$CANCELLATION_REASON[cancel$CANCELLATION_REASON == 'A'] = 'Airline Carrier'
cancel$CANCELLATION_REASON[cancel$CANCELLATION_REASON == 'B'] = 'Weather'
cancel$CANCELLATION_REASON[cancel$CANCELLATION_REASON == 'C'] = 'National Air System'
cancel$CANCELLATION_REASON[cancel$CANCELLATION_REASON == 'D'] = 'Security'

ggplot(cancel,aes(x = MONTH, fill = CANCELLATION_REASON)) + 
  geom_bar() +
  theme_tufte() +
  labs( x = "Month", y = "Number of flights cancelled ") +
  coord_flip() + # this coord flips makes the boxplots horizontal
  ggtitle("Number of flights cancelled per month ")


#### let's dig deeper into delays
################### 
### histogram for departure delay
ggplot(aircraft, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(bins = 200) +
  geom_vline(xintercept = 0, col = "red") +
  scale_x_continuous(limits = c(-100, 100)) +
  labs( x = "Departure delay (minutes)", y = "Count of flights") +
  theme_tufte() +
  ggtitle("Histogram of delays at departure", subtitle = "Red vertical line represents departure delay equal to 0")


### histogramfor arrival delay
ggplot(aircraft, aes(x = ARRIVAL_DELAY)) +
  geom_histogram(bins = 200) +
  geom_vline(xintercept = 0, col = "red") +
  scale_x_continuous(limits = c(-100, 100)) +
  labs( x = "Arrival delay (minutes)", y = "Count of flights") +
  theme_tufte() +
  ggtitle("Histogram of delays at arrival", subtitle = "Red vertical line represents arrival delay equal to 0")


### histogram for loss
ggplot(aircraft, aes(x = LOSS)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0, col = "red") +
  scale_x_continuous(limits = c(-50, 50)) +
  labs( x = " loss (minutes)", y = "Count of flights") +
  theme_tufte() +
  ggtitle("Histogram of time lost during flight", subtitle = "Red vertical line represents loss equal to 0")


##### related to loss..
ggplot(aircraft_small, aes(DEPARTURE_DELAY, ARRIVAL_DELAY)) + #aircraft_small is the smple of 1000 flights that we used earlier
  geom_point(aes(color = factor(AIRLINE), alpha = 1)) + 
   geom_abline(intercept = 0, slope = 1) +
  theme_tufte() +
  xlab('Departure delay in minutes') + ylab('Arrival delay in minutes') +
  ggtitle('Relationship between Departure delay and Arrival delay with Airline shown')
# things that we see: almost all the values are concentrated around 0 and as we saw with loss, there are more values below the line...which means that arrival delay < departure delay
# we can also observe that all airlines have delays...all colors are present
#### boxplot of delays per airline

### takes some time to run!!!
## make sure to format axis and everything...becasue it is an easy way to lose points
# format axis and labels!!!!
## decided to exclude outliers...because of the great amount the are...
ggplot(aircraft, aes(y = ARRIVAL_DELAY, x = AIRLINE, fill = AIRLINE)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(-15, 15)) +
  geom_hline(yintercept = 0, col = "black") +
  theme_tufte() +
  labs( x = " Airline ", y = "Arrival delay (minutes)") +
  coord_flip() + # this coord flips makes the boxplots horizontal
  guides(fill = FALSE) +# removes legend
  ggtitle("Arrival Delay per Airline", subtitle = "black vertical line represents a delay = 0")


## 4.3 at a regional level and interesting facts



### podemos poner lo de los vuelos mas largos y mas cortos..pero modificar coding..da error
# best / worst delays
colnames(aircraft)

ind_min <- which(aircraft$ARRIVAL_DELAY == min(aircraft$ARRIVAL_DELAY, na.rm = TRUE) )
min_delay <- aircraft[ind_min,c( 5,8, 12,18,  28,33:37)]  

datatable(min_delay, class = 'cell-border stripe', options = list(dom = 't')) 

ind_max <- which(aircraft$ARRIVAL_DELAY == max(aircraft$ARRIVAL_DELAY, na.rm = TRUE) )
max_delay <- aircraft[ind_max,c( 5,8, 12,18,  28, 33:37)]  

datatable(max_delay, class = 'cell-border stripe', options = list(dom = 't')) 
#### if we have some time, lower the ground to cincinnati

delays_cincinnati <- aircraft %>%
  filter(ORIGIN_CODE == 'CVG') %>%
  group_by(AIRLINE) %>%
  summarise( mean_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  arrange( desc(mean_delay))

datatable(delays_cincinnati, class = 'cell-border stripe', options = list(dom = 't'))

UA_CVG <- aircraft %>%
  filter(ORIGIN_CODE == 'CVG' & AIRLINE_CODE == 'UA') %>%
  select(ARRIVAL_DELAY,  MONTH, DAY, ORIGIN_CODE) %>%
  arrange(desc(ARRIVAL_DELAY ))

datatable(UA_CVG, class = 'cell-border stripe', options = list(dom = 't'))

delays_delta_cincinnati <- aircraft %>%
  filter(ORIGIN_CODE == 'CVG' & AIRLINE_CODE == 'DL') %>%
  group_by(AIRLINE) 
 
ggplot(delays_delta_cincinnati , aes(x = ARRIVAL_DELAY)) +
  geom_histogram(bins = 200) +
  geom_vline(xintercept = 0, col = "red") +
  scale_x_continuous(limits = c(-100, 100)) +
  labs( x = " Delay at Arrival (minutes)", y = "Count of flights") +
  theme_tufte() +
  ggtitle("Histogram of delays at arrival for delta in CVG")





