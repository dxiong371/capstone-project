
#----------- Capstone Project--------------#



# Load library for use and setting up the working directory

library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(hms) #time
library(ggplot2) #helps visualize data



# Load .csv files - May 2022 to April 2023

may22 <- read_csv('202205-divvy-tripdata.csv')
jun22 <- read_csv('202206-divvy-tripdata.csv')
jul22 <- read_csv('202207-divvy-tripdata.csv')
aug22 <- read_csv('202208-divvy-tripdata.csv') 
sep22 <- read_csv('202209-divvy-tripdata.csv')
oct22 <- read_csv('202210-divvy-tripdata.csv')
nov22 <- read_csv('202211-divvy-tripdata.csv')
dec22 <- read_csv('202212-divvy-tripdata.csv')
jan23 <- read_csv('202301-divvy-tripdata.csv')
feb23 <- read_csv('202302-divvy-tripdata.csv')
mar23 <- read_csv('202303-divvy-tripdata.csv')
apr23 <- read_csv('202304-divvy-tripdata.csv')



# Merging all 12 datasets into one

cyclistic_data <- rbind(may22, jun22, jul22, aug22, sep22, oct22, nov22, dec22, jan23, feb23, mar23, apr23)



# Making a new dataset to configure

cyclistic_data_v2 <- cyclistic_data



# Removing unnecessary columns

cyclistic_data_v2 <- cyclistic_data_v2 %>%
  select(-c(ride_id,start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id,))



# Adding columns that list the date, month, day, hour, day of the week and year of each ride

cyclistic_data_v2$date <- as.Date(cyclistic_data_v2$started_at) #The default format is yyyy-mm-dd
cyclistic_data_v2$month <- format(as.Date(cyclistic_data_v2$date), "%m")
cyclistic_data_v2$day <- format(as.Date(cyclistic_data_v2$date), "%d")
cyclistic_data_v2$year <- format(as.Date(cyclistic_data_v2$date), "%Y")
cyclistic_data_v2$day_of_week <- format(as.Date(cyclistic_data_v2$date), "%A")
cyclistic_data_v2$time <- format(as.Date(cyclistic_data_v2$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_data_v2$time <- as_hms((cyclistic_data_v2$started_at))
cyclistic_data_v2$hour <- hour(cyclistic_data_v2$time)



# creating a new column titled ride_length by subtracting the ended_at column and the started_at column 

cyclistic_data_v2$ride_length <- difftime(cyclistic_data_v2$ended_at, cyclistic_data_v2$started_at, units="mins")
cyclistic_data_v2$ride_length <-as.numeric(as.character(cyclistic_data_v2$ride_length))



# Cleaning the data

cyclistic_data_v2 <- distinct(cyclistic_data_v2)
cyclistic_data_v2 <- na.omit(cyclistic_data_v2)
cyclistic_data_v2 <- cyclistic_data_v2[!(cyclistic_data_v2$ride_length <=0),]



#create a column for the month using the full month name
cyclistic_data_v2 <-cyclistic_data_v2 %>% mutate(month = 
                                                   case_when(month == "01" ~ "January",
                                                             month == "02" ~ "February",
                                                             month == "03" ~ "March",
                                                             month == "04" ~ "April",
                                                             month == "05" ~ "May",
                                                             month == "06" ~ "June",
                                                             month == "07" ~ "July",
                                                             month == "08" ~ "August",
                                                             month == "09" ~ "September",
                                                             month == "10" ~ "October",
                                                             month == "11" ~ "November",
                                                             month == "12" ~ "December"
                                                   )
)



# Correcting the order for the months and the weekdays when analyzing
cyclistic_data_v2$month <- ordered(cyclistic_data_v2$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
cyclistic_data_v2$day_of_week <- ordered(cyclistic_data_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# View the new data set

View(cyclistic_data_v2)



# Total Rides #

nrow(cyclistic_data_v2)



# Total numbers of member types #

cyclistic_data_v2 %>%
  group_by(member_casual) %>%
  count(member_casual)



#total rides grouped by hour for each member type 

cyclistic_data_v2 %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48)



#total rides for each hour

cyclistic_data_v2 %>%
  count(hour) %>% 
  print(n = 24)



#total rides grouped by the day of the week for each member type

cyclistic_data_v2 %>%
  group_by(member_casual) %>% 
  count(day_of_week)



#total rides for each day of the week

cyclistic_data_v2 %>%
  count(day_of_week)



#total rides grouped by each month for each member type 

cyclistic_data_v2 %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24)



#total rides for each month

cyclistic_data_v2 %>%
  count(month)



# average of ride_length

mean(cyclistic_data_v2$ride_length)



#average ride_length for each member type

cyclistic_data_v2 %>% 
  group_by(member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
