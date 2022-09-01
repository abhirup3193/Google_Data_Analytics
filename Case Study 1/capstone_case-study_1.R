#--------------------------------------------FINAL ANALYSIS------------------------------------------
# Install packages
# if (!require(package)) install.packages('package')

#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms)#time
library(dplyr) #cleaning
library(skimr) #summarize
library(data.table) #exporting data frame




#load original .csv files, a years worth of data from August 2020 to July 2021
Q1_2020_df <- read_csv("cyclistic_data/Divvy_Trips_2020_Q1.csv")
apr_2020_df <- read_csv("cyclistic_data/202004-divvy-tripdata.csv")
may_2020_df <- read_csv("cyclistic_data/202005-divvy-tripdata.csv")
jun_2020_df <- read_csv("cyclistic_data/202006-divvy-tripdata.csv")
jul_2020_df <- read_csv("cyclistic_data/202007-divvy-tripdata.csv")
aug_2020_df <- read_csv("cyclistic_data/202008-divvy-tripdata.csv")
sep_2020_df <- read_csv("cyclistic_data/202009-divvy-tripdata.csv")
oct_2020_df <- read_csv("cyclistic_data/202010-divvy-tripdata.csv")
nov_2020_df <- read_csv("cyclistic_data/202011-divvy-tripdata.csv")
dec_2020_df <- read_csv("cyclistic_data/202012-divvy-tripdata.csv")

jan_2021_df <- read_csv("cyclistic_data/202101-divvy-tripdata.csv")
feb_2021_df <- read_csv("cyclistic_data/202102-divvy-tripdata.csv")
mar_2021_df <- read_csv("cyclistic_data/202103-divvy-tripdata.csv")
apr_2021_df <- read_csv("cyclistic_data/202104-divvy-tripdata.csv")
may_2021_df <- read_csv("cyclistic_data/202105-divvy-tripdata.csv")
jun_2021_df <- read_csv("cyclistic_data/202106-divvy-tripdata.csv")
jul_2021_df <- read_csv("cyclistic_data/202107-divvy-tripdata.csv")
aug_2021_df <- read_csv("cyclistic_data/202108-divvy-tripdata.csv")
sep_2021_df <- read_csv("cyclistic_data/202109-divvy-tripdata.csv")
oct_2021_df <- read_csv("cyclistic_data/202110-divvy-tripdata.csv")
nov_2021_df <- read_csv("cyclistic_data/202111-divvy-tripdata.csv")
dec_2021_df <- read_csv("cyclistic_data/202112-divvy-tripdata.csv")

jan_2022_df <- read_csv("cyclistic_data/202201-divvy-tripdata.csv")
feb_2022_df <- read_csv("cyclistic_data/202202-divvy-tripdata.csv")
mar_2022_df <- read_csv("cyclistic_data/202203-divvy-tripdata.csv")
apr_2022_df <- read_csv("cyclistic_data/202204-divvy-tripdata.csv")
may_2022_df <- read_csv("cyclistic_data/202205-divvy-tripdata.csv")
jun_2022_df <- read_csv("cyclistic_data/202206-divvy-tripdata.csv")
jul_2022_df <- read_csv("cyclistic_data/202207-divvy-tripdata.csv")

#fixing datatypes
# This step was necessary because the below imported CSV data was not in correct date-time format.
# Wrong format could through error in later part.

Q1_2020_df <- Q1_2020_df %>% mutate(started_at = dmy_hm(started_at))
Q1_2020_df <- Q1_2020_df %>% mutate(ended_at = dmy_hm(ended_at))

apr_2020_df <- apr_2020_df %>% mutate(started_at = dmy_hm(started_at))
apr_2020_df <- apr_2020_df %>% mutate(ended_at = dmy_hm(ended_at))

#merge all of the data frames into one year view
all_trips <- rbind(Q1_2020_df, 
                   apr_2020_df,
                   may_2020_df,
                   jun_2020_df,
                   jul_2020_df,
                   aug_2020_df,
                   sep_2020_df,
                   oct_2020_df,
                   nov_2020_df,
                   dec_2020_df,
                   
                   jan_2021_df,
                   feb_2021_df,
                   mar_2021_df,
                   apr_2021_df,
                   may_2021_df,
                   jun_2021_df,
                   jul_2021_df,
                   aug_2021_df,
                   sep_2021_df,
                   oct_2021_df,
                   nov_2021_df,
                   dec_2021_df,
                   
                   jan_2022_df,
                   feb_2022_df,
                   mar_2022_df,
                   apr_2022_df,
                   may_2022_df,
                   jun_2022_df,
                   jul_2022_df)

#remove individual month data frames to clear up space in the environment 
remove(Q1_2020_df,
       apr_2020_df,
       may_2020_df,
       jun_2020_df, 
       jul_2020_df, 
       aug_2020_df, 
       sep_2020_df, 
       oct_2020_df, 
       nov_2020_df,
       dec_2020_df, 
       
       jan_2021_df,
       feb_2021_df,
       mar_2021_df,
       apr_2021_df,
       may_2021_df,
       jun_2021_df,
       jul_2021_df,
       aug_2021_df,
       sep_2021_df,
       oct_2021_df,
       nov_2021_df,
       dec_2021_df,
       
       jan_2022_df,
       feb_2022_df,
       mar_2022_df,
       apr_2022_df,
       may_2022_df,
       jun_2022_df,
       jul_2022_df)

#Summerize Data
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
glimpse(all_trips) #summary
 

#create new data frame to contain new columns
cyclistic_datetime <- all_trips

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_datetime$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units = "mins")

#create columns: day of week, month, day, year, time, hour
cyclistic_datetime$date <- as.Date(cyclistic_datetime$started_at) #default format is yyyy-mm-dd, use start date
cyclistic_datetime$day_of_week <- wday(all_trips$started_at) #calculate the day of the week 
cyclistic_datetime$day_of_week <- format(as.Date(cyclistic_datetime$date), "%A") #create column for day of week
cyclistic_datetime$month <- format(as.Date(cyclistic_datetime$date), "%m")#create column for month
cyclistic_datetime$day <- format(as.Date(cyclistic_datetime$date), "%d") #create column for day
cyclistic_datetime$year <- format(as.Date(cyclistic_datetime$date), "%Y") #create column for year
cyclistic_datetime$time <- format(as.Date(cyclistic_datetime$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_datetime$time <- as_hms((cyclistic_datetime$started_at)) #create new column for time
cyclistic_datetime$hour <- hour(cyclistic_datetime$time) #create new column for hour


#create column for different seasons: Spring, Summer, Fall, Winter
cyclistic_datetime <-cyclistic_datetime %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
                                                    )

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_datetime <-cyclistic_datetime %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
                                                      )


#clean the data
cyclistic_datetime <- cyclistic_datetime %>% rename(bike_type = rideable_type) #Renaming Columns for more understandability 
cyclistic_datetime <- na.omit(cyclistic_datetime) #remove rows with NA values
cyclistic_datetime <- distinct(cyclistic_datetime) #remove duplicate rows 
cyclistic_datetime <- cyclistic_datetime[!(cyclistic_datetime$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_datetime <- cyclistic_datetime %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id))
#Data cleaned Up
message("Cleaned Up ", nrow(all_trips)-nrow(cyclistic_datetime), " Rows")

#view the final data
head(cyclistic_datetime)
glimpse(cyclistic_datetime)
 
#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(cyclistic_datetime)

#-----------------MEMBER TYPE---------------------
cyclistic_datetime %>%
  group_by(member_casual) %>% 
  count(member_casual)

ggplot(cyclistic_datetime, aes(member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casuals x Members", title="Chart 01 - Casuals x Members distribution")

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_datetime %>%
  group_by(member_casual, bike_type) %>% 
  count(bike_type)

#Plot
cyclistic_datetime %>% 
  group_by(member_casual, bike_type) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = bike_type, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat='identity')

#total rides 
cyclistic_datetime %>%
  group_by(bike_type) %>% 
  count(bike_type)

#-------------------HOUR--------------------------

#total rides by member type 
cyclistic_datetime %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#----------------------TIME OF DAY-----------------------

#-----morning-------
#total rides by member type 
cyclistic_datetime %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
cyclistic_datetime %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
cyclistic_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
cyclistic_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
cyclistic_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#total rides by member type 
cyclistic_datetime %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#number of rides
cyclistic_datetime %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
cyclistic_datetime %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
cyclistic_date %>%
  count(day_of_week)

cyclistic_datetime %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(day_of_week)%>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#----------------DAY OF THE MONTH-----------------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble

#---------------------MONTH-----------------------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(month) 

#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
cyclistic_date %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
cyclistic_date %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides
cyclistic_date %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
cyclistic_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
cyclistic_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
cyclistic_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
cyclistic_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
cyclistic_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))