#--------------------------------------------CREATING CSV FILE FOR TABLEAU------------------------------------------
#Capstone Project:
#Scenario:  You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company's future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations. Use Cyclistic's historical trip data to analyze and identify trends. 
#(Note: The data sets have a different names because Cyclistic is a fictional company. For the purposes of this case study, the datasets are appropriate and will enable you to answer the business questions. The data has been made available by Motivate International Inc. under this license.)

#load libraries

library(tidyverse) #calculations
library(lubridate) #dates 
library(janitor) # to clean data

#load original .csv files, a years worth of data from September 2020 to August 2021
q9_2020 <- read_csv("202009-divvy-tripdata.csv")
q10_2020 <- read_csv("202010-divvy-tripdata.csv")
q11_2020 <- read_csv("202011-divvy-tripdata.csv")
q12_2020 <- read_csv("202012-divvy-tripdata.csv")
q1_2021 <- read_csv("202101-divvy-tripdata.csv")
q2_2021 <- read_csv("202102-divvy-tripdata.csv")
q3_2021 <- read_csv("202103-divvy-tripdata.csv")
q4_2021 <- read_csv("202104-divvy-tripdata.csv")
q5_2021 <- read_csv("202105-divvy-tripdata.csv")
q6_2021 <- read_csv("202106-divvy-tripdata.csv")
q7_2021 <- read_csv("202107-divvy-tripdata.csv")
q8_2021 <- read_csv("202108-divvy-tripdata.csv") 

#unable to merge all of the data frames. Look for different data types
sapply(q9_2020,class)
sapply(q10_2020,class)
sapply(q11_2020,class)
sapply(q12_2020,class)
sapply(q1_2021,class)
sapply(q2_2021,class)
sapply(q3_2021,class)
sapply(q4_2021,class)
sapply(q5_2021,class)
sapply(q6_2021,class)
sapply(q7_2021,class)
sapply(q8_2021,class)

#Mutate data type to make all columns consistent
q9_2020 <- mutate(q9_2020, start_station_id = as.character(start_station_id))
q10_2020 <- mutate(q10_2020, start_station_id = as.character(start_station_id))
q11_2020 <- mutate(q11_2020, start_station_id = as.character(start_station_id))
q9_2020 <- mutate(q9_2020, end_station_id = as.character(end_station_id))
q10_2020 <- mutate(q10_2020, end_station_id = as.character(end_station_id))
q11_2020 <- mutate(q11_2020, end_station_id = as.character(end_station_id))

#merge all of the data frames into one data frame
bike_rides <- bind_rows(q9_2020, q10_2020, q11_2020, q12_2020, q1_2021, q2_2021, q3_2021, q4_2021, q5_2021, q6_2021, q7_2021, q8_2021)

#remove individual month data frames to clear up space in the environment 
remove(q9_2020, q10_2020, q11_2020, q12_2020, q1_2021, q2_2021, q3_2021, q4_2021, q5_2021, q6_2021, q7_2021, q8_2021)

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
bike_rides$minutes <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("min"))
bike_rides$minutes <-  as.numeric(as.character(bike_rides$minutes))
bike_rides$minutes <- round(bike_rides$minutes, digits = 1)

#create columns for:  month, day, year, day of week, and hour
bike_rides$date <- as.Date(bike_rides$started_at) #create column for date, default format is yyyy-mm-dd, to extract month, day, and year
bike_rides$month <- format(as.Date(bike_rides$date), "%m")#create column for month
bike_rides$day <- format(as.Date(bike_rides$date), "%d") #create column for day
bike_rides$year <- format(as.Date(bike_rides$date), "%Y") #create column for year
bike_rides$day_of_week <- format(as.Date(bike_rides$date), "%A") #create column for day of week
bike_rides$hour <- lubridate::hour(bike_rides$started_at) #create new column for hour

#create column for different seasons: Spring, Summer, Fall, Winter
bike_rides <-bike_rides %>% mutate(season = 
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
                                                       month == "02" ~ "Winter"))

#create column for different time_of_day: Night, Morning, Afternoon, Evening
bike_rides <-bike_rides %>% mutate(time_of_day = 
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
                                                       hour == "23" ~ "Evening"))


#create a column for the month using the full month name
bike_rides <-bike_rides %>% mutate(month = case_when(month == "01" ~ "January",
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
                                                       month == "12" ~ "December"))
                                       
#clean the data and create new data frame
df <- janitor::remove_empty(bike_rides, which = c("cols"))
df <- janitor::remove_empty(bike_rides, which = c("rows"))
df <- distinct(bike_rides) #remove duplicate rows 
df<- na.omit(bike_rides) #remove rows with NA values


#Filter the data
df <- df %>%
  filter(minutes>0) %>% #remove where ride_length is 0 or negative
  drop_na %>% 
  select(-c(ride_id,started_at,ended_at,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng))

#view the final data
dim(df)

#download the new data as a .csv file
write.csv(df,"cyclistic_data_for_Tableau.csv")

