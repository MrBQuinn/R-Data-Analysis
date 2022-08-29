# Case Study Overview:
# 1. Company Wants to maximize the number of annual memberships
# 2. How do causal riders and annual members use the product differently?
# 3. Why would casual riders buy Cyclistic annual memberships?
# 4. How can Cyclistic use digital media 
#   to influence casual riders to become members?

# Point 2 is main concern: casual vs annual member product use

# Install Packages, Ensure proper directory
library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("~/Data Analytics/cyclistic_dataset")

# Collect Data into Data Frames
q2_2018 <- read_csv("Divvy_Trips_2018_Q2.csv")
q3_2018 <- read_csv("Divvy_Trips_2018_Q3.csv")
q4_2018 <- read_csv("Divvy_Trips_2018_Q4.csv")
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
# NOTE: Q3 of 2019 & Q1 of 2018 are missing and only Q1 of 2020 is available

# Wrangle Data | Combine into Single File
# First check col names are all matching, same order not required
# One year at a time is easier
# 2018 (minus q1)
colnames(q2_2018)
colnames(q3_2018)
colnames(q4_2018)
# 2019 (minus q2 and q3)
colnames(q1_2019)
colnames(q4_2019)
# 2020 (q1 only)
colnames(q1_2020)
#q1_2018 & q2_2019 is removed due to formatting issue

# 2020 col name scheme is new standard
# Rename 2018 and 2019 columns to match 2020
# Grouped by year for testing/confirmation
(q2_2018 <- rename(q2_2018,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

(q3_2018 <- rename(q3_2018,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

(q4_2018 <- rename(q4_2018,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

# 2019
(q1_2019 <- rename(q1_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

(q4_2019 <- rename(q4_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

# Inspect Data Frame Structures
str(q2_2018)
str(q3_2018)
str(q4_2018)
str(q1_2019)
str(q4_2019)
str(q1_2020)

# Use mutate to change ride_id and rideable type to character col type
q2_2018 <- mutate(q2_2018, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2018 <- mutate(q3_2018, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q4_2018 <- mutate(q4_2018, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
# Run str() in console to test that data type is correct and ready for stacking

# Stack all data frames into one single master data frame
all_trips <- bind_rows(q2_2018, q3_2018, q4_2018, 
                       q1_2019, q4_2019,
                       q1_2020)
                       
# Remove lat, long, birthyear, and gender so all matches 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear))
# Ready for cleaning

# CLEAN DATA
# Evaluate new data frame
colnames(all_trips) #Col Names
nrow(all_trips) #Row Count: 4711947
dim(all_trips) #DF Dimensions: 10C by 4711947
head(all_trips) #First 6 Rows
str(all_trips) #Col Names and Data Types
summary(all_trips) #Stat Summary of numerical data

#Issues that need checking:
# 1. Check that member_casual column only has 2 types, casual and subscriber
# 2. Add day, month, and year columns for further analysis in the future
# 3. Create a ride_length column that calculates the length of ride for each row
# 4. Delete rides that have negative trip duration

# Pull Table of just count of member_casual column values
table(all_trips$member_casual)

# Reassign to 2020 labels
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

# Check member_casual is correct now
table(all_trips$member_casual)

# Populate day, month, and year columns from the date of activity
# Assigning new columns with as.Date() function with % argument
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_week <- format(as.Date(all_trips$date), "%A")

# Inspect Structure
str(all_trips)
