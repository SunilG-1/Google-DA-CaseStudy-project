# loading the required dependencies

library(tidyverse)
library(lubridate)
library(ggplot2)
library(skimr)

library(janitor)

# load data 

jan_21 <- read_csv("202101-divvy-tripdata.csv")
feb_21 <- read_csv("202102-divvy-tripdata.csv")
mar_21 <- read_csv("202103-divvy-tripdata.csv")
apr_21 <- read_csv("202104-divvy-tripdata.csv")
may_21 <- read_csv("202105-divvy-tripdata.csv")
jun_21 <- read_csv("202106-divvy-tripdata.csv")
jul_21 <- read_csv("202107-divvy-tripdata.csv")
aug_21 <- read_csv("202108-divvy-tripdata.csv")
sep_21 <- read_csv("202109-divvy-tripdata.csv")
oct_21 <- read_csv("202110-divvy-tripdata.csv")
nov_21 <- read_csv("202111-divvy-tripdata.csv")
dec_21 <- read_csv("202112-divvy-tripdata.csv")

# Validating data

# comparing columns
janitor::compare_df_cols(jan_21, feb_21, mar_21, apr_21, may_21, jun_21, jul_21, aug_21, sep_21, oct_21, nov_21, dec_21)


# they all have same number of columns and similar names so lets bind them in yearly view
trips2021 <- rbind(jan_21, feb_21, mar_21, apr_21, may_21, jun_21, jul_21, aug_21, sep_21, oct_21, nov_21, dec_21)

# we can use the `compare_df_cols_same()` which will return TRUE if all the columns from different table matches, if not it'll return FALSE
compare_df_cols_same(jan_21, feb_21, mar_21, apr_21, may_21, jun_21, jul_21, aug_21, sep_21, oct_21, nov_21, dec_21)

str(trips2021)

" Creating new columns for detailed analysis.
  * ride_length
  * week_day
  * month & year

"
# create a column with ride length
trips2021$ride_length_m <- as.integer(difftime(trips2021$ended_at, trips2021$started_at, units = "min"))

# create new column week_day
trips2021$week_day <- weekdays(trips2021$started_at)

# month
trips2021$month <- format(as.Date(trips2021$started_at),"%B")

# year
trips2021$year <- format(as.Date(trips2021$started_at),"%Y")



"Cleaning the data:
  
  * changing the data type of columns: started_at, ended_at
  * Removing columns that aren't needed: start_station_name ~ end_lng
  * dropping rows having NA and Duplicate values" 

# shows NA values by column name
colSums(is.na(trips2021))

# drop columns
trips2021 <- trips2021 %>%
  select(-c("start_station_name":"end_lng"))

# remove all the na values
trips2021_clean <- na.omit(trips2021)

# check if data.frame has empty values true then returns TRUE else FALSE.
is_empty(trips2021_clean)

# Removing duplicates
trips2021_clean <- trips2021_clean[!duplicated(trips2021_clean$ride_id),]


# so there is some data that is in negative `ride_length_m`, need to clean that data too
head(filter(trips2021_clean, ride_length_m < 0))

trips2021_clean <- filter(trips2021_clean, ride_length_m >= 0)

# check if columns has NA values
colSums(is.na(trips2021_clean))

str(trips2021_clean)

view(head(trips2021_clean))

# check summary of the data

summary(trips2021_clean)
summary(trips2021_clean$ride_length_m)

# We can see from summary of the 'ride_length_m' its has min of 0 and max ride length of 55944 min
# filtering the data where ride_length_m is > 0 and <= 1440 min

trips2021_clean <- trips2021_clean %>% 
  filter(ride_length_m >= 1 & ride_length_m <= 1440) 

# now data is clean and ready for aggregation and further analysis lets export
write.csv(trips2021_clean, "trips2021_clean_data.csv")

# let's analyze data further

# 1. CASUAL vs ANNUAL MEMBERS

# Total number of Rides.
nrow(trips2021_clean) # returns total number of rides

trips2021_clean %>% 
  group_by(member_casual) %>% 
  summarise(
    count = n(),
    "in(%)" = (length(member_casual) / nrow(trips2021_clean)) * 100)

# 2. RIDE TIME

# let's check summary of ride time
summary(trips2021_clean$ride_length_m)

# aggregating ride_length_m on members_casual to check average ride time of Casual vs Members 
aggregate(trips2021_clean$ride_length_m, list(trips2021_clean$member_casual), mean)


# 3. Average ride time on 'weekdays'

#Let's order the week_day
trips2021_clean$week_day <- ordered(trips2021_clean$week_day, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# from this we can count number of rides by weekdays and average
trips2021_clean %>% 
  group_by(week_day) %>% 
  summarise(ride_count = n(),
            '%' = (ride_count / nrow(trips2021_clean)) * 100,
            avg_ride_time = mean(ride_length_m))

# 4. hour of the day

# create another column for time_hour
trips2021_clean$time_hour <- format(trips2021_clean$started_at, '%H')

trips2021_clean %>% 
  group_by(time_hour) %>% 
  summarise(count_rides = n(),
            '%_of_rides' = (count_rides / nrow(trips2021_clean)) * 100,
            avg_ride_time = mean(ride_length_m))

" 5. Ride count by Month
  
  *Summer: June - August
  *Fall: September - November
  *Winter: December - February
  *Spring: March - May"

# Order the month
trips2021_clean$month <- ordered(trips2021_clean$month, levels=c(rep(month.name)))

trips2021_clean %>%
  group_by(month) %>%
  summarise(count = n(),
            "(%)" = (length(ride_id) / nrow(trips2021))* 100)

" 6. by Ride-type
    There are 3 types of bikes available in cyclistic:

    *Electric bike
    *Classic bike, and
    *Docked bike"

trips2021_clean %>%
    group_by(rideable_type)%>%
    summarise(count = n(),
             "in(%)" = (length(rideable_type) / nrow(trips2021_clean)) * 100)



