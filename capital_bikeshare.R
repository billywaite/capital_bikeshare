
# Explore just one of the data sets
# Create duration based on start and end date
# Create distance traveled, probably have to bring in lat / lon
# Calculate speed
# Group by neighborhoods
# Group by Quandrants

# Analysis variables - average trip distance, average trip duration, average trip speed
# facet by member type, average distance by bike
# Most popular hour, day, month, visuals

# Because of the size of the data, it probably makes sense to create tables of summary statistics and then
# upload those summary tables to a public tableau dashboard

# for tableau, want to create a dashboard where you can filter by data - but most importantly
# want to filter by start station and end station so you can see optimal time to ride by day
# and itwill show you distance, average time other riders take to travel that distance etc.

# which bike is the most ridden, aka the hussie

#################################################################
#                     Set-up & Data Import                      #
#################################################################
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)

setwd("/Users/wwaite/Dev/R/Personal/capital_bikeshare/data_import")

q1_2017 <- read.csv("2017-Q1-Trips-History-Data.csv")

# Store in tbl
df <- tbl_df(q1_2017)

# Rename variables
df <- df %>%
  rename(duration = Duration,
         start_date = Start.date,
         end_date = End.date,
         start_station_number = Start.station.number,
         start_station = Start.station,
         end_station_number = End.station.number,
         end_station = End.station,
         bike_number = Bike.number,
         member_type = Member.Type)

# Convert data type to date
df$start_date <- mdy_hm(df$start_date)
df$end_date <- mdy_hm(df$end_date)

################################################################
#                 Visualizations on existing data              #
################################################################

# Remove existing duration column and create new duration column
df1 <- df %>%
  select(2:9)

df1$trip_duration <- as.numeric(df1$end_date - df1$start_date)

# Quick analysis of the data
summary(df1)

# Visualize Casual vs. Registered member type
viz1 <- ggplot(data = df1, aes(member_type)) +
  geom_histogram(stat = "count", aes(fill = member_type, alpha = 0.8)) +
  guides(fill = FALSE, alpha = FALSE) +
  xlab("Member Type") + ylab("Total Rides") +
  ggtitle("Total Rides by Bike Membership Type")

# Build data table for rides less than 30 minutes and above
df1$large_trip_duration <- ifelse(df1$trip_duration < 30, FALSE, TRUE)

# Visualize proportion of membership type for rides less than 30 minutes and above.
viz2 <- ggplot(data = df1, aes(large_trip_duration)) +
  geom_histogram(position = "fill", stat = "count", aes(fill = member_type, alpha = 0.8)) +
  guides(alpha = FALSE) +
  xlab("Trip Duration less than 30 Minutes?") + ylab("Proportion of Total Rides") +
  ggtitle("Proportion of Total Rides Less than 30 Minutes by Member Type")

# Visualize distribution of rides by day and hour of the week
viz3 <- ggplot(data = df1, aes(hour(start_date))) + 
  geom_density(aes(fill = factor(wday(start_date))), alpha = 0.7) +
  labs(title="When Bikers Bike", 
       subtitle="Density of Bike Rides by Hour of the Day Grouped by Day of the Week",
       x="Hour of the Day",
       fill="Day of the Week")


##########################################################################

# Figure out average trip distance

# Import data from DC Open data set, contains latitude & longitude
location <- read.csv("Capital_Bike_Share_Locations.csv")

location <- location %>%
  select(TERMINAL_NUMBER, LATITUDE, LONGITUDE)

# Join the latitude and longitude into the main data frame
df2 <- inner_join(df1, location, by = c("start_station_number" = "TERMINAL_NUMBER"))

df2 <- df2 %>%
  rename(start_lat = LATITUDE,
         start_long = LONGITUDE)

df3 <- inner_join(df2, location, by = c("end_station_number" = "TERMINAL_NUMBER"))

df3 <- df3 %>%
  rename(end_lat = LATITUDE,
         end_long = LONGITUDE)

# load library to calculate distance
library(geosphere)

# create test set for map function
test <- df3[1:100, ]

# Calculate distance using latitude and longitude
df3$meters <- 0

for (i in 1:nrow(df3)) {
  df3$meters[i] <- distm(c(df3$start_long[i], df3$start_lat[i]), 
                          c(df3$end_long[i], df3$end_lat[i]), fun = distHaversine)
  print(i)
}

# Convert meters to miles
df3$miles <- df3$meters / 1609.344

# Calculate speed in mph
df3$mph <-df3$miles / (df3$trip_duration / 60)


mph_df <- df3 %>%
  filter()
# Visualize speed by day of the week
viz4 <- ggplot(data = df3, aes(mph)) +
  geom_histogram(aes(fill = wday(start_date)))
###############################################################################

setwd("/Users/wwaite/Dev/R/Personal/capital_bikeshare/data_export")

write.csv(df3, "q12017_capital_bikeshare.csv")


