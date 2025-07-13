# Sourcing the packages for data prep
source("config.r")


cleaned_data <- read.csv("data_insight/202301-divvy-tripdata-cleaned.csv")


## Insight - A##
### Ride Duration Analysis###

# Understand how members vs. casuals behave differently in terms of trip lengths.
cleaned_data %>%
  group_by(member_casual) %>%
  summarise(
    avg_duration = mean(ride_duration),
    median_duration = median(ride_duration),
    max_duration = max(ride_duration),
    count = n()
  )


## Insight - B##
### Day of Week Analysis###

# To check if casual users might ride more on weekends, members more on weekdays
cleaned_data <- cleaned_data %>%
  mutate(day_of_week = lubridate::wday(started_at, label = TRUE))


cleaned_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(avg_duration = mean(ride_duration), ride_count = n()) %>%
  arrange(member_casual, day_of_week)


##Insight - C##
### Hourly Analysis###

# Helps identify peak hours for different user types
cleaned_data <- cleaned_data %>%
  mutate(started_at = ymd_hms(started_at))

cleaned_data <- cleaned_data %>%
  mutate(hour_of_day = lubridate::hour(started_at))

hourly_summary <- cleaned_data %>%
  group_by(member_casual, hour_of_day) %>%
  summarise(ride_count = n(), .groups = "drop") %>%
  arrange(member_casual, hour_of_day)

print(hourly_summary)


##Insight - D##
### Popular Stations Analysis###

# Helps identify the most-used pickup points
cleaned_data %>%
  group_by(start_station_name) %>%
  summarise(ride_count = n()) %>%
  arrange(desc(ride_count)) %>%
  head(10)


##Insight - E##
### Trip flow analysis###

# To spot common travel patterns
cleaned_data %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name)) %>%
  group_by(start_station_name, end_station_name) %>%
  summarise(route_count = n()) %>%
  arrange(desc(route_count)) %>%
  head(10)


## Insight - F##
## Bike Type Preferences##

# To understand which bikes are preferred by which user group
cleaned_data %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n()) %>%
  arrange(member_casual, desc(count))


## Insight - G##
### Seasonal or Monthly Trends###

# Useful for seasonal planning
cleaned_data <- cleaned_data %>%
  mutate(month = lubridate::month(started_at, label = TRUE))

cleaned_data %>%
  group_by(month, member_casual) %>%
  summarise(ride_count = n()) %>%
  arrange(month)


## Insight - H##
### Distance estimation analysis###

# To analyze how far people travel
cleaned_data <- cleaned_data %>%
  mutate(
    distance_km = distHaversine(
      cbind(start_lng, start_lat),
      cbind(end_lng, end_lat)
    ) / 1000
  )



