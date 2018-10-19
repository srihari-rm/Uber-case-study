###### UBER CASE STUDY ###### 

# Import necessary libraries
library(stringr)
library(ggplot2)
library(dplyr)

# Read data files into R
UberReqData<- read.csv("Uber request data.csv")

# Data cleaning is done using excel. Verified for all NA and duplicate values.

# Making default time separator as '-'
UberReqData$req_time <- str_replace_all(UberReqData$Request.timestamp, "[/]",  "-")
UberReqData$drop_time <- str_replace_all(UberReqData$Drop.timestamp, "[/]",  "-")
##  UberReqData <- UberReqData[,-5]                                 # Delete the previously existing unformatted column
##  UberReqData <- UberReqData[,-6]                                 # Delete the previously existing unformatted column
View(UberReqData)                                           # View the table to verify

# Change the time column to Date time object
UberReqData$req_time <- as.POSIXlt(UberReqData$req_time, format = "%d-%m-%Y %H:%M")         # Create new column for request time
UberReqData$drop_time <- as.POSIXlt(UberReqData$drop_time, format = "%d-%m-%Y %H:%M")       # Create new column for drop time


View(UberReqData)                                           # View the table to verify

# Create new column for hour and day data from the request time
UberReqData$req_hour <- format(UberReqData$req_time, "%H")
UberReqData$day <- format(UberReqData$req_time, "%d")

View(UberReqData)                                           # View the table to verify
write.csv(UberReqData,'uber.csv')                           # Imported to csv to check how many days' data are present - only 5 days data exist (11th, 12th, 13th, 14th aand 15th)

# Plotting the data

# Overall No of requests during the time of day along with pickup point
plot1 <- ggplot(UberReqData, aes(x = as.factor(req_hour),fill = Pickup.point))+geom_bar(position = "dodge")+labs(x = "Hour", y = "No. of Requests", fill = "Pickup Point" )
plot1

# Since request hour is in date format, converting it to numeric format
UberReqData$req_hour <- as.numeric(UberReqData$req_hour)
# Segregating it to different slots in the day
UberReqData$time_slot = ifelse(UberReqData$req_hour < 5, "Early_Morning", 
                               ifelse(UberReqData$req_hour < 10,"Morning",
                                      ifelse(UberReqData$req_hour < 17,"Mid_day",
                                             ifelse(UberReqData$req_hour < 22,"Late_evening",
                                                    "Night"))))

# Number of trips throughout the day
Early_morning <- subset(UberReqData, UberReqData$time_slot == "Early_Morning")
nrow(Early_morning)                 # Total of 578 trips in early morning

Morning <- subset(UberReqData, UberReqData$time_slot == "Morning")
nrow(Morning)                       # Total of 2103 trips in the morning

Mid_day <- subset(UberReqData, UberReqData$time_slot == "Mid_day")
nrow(Mid_day)                       # Total of 1224 trips during mid day

Late_evening <- subset(UberReqData, UberReqData$time_slot == "Late_evening")
nrow(Late_evening)                  # Total of 2342 trips in the evening

Night <- subset(UberReqData, UberReqData$time_slot == "Night")
nrow(Night)                         # Total of 498 trips in the night

# Day wise No of requests during the time of day along with status of requests
plot2 <- ggplot(UberReqData, aes(x = as.factor(req_hour),fill = Status))+geom_bar(position = "dodge")
plot2 + facet_wrap( ~ UberReqData$day, nrow =5, ncol = 1) + labs(x = "Hour", y = "No. of Requests", fill = "Status" )

# Day wise No of requests during the time of day along with pickup point
plot3 <- ggplot(UberReqData, aes(x = as.factor(req_hour),fill = Pickup.point))+geom_bar(position = "dodge")
plot3 + facet_wrap( ~ UberReqData$day, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )


# Plot for Number of requests vs the time slot along with the trip status information
plot4 <- ggplot(UberReqData, aes(x = as.factor(time_slot), fill= as.factor(UberReqData$Status))) + geom_bar()+labs(x = "Time Slot", y = "No. of Requests", fill = "Status" )
plot4

# Reasons for cancellations during the morning time
morning_trips <- subset(UberReqData,time_slot=="Morning")
nrow(morning_trips)        # Total of 2103 requests in the morning time
plot5 <- ggplot(morning_trips, aes(x = as.factor(Pickup.point), fill= as.factor(morning_trips$Status))) + geom_bar() +labs(x = "Pickup Point", y = "No. of Requests", fill = "Status" )
plot5

# Reason for cancellation based on location
airport_cancel <- subset(morning_trips, morning_trips$Pickup.point == "Airport" & morning_trips$Status == "Cancelled")
nrow(airport_cancel)        # Total of 23 requests are cancelled in the morning times when the pick up point is airport

city_cancel <- subset(morning_trips, morning_trips$Pickup.point == "City" & morning_trips$Status == "Cancelled")
nrow(city_cancel)           # Total of 820 requests are cancelled in the morning time when the pink up point is city

# Pie chart showing the status of cancellation in the morning time
morning_trips_requests <- subset(morning_trips, Pickup.point %in% "City")
ggplot(morning_trips_requests, aes(x = morning_trips_requests$Pickup.point, fill= as.factor(morning_trips_requests$Status))) + geom_bar() + coord_polar(theta = "y", start=0)+ labs( y = "No. of Requests", x = "", fill = "Status")

# Availability and Requirment of cars during the morning time
morning_trips_completed <- subset(morning_trips, morning_trips$Pickup.point == "City" & morning_trips$Status == "Trip Completed")
nrow(morning_trips_completed)     # Total of 472 trips in the morning are completed starting from city
morning_trips_city <- subset(morning_trips, morning_trips$Pickup.point == "City")
nrow(morning_trips_city)          # Total of 1677 trips are requested from city in the morning time

# Unavailability of cars during the evening times
evening_trips <- subset(UberReqData,time_slot=="Late_evening")
ggplot(evening_trips, aes(x = as.factor(Pickup.point), fill= as.factor(evening_trips$Status))) + geom_bar()+labs(x = "Pickup Point", y = "No. of Requests", fill = "Status" )

# Unavailability of cars during evening times based on location
airport_unavailability <- subset(evening_trips, evening_trips$Pickup.point == "Airport" & evening_trips$Status == "No Cars Available")
nrow(airport_unavailability)      # Total of 1321 requests had No cars available at the airport in the evening time

city_unavailability <- subset(evening_trips, evening_trips$Pickup.point == "City" & evening_trips$Status == "No Cars Available")
nrow(city_unavailability)         # Total of 71 requests had No cars available in the city during the evening times

# Pie chart showing unavailability of cars in the evening times
eve_airport_unavailable <- subset(evening_trips, Pickup.point %in% "Airport")
ggplot(eve_airport_unavailable, aes(x = eve_airport_unavailable$Pickup.point, fill= as.factor(eve_airport_unavailable$Status))) + geom_bar() + coord_polar(theta = "y", start=0) + labs( y = "No. of Requests", x = "", fill = "Status")

# Availability and Requirment of cars during the evening time
eve_airport_tripcompleted <- subset(evening_trips, evening_trips$Pickup.point == "Airport" & evening_trips$Status == "Trip Completed")
nrow(eve_airport_tripcompleted)   # Total of 373 trips in the evening are completed starting from airport

eve_from_airport <- subset(evening_trips, evening_trips$Pickup.point == "Airport")
nrow(eve_from_airport)            # Total of 1800 requests in the evening times are from airport
