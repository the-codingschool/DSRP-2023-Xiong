getwd()
csv <- read.csv("data/apartments_for_rent_classified_10K.csv", sep = ";")
head(csv)
colnames(csv2)
csv$id[1]
csv$category[1]
csv$title[1]
csv$body[1]
csv$amenities[1]
csv$bathrooms[1]
csv$bedrooms[1]
csv$currency[1]
csv$fee[1]
csv$has_photo[1]
csv$pets_allowed[1]
csv$price[1]
csv$price_display[1]
csv$price_type[1]
csv$square_feet[1]
csv$address[1]
csv$cityname[1]
csv$state[1]
csv$latitude[1]
csv$longitude[1]
csv$source[1]
csv$time[1]


unique(csv$category)
unique(csv$title)
unique(csv$body)
unique(csv$amenities)
unique(csv$bathrooms)
unique(csv$bedrooms)
unique(csv$currency)
unique(csv$fee)
unique(csv$has_photo)
unique(csv$pets_allowed)
unique(csv$price)
unique(csv$price_display)
unique(csv$price_type)
unique(csv$square_feet)
unique(csv$address)
unique(csv$cityname)
unique(csv$state)
unique(csv$latitude)
unique(csv$longitude)
unique(csv$source)
unique(csv$time) # Unix epoch time

library(dplyr)
filter(csv, cityname == "Pittsburgh")
filter(csv, cityname == "Washington")
min(csv$price)

## Making changes

#Currency is only in USD, so we can get rid of it

csv2 <- select(csv, -currency, -fee, price_display)
colnames(csv2)


## Converting time

#install.packages("lubridate")
library(lubridate)

as_datetime(csv$time[1])

csv2$time <- as_datetime(csv2$time)

## clean up category

csv2$category <- gsub("housing/rent/", "", as.character(csv2$category))

unique(csv2$category)

##
library(ggplot2)

ggplot(data = csv2, aes(x = square_feet , y = price)) +
geom_point(size = 0.75) +
  geom_smooth()

length(csv2$square_feet)


## 
ggplot(data = csv, aes(x = has_photo, y = price)) +
  geom_point()


##
ggplot(data = csv2, aes(x = bathrooms, y = square_feet)) +
  geom_point(size = 0.75) +
  geom_smooth()

mean(csv2$price)
wton <- filter(csv2, cityname == "Washington")
dc <- filter(csv2, state == "DC")
dc$
ggplot(data = dc, aes(x = pets_allowed , y = price)) +
  geom_point() +
  geom_smooth()


## trying to map dc adresses using latitude and longitude


dctest <- select(dc, latitude, longitude)

dctest <- distinct(dctest)

dctest$latitude <- as.numeric(dctest$latitude)
dctest$longitude <- as.numeric(dctest$longitude)

#duplicated_rows <- duplicated(dctest)
#sum(duplicated_rows)
#install.packages("leaflet")
#install.packages("ggmap")
library(leaflet)
#library(ggmap)

map <- leaflet(data = dctest) %>%
  addTiles()

map <- map %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    #popup = ~paste("latitude: ", latitude, "<br>longitude: ", longitude)
  )

map







ggplot(data = dc, aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Map of Locations")



filter(csv2, state == "DC")
filter(csv2, cityname == "Washington")


## Start working with Station Data

metro <- read.csv("C:/Dev/DSRP-2023-Di/Yuriy/data/Metro_Stations_Regional.csv")
colnames(metro)
head(metro)
metro2 <- select(metro, X, Y)


