install.packages("readr")
library("readr")
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)

data <- read_delim("data/apartments_for_rent_classified_10K.csv", delim = ";")
View(data)

?read_delim

#How does the presence of specific amenities (e.g., pool, gym, parking) affect the price of apartments?
#Does the location of an apartment (latitude and longitude) have any impact on its price, and are there any clusters of high-priced or low-priced apartments?

head(data)
summary(data)
str(data)

# Remove unnecessary columns
new_data <- select(data,- title, -body, -has_photo, -source, -time)
View(new_data)

# Filter out apartments without amenities
amenities <- subset(new_data, !is.na(amenities) & amenities != "null")
View(amenities)


#Tidy Format
# 1.replace null with NA
apt<-new_data %>%
  mutate(
    amenities = ifelse(grepl("null", amenities), NA, amenities),
    bathrooms = ifelse(grepl("null", bathrooms), NA, bathrooms),
    address = ifelse(grepl("null", address), NA, address))  
View(apt)

# 2. Choose a City
tm <- apt |> filter(cityname== "Tampa" | cityname== "Washington")
View(tm)

##Visualization
#Filter data for Tampa and Washington and select only the relevant columns
filtered_data <- tm %>%
  filter(cityname %in% c("Tampa", "Washington"), grepl("Dishwasher|Pool", amenities))%>% 
          
  select(cityname, amenities, price)


#Only Dishwasher and Pool

# Create a box plot to visualize the impact of amenities on price
ggplot(filtered_data, aes(x = amenities, y = price, fill = cityname)) +
  geom_boxplot() +
  labs(x = "Amenities", y = "Price", fill = "City") +
  theme_minimal()

unique(apt$amenities)
