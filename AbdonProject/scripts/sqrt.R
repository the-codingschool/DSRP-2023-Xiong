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


# Mutate - Urban or Suburban
urban <- apt %>%
  mutate(urban = case_when(
    cityname %in% c("Tampa", "Seattle", "Nashville", "Washington", "Tucson", "San Francisco") ~ "Urban",
    cityname %in% c("Aurora", "Glen Burnie", "West Lafayette", "Riverview", "Overland Park") ~ "Suburban",
    TRUE ~ NA_character_
  ))

View(urban)
unique(urban$cityname)

# Mutate - Regions
region <- apt %>%
  mutate(region = case_when(
    state %in% c("DC", "NY", "NJ", "CT", "RI", "MA", "VT", "NH", "ME") ~ "Northeast",
    state %in% c("VA", "NC", "GA", "FL", "MD", "DE", "SC", "AL", "TN", "KY", "WV", "MS") ~ "Southeast",
    state %in% c("IN", "IL", "OH", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
    state %in% c("TX", "AZ", "NM", "OK") ~ "Southwest",
    state %in% c ("WA", "CA", "OR", "NV", "UT", "CO", "AK", "MT", "WY", "ID", "HI") ~ "West"
    
  ))

unique(urban$state)
View(region)

# Combine the "urban" and "region" data frames
cityregion <- merge(urban, region)

# Filter the data and remove rows with missing values
newcity <- cityregion %>%
  filter(!is.na(square_feet) & !is.na(price))

# Create the scatter plot
ggplot(region, aes(x = square_feet, y = price, color = region)) +
  geom_boxplot() +
  labs(x = "Square Footage (sqrft)", y = "Price", title = "Relationship between Price and Square Footage by Region") +
  theme_minimal()
