library(readr)
apartments_for_rent_classified_10K <- read_csv("data/apartments_for_rent_classified_10K.csv")
View(apartments_for_rent_classified_10K)

library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

data <- read_delim ("data/apartments_for_rent_classified_10K.csv", delim = ";")
View(data)
str(data)

## Regions #### 

clean_data <- select(data,
                     -body,
                     -amenities,
                     -source, 
                     -time, 
                     -fee, 
                     -has_photo, 
                     -currency,
                     -pets_allowed,
                     -price_display)


West <- filter(clean_data, state == c("WA","MT","OR","ID","WY","NV","UT","CO","CA","AZ","NM"))
East <- filter(clean_data, state == c("PA","NY","VT","NH","ME","MA","CT","NJ","RI"))
Midwest <- filter(clean_data, state == c("ND","SD","MN","IA","NE","KS","MO","IL","WI","IN","OH","MI"))
South <- filter(clean_data, state == c("TX","OK","AR","LA","MS","AL","TN","KY","GA","FL","SC","NC","WV","VA","MD","DE","DC"))

Regions <- clean_data %>% 
  mutate(region = case_when(
    state %in% c("WA","MT","OR","ID","WY","NV","UT","CO","CA","AZ","NM") ~ "West",
    state %in% c("PA","NY","VT","NH","ME","MA","CT","NJ","RI") ~ "East",
    state %in% c("ND","SD","MN","IA","NE","KS","MO","IL","WI","IN","OH","MI") ~ "Midwest",
    state %in% c("TX","OK","AR","LA","MS","AL","TN","KY","GA","FL","SC","NC","WV","VA","MD","DE","DC") ~ "South"))

Regions <- na.omit(Regions)
str(Regions)

small <- min(Regions$square_feet)
medium <- mean(Regions$square_feet)
large <- max(Regions$square_feet)

Regions <- Regions %>%
  mutate(size = case_when(
    square_feet %in% 100:700 ~ "small",
    square_feet %in% 700:2000 ~ "medium",
    square_feet %in% 2000:40000 ~ "large"))

Regions2 <- Regions %>%
  group_by(region, size) %>%
  summarise(average_cost = mean(price))

## Visualization

ggplot(Regions2, aes(x = region, y = average_cost, fill = size)) +
  geom_bar(stat = "Identity", position = "dodge") +
  ggtitle("Average Apartment Cost by Region and Size")

## t-test

WEST <- filter(Regions2, region == "West")
EAST <- filter(Regions2, region == "East")
MIDWEST <- filter(Regions2, region == "Midwest")
SOUTH <- filter(Regions2, region == "South")

str(Regions2)

t.test(Regions2$average_cost, paired = F)
