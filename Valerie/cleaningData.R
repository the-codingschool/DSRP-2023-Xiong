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



ggplot(w_e_large, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") + 
  ggtitle("Pricing of Large Apartments In West & East")


ggplot(w_e_medium, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") + 
  ggtitle("Pricing of Medium Apartments In West & East")


ggplot(w_e_small, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") + 
  ggtitle("Pricing of Small Apartments In West & East")


ggplot(w_s_large, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") + 
  ggtitle("Pricing of Large Apartments In West & South")

ggplot(w_s_medium, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") + 
  ggtitle("Pricing of Medium Apartments In West & South")


ggplot(Regions2, aes(x = region, y = average_cost, fill = size)) +
  geom_bar(stat = "Identity", position = "dodge") +
  ggtitle("Average Apartment Cost by Region and Size")



## t-test

WEST <- filter(Regions, region == "West")


EAST <- filter(Regions, region == "East")


MIDWEST <- filter(Regions, region == "Midwest")


SOUTH <- filter(Regions, region == "South")


west_large <- filter(WEST, size == "large")
west_medium <- filter(WEST, size == "medium")
west_small <- filter(WEST, size == "small")

east_large <- filter(EAST, size == "large")
east_medium <- filter(EAST, size == "medium")
east_small <- filter(EAST, size == "small")

south_large <- filter(SOUTH, size == "large")
south_medium <- filter(SOUTH, size == "medium")
south_small <- filter(SOUTH, size == "small")

midwest_large <- filter(MIDWEST, size == "large")
midwest_medium <- filter(MIDWEST, size == "medium")
midwest_small <- filter(MIDWEST, size == "small")


w_e_large <- rbind(west_large, east_large)
w_e_medium <- rbind(west_medium, east_medium)
w_e_small <- rbind(west_small, east_small)

w_s_large <- rbind(west_large, south_large)
w_s_medium <- rbind(west_medium, south_medium)
w_s_small <- rbind(west_small, south_small)

w_m_large <- rbind(west_large, midwest_large)
w_m_medium <- rbind(west_medium, midwest_medium)
w_m_small <- rbind(west_small, midwest_small)






Regions_large <- filter(Regions, size == "large")
Regions_medium <- filter(Regions, size == "medium")
Regions_small <- filter(Regions, size == "small")


t.test(west_large$price, east_large$price)
t.test(west_large$price, south_large$price)
t.test(west_large$price, midwest_large$price)



anova_results <- aov(price ~ region, data = Regions_large)
summary(anova_results)
TukeyHSD(anova_results)                          

p_value <- summary(anova_results)[[1]]$"Pr(>F)"[1]
print(p_value)

anova_results2 <- aov(price ~ region, data = Regions_medium)
summary(anova_results2)
TukeyHSD(anova_results2)

p_value2 <- summary(anova_results2)[[1]]$"Pr(>F)"[1]
print(p_value2)



anova_results3 <- aov(price ~ region, data = Regions_small)
summary(anova_results3)
TukeyHSD(anova_results3)














