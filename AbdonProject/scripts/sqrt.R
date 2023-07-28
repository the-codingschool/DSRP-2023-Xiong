library("readr")
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)

data <- read_delim("data/apartments_for_rent_classified_10K.csv", delim = ";")
View(data)

?read_delim

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


# Mutate - Regions
region <- apt %>%
  mutate(region = case_when(
    state %in% c("DC", "NY", "NJ", "CT", "RI", "MA", "VT", "NH", "ME") ~ "Northeast",
    state %in% c("VA", "NC", "GA", "FL", "MD", "DE", "SC", "AL", "TN", "KY", "WV", "MS") ~ "Southeast",
    state %in% c("IN", "IL", "OH", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
    state %in% c("TX", "AZ", "NM", "OK") ~ "Southwest",
    state %in% c ("WA", "CA", "OR", "NV", "UT", "CO", "AK", "MT", "WY", "ID", "HI") ~ "West"
    
  ))


View(region)

# Mutate - Small Medium Large
min(region$square_feet)
mean(region$square_feet)
max(region$square_feet)


small <- 600
medium <- 1000

size <- region %>% 
  mutate(size = case_when(
    square_feet < small ~ "Small",
    square_feet < medium ~ "Medium",
    TRUE ~ "Large"
  ))

size <- na.omit(size)
View(size)
# REGION + SQRT  
sqrt_region <- size %>%
  group_by(region, size)
View(sqrt_region)

na.omit(sqrt_region)
# Avg cost
result <- sqrt_region %>%
  group_by(region, size) %>%
  summarise(average_cost = mean(price))
View(result)

# Create the Bar Plot
ggplot(result, aes(x = region, y = average_cost, fill = size)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Apartment Cost by Region and Size",
       x = "Region",
       y = "Average Cost") +
  theme_minimal()


#Separate apt sizes 
west<-filter(sqrt_region, region == "West")
midwest<-filter(sqrt_region, region == "Midwest")
northeast<-filter(sqrt_region, region == "Northeast")
southeast<-filter(sqrt_region, region == "Southeast")
southwest<-filter(sqrt_region, region=="Southwest")

west_large<- filter(west, size == "Large")
midwest_large<-filter(midwest, size == "Large")
northeast_large<-filter(northeast, size == "Large")
southeast_large<-filter(southeast, size == "Large")
southwest_large <- filter(southwest, size == "Large")


large_midwest <- filter(sqrt_region, region == "Midwest" & size == "Large") #Filter large apartments for Midwest and West regions 
large_west <- filter(sqrt_region, region == "West" & size == "Large")

View(large_west)
lm<-head(large_midwest)
lw<- head(large_west)

lmw<- table(lm$price, lw$price)

t.test(lm$price, lw$price)

# Perform ANOVA
# Combine large apartments data from all regions
large_region<-filter(sqrt_region, size == "Large")

large_region <- rbind(west_large, midwest_large, northeast_large, southeast_large, southwest_large )

anova_result <- aov(price ~ region, data = large_region)
      
p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
print(p_value)









# Filter large apartments for Midwest and Northeast regions
large_midwest <- filter(sqrt_region, region == "Midwest" & size == "Large")
large_northeast <- filter(sqrt_region, region == "Northeast" & size == "Large")

View(large_west)
lm<-head(large_midwest)
ne<- head(large_northeast)

t.test(lm$price,ne$price)


# Avg cost
lg_mid <- filter(sqrt_region, region=="Midwest")
lg_west <- filter(sqrt_region, region=="West")
                    
lg_west_mid <- rbind(lg_mid, lg_west)                         

# Create the scatter plot
ggplot(lg_west_mid, aes(x = size, y = price, fill = region)) +
  geom_bar(stat = "Identity", position = "dodge") +
  ggtitle("pricing of Large Apartments in West & Midwest")




#How does region affect the rent of similar size apartments
#Goal is to find if there is noticeable difference between the price of an apartment when comparing similar-size apartments among the regions.
#Analysis: Find if certain region have cheaper or higher price 