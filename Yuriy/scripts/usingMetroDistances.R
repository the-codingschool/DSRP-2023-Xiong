## Start working with Station Data

metro <- read.csv("C:/Dev/DSRP-2023-Di/Yuriy/data/Metro_Stations_Regional.csv")
colnames(metro)
head(metro)
metro2 <- select(metro, X, Y)
metro3 <- metro2 %>% rename("Longitude" = "X", "Latitude" = "Y")
metro4 <- select(metro3, Latitude, Longitude)
metro4

colnames(dc)
unique(dc$bathrooms)
dc_useful <- select(dc, latitude, longitude, price, square_feet, bedrooms)

dc_useful$latitude <- as.numeric(dc_useful$latitude)
dc_useful$longitude <- as.numeric(dc_useful$longitude)
dc_useful$price <- as.numeric(dc_useful$price)
dc_useful$square_feet <- as.numeric(dc_useful$square_feet)
dc_useful$bedrooms <- as.numeric(dc_useful$bedrooms)
dc_useful

## Start finding geospatial distances

library(geosphere)

# Create a matrix to store the distances between each apartment and all metro stations
distances_to_metro <- matrix(NA, nrow = nrow(dc_useful), ncol = nrow(metro4))

# Calculate the distances between each apartment and all metro stations
for (i in 1:nrow(dc_useful)) {
  for (j in 1:nrow(metro4)) {
    distances_to_metro[i, j] <- vincenty_distance(dc_useful[i, "latitude"], dc_useful[i, "longitude"],
                                                  metro4[j, "Latitude"], metro4[j, "Longitude"])
  }
}

# Find the index of the nearest metro station for each apartment
nearest_metro_index <- apply(distances_to_metro, 1, which.min)

# Create a new data frame with the nearest metro station information
nearest_metro_info <- metro4[nearest_metro_index, c("Latitude", "Longitude")]
colnames(nearest_metro_info) <- c("Nearest_Metro_Latitude", "Nearest_Metro_Longitude")

# Add the distance to the nearest metro station and nearest metro info to dc_useful
dc_useful <- cbind(dc_useful, distance_to_nearest_metro = apply(distances_to_metro, 1, min))
dc_useful <- cbind(dc_useful, nearest_metro_info)

# Perform correlation analysis
correlation <- cor(dc_useful$distance_to_nearest_metro, dc_useful$price)
correlation #0.101543

head(dc_useful)

library(dplyr)

colnames(dc_useful)

dc_with_distance <- dc_useful[c(3,4,5,6)]

dc_with_distance$Index <- rownames(dc_with_distance)
rownames(dc_with_distance) <- NULL

#dc_with_distance is finished

## Now to organize by apartment complex
## goal is to make graph with all 9 complexes by closeness to metro with all price per square foot
unique(dc_with_distance$distance_to_nearest_metro)
filter(dc_with_distance, distance_to_nearest_metro == 777.7386)

#delete bad index
dc_with_distance <- select(dc_with_distance, -Index)
dc_with_distance

#make price per square foot
dc_ppsf <- mutate(dc_with_distance,
                       price_per_sqfoot = price/square_feet)
dc_ppsf

#make complex colum
dcx <- mutate(dc_ppsf,
              complex = "1")

dcy <- dcx
length(unique(dcy$distance_to_nearest_metro))
if(dcy$distance_to_nearest_metro[i] == 1473.3364)
unqdist = unique(dcy$distance_to_nearest_metro)  

for(i in 1:nrow(dcy)){
  for(j in 1:9){
    if(dcy$distance_to_nearest_metro[i] == unqdist[j]){
      dcy$complex[i] <- j
    }
  }
}#working

dcy
select(dcy, distance_to_nearest_metro, complex)

dc_with_complex <- dcy

## now to make line plot displaying price per square foot(x) and distance from metro(y) with each line being a different color

ggplot(data = dc_with_complex, aes(x = price_per_sqfoot, y = distance_to_nearest_metro, group = complex, color = complex)) +
  geom_point() +
  #geom_smooth(method = "lm", se=FALSE, fullrange = FALSE) +
  labs(x = "price ($) per square foot",
       y = "distance to nearest metro (meters)")


## ML Models

## PCA
dc_PCA <- as.data.frame(apply(dc_with_complex, 2, as.numeric))
pcas <- prcomp(dc_PCA, scale. = T)
summary(pcas)
pcas$rotation

var_percs <- as.data.frame(pcas$rotation^2)


pca_vals <- as.data.frame(pcas$x)
pca_vals$distance_to_nearest_metro <- dc_PCA$distance_to_nearest_metro

ggplot(pca_vals, aes(PC1, PC2,)) +
  geom_point() +
  theme_minimal()
#


##Linear Regression Model

#remove complex
dc_reg <- select(dc_PCA, -complex, -price_per_sqfoot)

library(rsample)

reg_split <- initial_split(dc_reg, prop = .75) #use 75% of data for training

reg_train <- training(reg_split)
reg_test <- testing(reg_split)

library(parsnip)

lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(distance_to_nearest_metro ~ price + square_feet + bedrooms,
      data = reg_train)

lm_fit$fit
summary(lm_fit$fit)


reg_results <- reg_test

lm_pred <- predict(lm_fit, reg_test)$.pred
lm_pred <- round(lm_pred)

reg_results$predicted_distance <- lm_pred

yardstick::mae(reg_results, truth = distance_to_nearest_metro, estimate = predicted_distance)

yardstick::rmse(reg_results, truth = distance_to_nearest_metro, estimate = predicted_distance)




##Linear Regression with price per square foot
dc_ppsf_reg <- select(dc_PCA, -complex, -bedrooms, -square_feet, -price)

reg_split <- initial_split(dc_ppsf_reg, prop = .75) #use 75% of data for training

reg_train <- training(reg_split)
reg_test <- testing(reg_split)

lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(distance_to_nearest_metro ~ price_per_sqfoot,
      data = reg_train)

lm_fit$fit
summary(lm_fit$fit)


reg_results <- reg_test

lm_pred <- predict(lm_fit, reg_test)$.pred
lm_pred <- round(lm_pred)

reg_results$predicted_distance <- lm_pred

yardstick::mae(reg_results, truth = distance_to_nearest_metro, estimate = predicted_distance)

yardstick::rmse(reg_results, truth = distance_to_nearest_metro, estimate = predicted_distance)


## counts of apartments in complexes 
ggplot(data = dc_with_complex, aes(x = complex, fill = complex)) +
  geom_bar() +
  labs(x = "apartment complex",
       y = "number of apartments")




















