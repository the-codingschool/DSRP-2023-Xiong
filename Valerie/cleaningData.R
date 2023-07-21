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

UT_SF <- data |>
  filter(cityname == c("Salt Lake City","San Francisco", state = c("UT","CA"))) 

str(UT_SF)

ggplot(data = UT_SF, aes(x = cityname, y = price)) +
  geom_violin(color = "darkblue", fill = "lightblue") +
  geom_boxplot( width = 0.2, fill = NA)


ggplot(data = UT_SF, aes(x = price, y = square_feet)) +
  geom_line(stat = "summary",
            fun = "mean")
  
ggplot(data = UT_SF, aes(x = price, y = square_feet)) +
  geom_point() +
  geom_smooth()
  
Salt <- mutate(UT_SF, cityname = "Salt Lake City", state = "UT")
San <- mutate(UT_SF, cityname = "San Francisco", state = "CA")

select(Salt, title, square_feet, price)
select(San, title, square_feet, price)

ggplot(data = Salt, aes(x = price, y = square_feet)) +
  geom_point() 
 
ggplot(data = San, aes(x = price, y = square_feet)) +
  geom_point()


