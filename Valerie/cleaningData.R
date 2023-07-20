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

W_SF <- data |>
  filter(cityname == c("Washington","San Francisco", state = c("DC","CA"))) 

str(W_SF)

ggplot(data = W_SF, aes(x = cityname, y = price)) +
  geom_violin(color = "darkblue", fill = "lightblue") +
  geom_boxplot( width = 0.2, fill = NA)


ggplot(data = W_SF, aes(x = price, y = square_feet)) +
  geom_line(stat = "summary",
            fun = "mean")
  
ggplot(data = W_SF, aes(x = price, y = square_feet)) +
  geom_point() +
  geom_smooth()
  
Washington <- mutate(W_SF, cityname = "Washington", state = "DC")
SanFrancisco <- mutate(W_SF, cityname = "San Francisco", state = "CA")

select(Washington, title, square_feet, price)
select(SanFrancisco, title, square_feet, price)

ggplot(data = Washington, aes(x = price, y = square_feet)) +
  geom_point() 
 






