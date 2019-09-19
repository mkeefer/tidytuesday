#BAE 590 Module 4: tidytuesday
#Creating a data visualization for the tidytuesday submission named Wine Ratings, which is a wine-enthusiast ratings dataset from Github user Kaggle
#Prepared by: Maddie Keefer
#Date: 9/18/19

#load packages
library(tidyverse)
library(ggthemes)

#Load the dataset
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

# Quickly inspect the contents
head(wine_ratings)
str(wine_ratings)
#this dataset is made up of double (continuous) and character (discrete) variable types

#I want to see the median Tempranillo wine bottle costs for each Wine Enthusiast rating

#create data subset where variety = Tempranillo
temp_wine <- subset(wine_ratings, variety == "Tempranillo")

#remove Tempranillo subset NANs
temp_wine_clean <- na.omit(temp_wine)

#group Tempranillo wines by Wine Enthusiast score, and summarize by median bottle price
temp_price <- temp_wine %>% group_by(points) %>% summarize(price = median(price, na.rm = TRUE))

#create bar chart
ggplot() +
  geom_col(data = temp_price, mapping = aes(x = points, y = price)) +
  xlab("Wine Enthusiast Rating") +
  ylab("Median Bottle Price (USD)") +
  theme_gray()

ggsave("tempranillo price_vs_rating.pdf", width = 10, height = 8, units = "in")
