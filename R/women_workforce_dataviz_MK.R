########################################################################################
# Summary: Module 8 Homework: Advanced Data Viz with ggplot2
# Women in the Workforce
# Date: October 21, 2019
# Prepared by: Maddie Keefer
########################################################################################

# Data Load ---------------------------------------------------------------

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 


# Data Viz Homework (data from 'Women in the Workplace' #tidytuesday post) ---------------------------------
install.packages("viridis")
install.packages("colorspace")
library(viridis)
library(colorspace)
library(ggplot2)
library(tidyverse)

# Plot Number 1: Comparison of maximum recorded earnings for men and women in various job sectors in 2016
jobs_gender_16 <- jobs_gender %>%
  filter(year == 2016) %>%
  gather(gender_earnings, earnings, 10:11)

ggplot(jobs_gender_16, mapping = aes(x = major_category, y = earnings, fill = factor(gender_earnings))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  labs(y = "Earnings ($)", x = "Job Sector", title = "Maxiumum Recorded Earnings for Women and Men in 2016") + 
  scale_fill_manual(name = "Gender", values = c("#8da0cb", "#fc8d62"), labels = c("Women", "Men")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))

# Plot Number 2 Boxplot distribution of female earnings as a percentage of male earnings by age group
earnings_female %>%
  mutate(group = group) %>%
  group_by(group) %>%
  mutate(med_percent = median(percent)) %>%
  ggplot(earnings_female, mapping = aes(x = group, y = percent), fill = med_percent) + 
  geom_boxplot(aes(fill = med_percent)) +
  labs(x = 'Age Group', y = "Female Salary Percent of Male Salary", title = "Distribution of Gender Pay Gap by Age Group (1979 - 2011)") + 
  scale_fill_viridis(option = "plasma", name = "Median % of Male Salary", direction = -1) +
  theme_bw() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  
# Extra credit: Create an extremely ugly plot -----------------------------

ggplot(employed_gender, mapping = aes(x = year, y = full_time_female)) +
  geom_point(aes(color = factor(full_time_male))) +
  scale_color_brewer(palette = "Set1")



