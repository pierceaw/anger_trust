library(dplyr)
library(car)
library(stargazer)
library(xtable)
library(ggplot2)
library(reshape2)

anger <- read.csv("Data/experimental_data.csv")

# Clean DVs
anger$govt_unresponsive <- as.numeric(as.character(anger$govt_unresponsive))
anger$govt_unresponsive[anger$govt_unresponsive==-99] <- NA

# Non-white indicator
anger$race_eth <- as.numeric(as.character(anger$race_eth))
anger$race_eth[anger$race_eth==-99] <- NA
anger <- anger %>% mutate(nonwhite = as.numeric(race_eth != 1))

# Create indicators for treatment status
anger <- anger %>% mutate(anger_treated = as.numeric(anger != ""),
                          anger_salience_treated = as.numeric(anger_politics != ""),
                          salience_treated = as.numeric(salience != ""),
                          control = as.numeric(breakfast_control != ""))

# Regressions
m1 <- lm(govt_unresponsive ~ anger_treated + anger_salience_treated + salience_treated, data = anger)
m2 <- lm(govt_unresponsive ~ anger_treated + anger_salience_treated + salience_treated, data = subset(anger, nonwhite == 1))
