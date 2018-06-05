#
# R script for experimental analysis
#

library(dplyr)
library(ggplot2)

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

# Coefficient plot
# Nonwhite
Treatment <- c("Anger", "Political Anger", "Political Salience")
Nonwhite <- "Yes"
Coefficient <- c(m2$coefficients[[2]], m2$coefficients[[3]], m2$coefficients[[4]])
SE <- c(summary(m2)$coefficients[,2][[2]], summary(m2)$coefficients[,2][[3]], summary(m2)$coefficients[,2][[4]])

model1Frame <- data.frame(Treatment, Nonwhite, Coefficient, SE)

# White
Treatment <- c("Anger", "Political Anger", "Political Salience")
Nonwhite <- "No"
Coefficient <- c(m1$coefficients[[2]], m1$coefficients[[3]], m1$coefficients[[4]])
SE <- c(summary(m1)$coefficients[,2][[2]], summary(m1)$coefficients[,2][[3]], summary(m1)$coefficients[,2][[4]])

model2Frame <- data.frame(Treatment, Nonwhite, Coefficient, SE)

interval95 <- -qnorm((1-0.95)/2)

allModelFrame <- data.frame(rbind(model1Frame, model2Frame))

p1 <- ggplot(allModelFrame, aes(shape = Nonwhite))
p1 <- p1 + geom_hline(yintercept = 0, color = gray(1/2), lty = 2)
p1 <- p1 + geom_linerange(aes(x = Treatment, ymin = Coefficient - SE*interval95, ymax = Coefficient + SE*interval95), 
                          lwd = 1, position = position_dodge(width = 1/2))
p1 <- p1 + geom_pointrange(aes(x = Treatment, y = Coefficient, ymin = Coefficient - SE*interval95, ymax = Coefficient + SE*interval95),
                           lwd = 1/2, position = position_dodge(width = 1/2), fill = "BLACK")
p1 <- p1 + coord_flip() + theme_bw()
p1 <- p1 + ylab("Treatment Effect")
p1 <- p1 + scale_color_manual(values = c(1,2))

