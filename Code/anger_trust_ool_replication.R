# load required packages
library(dplyr)
library(tidyr)
library(clusterSim)

# load in the data
load("C://Users/barne/Desktop/ICPSR_35348/DS0001/35348-0001-Data.rda")

# change factor variables to numeric variables
source("C://Users/barne/Desktop/ICPSR_35348/factor_to_numeric_icpsr.R")
df <- da35348.0001

# preprocess the data
df2 <- df %>%
  mutate(
  # code race/ethnicity
  nonwhite = case_when(
    PPETHM == 1 ~ 0,
    PPETHM %in% c(2,3,4,5) ~ 1,
    TRUE ~ as.double(NA)
  ),
  black = case_when(
    PPETHM == 2 ~ 1,
    PPETHM %in% c(1,3,4,5) ~ 0,
    TRUE ~ as.double(NA)
  ),
  hispanic = case_when(
    PPETHM == 4 ~ 1,
    PPETHM %in% c(1,2,3,5) ~0,
    TRUE ~ as.double(NA)
  ),
  # recode anger to low-high
  angry = case_when(
    W1_B4 == 5 ~ 1,
    W1_B4 == 4 ~ 2,
    W1_B4 == 3 ~ 3, 
    W1_B4 == 2 ~ 4,
    W1_B4 == 1 ~ 5,
    TRUE ~ as.double(NA)
  ),
  democrat = case_when(
    W1_C1 == 2 ~ 1,
    W1_C1 %in% c(1,3,4) ~ 0,
    TRUE ~ as.double(NA)
  ),
  # code trust in government, police, and legal systems to low-high
  trust_gov = case_when(
    W1_K1_A == 1 ~ 4, # Always
    W1_K1_A == 2 ~ 3, # most of the time
    W1_K1_A == 3 ~ 2, # Some of the time
    W1_K1_A == 4 ~ 1, # Never
    TRUE ~ as.double(NA)
  ),
  trust_police = case_when(
    W1_K1_B == 1 ~ 4,
    W1_K1_B == 2 ~ 3,
    W1_K1_B == 3 ~ 2,
    W1_K1_B == 4 ~ 1,
    TRUE ~ as.double(NA)
  ),
  trust_legal = case_when(
    W1_K1_C == 1 ~ 4,
    W1_K1_C == 2 ~ 3,
    W1_K1_C == 3 ~ 2,
    W1_K1_C == 4 ~ 1,
    TRUE ~ as.double(NA)
  ),
  # code traditional trust measures to low-high
  govt_cares = case_when(
    W1_B1 == 1 ~ 5,
    W1_B1 == 2 ~ 4,
    W1_B1 == 3 ~ 3,
    W1_B1 == 4 ~ 2,
    W1_B1 == 5 ~ 1,
    TRUE ~ as.double(NA)
  ),
  have_say = case_when(
    W1_B2 == 1 ~ 5,
    W1_B2 == 2 ~ 4,
    W1_B2 == 3 ~ 3,
    W1_B2 == 4 ~ 2,
    W1_B2 == 5 ~ 1,
    TRUE ~ as.double(NA)
  ),
  education = case_when(
    as.numeric(PPEDUC) %in% c(1:9) ~ 1,
    as.numeric(PPEDUC) %in% c(10:12) ~ 2,
    as.numeric(PPEDUC) %in% c(13:14) ~ 3,
    TRUE ~ as.double(NA)
  ),
  female = case_when(
    PPGENDER == 2 ~ 1,
    PPGENDER == 1 ~ 0,
    TRUE ~ as.double(NA)
  ),
  ideology = as.numeric(W1_C2),
  # recode activism activities to 0, 1
  act1 = recode(W1_L4_A, "1" = 1, "2" = 0),
  act2 = recode(W1_L4_B, "1" = 1, "2" = 0),
  act3 = recode(W1_L4_C, "1" = 1, "2" = 0),
  act4 = recode(W1_L4_D, "1" = 1, "2" = 0),
  act5 = recode(W1_L5_A, "1" = 1, "2" = 0),
  act6 = recode(W1_L5_B, "1" = 1, "2" = 0),
  act7 = recode(W1_L5_C, "1" = 1, "2" = 0),
  act8 = recode(W1_L5_D, "1" = 1, "2" = 0),
  act9 = recode(W1_L5_E, "1" = 1, "2" = 0),
  act10 = recode(W1_L5_F, "1" = 1, "2" = 0)
  ) %>%
  dplyr::select(nonwhite, black, hispanic, angry, ideology, democrat, trust_gov, trust_police, trust_legal, govt_cares,
         have_say, education, female, act1:act10) %>%
  mutate(activism = act1 + act2 + act3 + act4 + act5 + act6 + act7 + act8 + act9 + act10)

# scale data for regression
scale01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm =T))}
scaled_df <- df2 %>% mutate_all(scale01)

# replicate Webster table 1
summary(lm(trust_gov ~ angry + democrat + ideology + female + nonwhite + education + activism, data = scaled_df))

# check for interaction with race
# surprisingly, this is positive. The survey as taken in 2012 and has may black respondents
# maybe trust is more regime oriented after all?
summary(lm(trust_gov ~ angry*nonwhite + democrat + ideology + female + education + activism, data = scaled_df))

summary(lm(trust_gov ~ angry + black + hispanic + democrat + ideology + female + education + activism, data = scaled_df))

# this is a fuller model, I think
# having measures for trust in the police and legal system better controls for 
# trust in the system vs trust in the regime
# I think this shows that nonwhites have an additional impact for anger even controlling for party
# maybe this means that the relationship between anger and trust is mediated by racial investment
# in the regime, thus, anger -> more distrust in 2016 but more trust in 2012
# like if you're black and angry in 2012, you're not going to take it out on Obama
# you're more likely to look to Obama (or the Democrats) to fix the problem
# since they are the ones who are willing and able to make changes
summary(lm(trust_gov ~ angry*nonwhite + democrat + ideology + female + education + activism + trust_police + trust_legal, data = scaled_df))

# cross tabls comparing white/non-white on trust measures
# In this data set, non-whites have \emph{higher} trust in gov
# which I think is due to Obama
prop.table(table(df2$nonwhite, df2$trust_gov),margin = 1)
prop.table(table(df2$nonwhite, df2$trust_police),margin = 1)
prop.table(table(df2$nonwhite, df2$trust_legal),margin = 1)

library(interplot)
m1 <- lm(trust_gov ~ angry*nonwhite + democrat + ideology + female + education + activism, data = scaled_df)
interplot(m = m1, var1 = "nonwhite", var2 = "angry", ci = 0.95, adjCI = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  ylab("Additional Effect for \nNon-White Respondents") +
  xlab("How Angry Do You Feel About the Way Things are \nGoing in the Country These Days?") +
  ggtitle("Impact of Anger on Trust for Non-White Repondents \nCompared to White Baseline, 2012 OOL") +
  scale_x_continuous(breaks = c(0,0.25,5,0.75,1),
                     labels = c("Not At All","A Little","Somewhat", "Very", "Extremely")) 

library(effects)
scaled_df$nonwhite2 <- factor(scaled_df$nonwhite, labels = c("White", "Nonwhite"))
m2 <- lm(trust_gov ~ angry*nonwhite2 + democrat + ideology + female + education + activism, data = scaled_df)
preds <- predictorEffect("angry", m2, se = TRUE)

par(mar = c(5,5,3,2))
plot(preds$fit[1:50] ~ preds$x$angry[1:50],
     type = "l",
     lty = 1,
     lwd = 3,
     xlim = c(0,1.0),
     ylim = c(00,0.67),
     main = "Predicted Effect of Anger on Trust \nConditioned on Race, 2012 OOL",
     xlab = "",
     ylab = "How Much Can You Trust the \nGovernment in Washington",
     xaxt = "n",
     yaxt = "n")
lines(preds$fit[51:100] ~ preds$x$angry[51:100],
      lty = 3,
      lwd = 3)
axis(1, at = c(0,0.25,0.5,0.75,1),
     labels = c("Not At All","A Little","Somewhat", "Very", "Extremely"))
axis(2, at = c(1,2/3,1/3,0),
     labels = c("Always","Most of the Time","Some of the Time","Never"))
w_poly.x <- c(preds$x$angry[1:50], rev(preds$x$angry[1:50]))
w_poly.y <- c(preds$lower[1:50], rev(preds$upper[1:50]))
polygon(x=w_poly.x, y=w_poly.y, col=adjustcolor("black",alpha = 0.15), border = NA)
nw_poly.x <- c(preds$x$angry[51:100], rev(preds$x$angry[51:100]))
nw_poly.y <- c(preds$lower[51:100], rev(preds$upper[51:100]))
polygon(x=nw_poly.x, y=nw_poly.y, col=adjustcolor("#646464",alpha = 0.15), border = NA)
mtext("How Angry Do You Feel About the Way Things are \nGoing in the Country These Days?", side = 1, line = 3)
legend("bottomleft",lty = c(1,3), lwd = 3, legend = c("White", "Non-White"), bty = "n")






