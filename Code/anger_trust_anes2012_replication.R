# Load Required Packages
library(dplyr)

# Load in 2012 ANES Data
df_raw <- read.csv("Data/sub-data2012anes.txt", 
                   stringsAsFactors = FALSE, 
                   na.strings = c("-1","-2","-6","-7","-8","-9","90","95","99"))

# relabel variables to have meaningful names
colnames(df_raw) <- c("caseid", "race","dem_pres_anger","rep_pres_anger","party_id","govt_cares","education","gender",
                      "lib_con_scale", "contact_congress","talk_others","wear_campaign","joined_protest",
                      "signed_petition","work_party","contribute_money_party", "contribute_money_cand",
                      "contribute_3rd_party","political_meetings","attend_school_meet","trust_gov")

# recode variables to match Webster 2012 Anes Analysis
df1 <- df_raw %>%
  mutate(
    anger = case_when(
      .$party_id %in% c(1,2,3) ~ .$rep_pres_anger/-5,
      .$party_id %in% c(5,6,7) ~ .$dem_pres_anger/-5,
      TRUE ~ as.double(NA)
    )
  ) %>%
  mutate(
    democrat = case_when(
      .$party_id %in% c(1,2,3) ~ 1,
      .$party_id %in% c(4,5,6,7) ~ 0,
      TRUE ~ as.double(NA)
    )
  ) %>%
  mutate(
    female = case_when(
      .$gender == 2 ~ 1,
      TRUE ~ 0
    )
  )  %>%
  mutate(
    non_white = case_when(
      .$race %in% c(2,3,4) ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    educ = case_when(
      .$education %in% c(1:2) ~ 1/3,
      .$education %in% c(3:4) ~ 2/3,
      .$education %in% c(5) ~ 3/3
    )
  ) %>%
  mutate(
    activism = -(.$political_meetings + .$talk_others + .$wear_campaign + .$work_party +
                   .$contribute_money_party + .$contribute_money_cand + .$contribute_3rd_party +
                   .$joined_protest + .$attend_school_meet + .$contact_congress + .$signed_petition - 22)/11
  ) %>%   
  mutate(
    trust = -(.$trust_gov - 5)/5
  ) %>%
  mutate(govt_cares_dv = -(.$govt_cares -5)) %>%
  mutate(ideology = .$lib_con_scale/7)

m1 <- lm(govt_cares_dv ~ anger + democrat + ideology + female + non_white + educ + activism + trust, data = df1)
summary(m1)

mdl_sub <- subset(df1, select = c("govt_cares_dv", "anger", "democrat", "ideology","female", "non_white", "educ","activism", "trust"))

df1$non_white2 <- factor(df1$non_white, labels = c("White","Non-White"))
m11 <- lm(govt_cares_dv ~ anger*non_white + democrat + ideology + female + educ + activism + trust, data = df1)
summary(m11)

library(interplot)
interplot(m = m11, var1 = "non_white", var2 = "anger", ci = 0.95, adjCI = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  ylab("Additional Effect for \nNon-White Respondents") +
  xlab("How Often Does the Candidate of the \nOpposing Party Make You Angry?") +
  ggtitle("Impact of Anger on Trust for Non-White Repondents \nCompared to White Baseline, 2012 ANES") +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("Never","Some of the Time","About Half of the Time", "Most of the Time", "Always")) 


library(effects)
df1$race <- df1$non_white2
m12 <- lm(govt_cares_dv ~ anger*non_white2 + democrat + lib_con_scale + female + educ + activism + trust, data = df1)
plot(predictorEffect("anger", m12), 
     lines=list(multiline=TRUE, lty = c(1,3), col = c("black","black")), 
     confint=list(style="auto", col = c("#000000", '#646464'), alpha = 0.4),
     main = "Predicted Effect of Anger on Trust \nConditioned on Race",
     xlab = "How Often Does the Candidate of the \nOpposing Party Make You Angry?",
     ylab = "Public Officials Don't Care What People Think",
     lattice = list(key.args = list(x = 0.65, y = 0.99, corner = c(1.5, 1))),
     axes = list(x = list(rug = FALSE))
)


