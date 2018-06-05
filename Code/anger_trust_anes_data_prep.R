##############
#  Data Prep R code for replicating 
#  "Anger and Declining Trust in Government in the
#  American Electorate" by Steven Webster#  
##############

# Load Required Packages
library(dplyr)

# Load in 2016 ANES Data
df_raw <- read.csv("Data/2016ANES_Webster_subset.csv", 
                stringsAsFactors = FALSE, 
                na.strings = c("-1","-2","-6","-7","-8","-9","90","95","99"))

# relabel variables to have meaningful names
colnames(df_raw) <- c("respondent_id", "dem_pres_anger","rep_pres_anger","obama_anger","party_id","lib_con_scale",
                      "gender","white","education","trust_gov","political_meetings","talk_others","wear_campaign",
                      "work_party","contribute_money_cand","contribute_money_party","contribute_3rd_party","joined_protest",
                      "attend_school_meet","signed_petition","contact_congress","have_say","govt_cares")

# recode variables to match Webster 2012 Anes Analysis
df1 <- df_raw %>%
  mutate(
    anger = case_when(
      .$party_id %in% c(1,2,3) ~ .$rep_pres_anger/5,
      .$party_id %in% c(5,6,7) ~ .$dem_pres_anger/5,
      TRUE ~ as.double(NA)
    )
  ) %>%
  mutate(
    democrat = case_when(
      .$party_id %in% c(1,2,3) ~ 1,
      .$party_id %in% c(5,6,7) ~ 0,
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
      .$white == 1 ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  mutate(
    educ = case_when(
      .$education %in% c(1:9) ~ 1/3,
      .$education %in% c(10:13) ~ 2/3,
      .$education %in% c(14:16) ~ 3/3
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
  mutate(have_say_dv = -(.$have_say - 5)) %>%
  mutate(ideology = .$lib_con_scale/7)


  
  
  
  
  
  
  
  
  
  
  



