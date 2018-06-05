# Code for replication OLS and Ord Logit Models in Webster

# load the data prep file
source("Code/anger_trust_anes_data_prep.R")

## replicate the Table 1 linear model for have say in govt
m1 <- lm(govt_cares_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, data = df1)
summary(m1)

# whites only
m3 <- lm(govt_cares_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, 
         data = subset(df1, non_white == 0))
summary(m3)

# non-whites
m4 <- lm(govt_cares_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, 
         data = subset(df1, non_white == 1))
summary(m4)

## Replicate Table 1 Linear Model for govt cares
m2 <- lm(have_say_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, data = df1)
summary(m2)

# whites only
m5 <- lm(have_say_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, 
         data = subset(df1, non_white == 0))
summary(m5)

# non-whites
m6 <- lm(have_say_dv ~ anger + democrat + lib_con_scale + female + non_white + educ + activism + trust, 
         data = subset(df1, non_white == 1))
summary(m6)

# testing for effect of education
# Some college +
m7 <- lm(govt_cares_dv ~ anger + democrat + educ + trust + lib_con_scale + female + non_white + activism , 
         data = subset(df1, educ > 1/3))
summary(m7)

# HS or less
m8 <- lm(govt_cares_dv ~ anger + democrat + educ + trust + lib_con_scale + female + non_white + activism , 
         data = subset(df1, educ == 1/3))
summary(m8)

# Some college +
m9 <- lm(have_say_dv ~ anger + democrat + educ + trust + lib_con_scale + female + non_white + activism , 
         data = subset(df1, educ > 1/3))
summary(m9)

# HS or less
m10 <- lm(have_say_dv ~ anger + democrat + educ + trust + lib_con_scale + female + non_white + activism , 
          data = subset(df1, educ == 1/3))
summary(m10)


