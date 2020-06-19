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

df1$non_white2 <- factor(df1$non_white, labels = c("White","Non-White"))
m11 <- lm(govt_cares_dv ~ anger*non_white + democrat + lib_con_scale + female + educ + activism + trust, data = df1)
summary(m11)
theme_set(theme_classic())
plot_model(m11, type=c("int"), terms = "anger", colors = c("#000000","#696969"), legend.title = "Race") + 
  ylab("Public Officials Don't Care What People Think")+
  xlab("How Often Does the Candidate of the \nOpposing Party Make You Angry?")+
  ggtitle("Impact of Anger on Trust by Race")+
  theme_bw()+ 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y  = element_text(angle=0, vjust=0.5),
        legend.position = c(0.8, 0.2)) +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                   labels = c("Never","Some of the Time","About Half of the Time", "Most of the Time", "Always")) +
  scale_y_continuous(breaks = c(2,3),
                     labels = c("Disagree \nSomewhat", "Neither Agree \nNor Disagree"), 
                     limits = c(2,3)) 


library(interplot)
interplot(m = m11, var1 = "non_white", var2 = "anger", ci = 0.95, adjCI = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  ylab("Additional Effect for \nNon-White Respondents") +
  xlab("How Often Does the Candidate of the \nOpposing Party Make You Angry?") +
  ggtitle("Impact of Anger on Trust for Non-White Repondents \nCompared to White Baseline, 2016 ANES") +
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



str(predictorEffect("anger", m12, se = TRUE))

preds <- predictorEffect("anger", m12, se = TRUE)

par(mar = c(7,4,3,2))
plot(preds$fit[1:50] ~ preds$x$anger[1:50],
     type = "l",
     lty = 1,
     lwd = 3,
     xlim = c(0.2,1.0),
     ylim = c(1.95,3.05),
     main = "Predicted Effect of Anger on Trust \nConditioned on Race, 2016 ANES",
     xlab = "",
     ylab = "Public Officials Don't Care What People Think",
     xaxt = "n",
     yaxt = "n")
lines(preds$fit[51:100] ~ preds$x$anger[51:100],
      lty = 3,
      lwd = 3)
axis(1, at = c(0.2, 0.4, 0.6, 0.8, 1),
     labels = c("Never","Some of \nthe Time","About Half \nof the Time", "Most of \nthe Time", "Always"),
     padj = 0.5)
axis(2, at = c(2,3),
     labels = c("Disagree \nSomewhat", "Neither Agree \nNor Disagree"))
w_poly.x <- c(preds$x$anger[1:50], rev(preds$x$anger[1:50]))
w_poly.y <- c(preds$lower[1:50], rev(preds$upper[1:50]))
polygon(x=w_poly.x, y=w_poly.y, col=adjustcolor("black",alpha = 0.15), border = NA)
nw_poly.x <- c(preds$x$anger[51:100], rev(preds$x$anger[51:100]))
nw_poly.y <- c(preds$lower[51:100], rev(preds$upper[51:100]))
polygon(x=nw_poly.x, y=nw_poly.y, col=adjustcolor("#646464",alpha = 0.15), border = NA)
mtext("How Often Does the Candidate of the \nOpposing Party Make You Angry?", side = 1, line = 5)
legend("bottomright",lty = c(1,3), lwd = 3, legend = c("White", "Non-White"), bty = "n")

library(modelsummary)
library(gt)

cm <- c('anger' = "Anger at Opposing Party's Candidate",
        'non_white' = 'Non-White',
        'anger:non_white' = 'Anger * Non-White',
        'democrat' = 'Democrat',
        'lib_con_scale' = 'Ideological Self-Placement',
        'female' = 'Female',
        'educ' = 'Education Level',
        'activism' = '11-Point Activism Scale',
        'trust' = 'Pre-Election Trust in Government',
        '(Intercept)' = 'Constant')

msummary(list("Webster Model" = m1, "w/Interaction" = m11),
         title = "OLS Model Comparisons Predicting Distrust in Government Across Three Surveys",
         subtitle = "Dependent Variable: Agreement with Statement 'Public Officials Don't Care What I Think",
         coef_map = cm,
         stars = TRUE,
         gof_omit = "logLik|AIC|BIC") %>%
  gt::tab_spanner(label = '2016 ANES', columns = c("Webster Model","w/Interaction"))
