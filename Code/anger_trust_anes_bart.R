## Code for estimating heterogenous treatment effects usng
## Bayesian additive regression trees
setseed(1234)

# set the memory for BART machines
options(java.parameters = "-Xmx5g")

# load the require packages
library(bartMachine) 
library(ICEbox) #(article link: https://arxiv.org/pdf/1309.6392.pdf)

# use the ANES Data
source("Code/anger_trust_anes_data_prep.R")

# build inputs for bartMachine
df2 <- df1 %>% 
  select(c(anger, democrat, lib_con_scale, female, non_white, educ, activism, trust, govt_cares_dv, have_say_dv)) %>% 
  na.omit()
X <- df2 %>% 
  select(c(anger, democrat, lib_con_scale, female, non_white, educ, activism, trust)) %>% 
  data.frame() # predictors
y <- df2 %>% 
  select(govt_cares_dv) %>% 
  pull() # response

# build a BARTmachine
bm1 <- bartMachine(X, y, num_trees = 15, num_burn_in = 500, num_iterations_after_burn_in = 1000)

# view rmse for estimating the appropriate number of trees
rmse_by_num_trees(bm1, tree_list=c(seq(5, 50, by=5)), num_replicates=5) # 15 trees seems optimal

# check for MCMC convergence
plot_convergence_diagnostics(bm1)

# check error assumptions (eg normality)
check_bart_error_assumptions(bm1)

# estimate the pairwise influence of different interaction effects
interaction_investigator(bm1, num_replicates_for_avg=5)

# see the relative impact of different variables
var_selection_by_permute(bm1, num_reps_for_avg=20)


# use ICE to examine individual effects
ice1 <- ice(bm1, X, y, "anger")
plot(ice1, centered = TRUE, color_by = "non_white") # plot for different interactions base on white/nonwhite
plot(ice1, centered = TRUE, color_by = "female") # plot for different interactions base on male/female
plot(ice1, centered = TRUE, color_by = "educ")

png("Documentation/bart_model.png")
plot(ice1, centered = TRUE, color_by = "non_white",
     axes = FALSE,
     ylim = c(0,0.15),
     xlab = "Level of Anger",
     ylab = "Partial yhat (Centered)")
title(" Individual Conditional Expectation Plot \n Anger Predicting Trust, Conditioned on Race", adj = 0)

dev.off()


librarY(pdp)
pred.ice <- function(bm_object){
  out <- cbind(bm_object$X, bm_object$y_hat_train)
  colnames(out) <- c(colnames(bm1$X),"pred")
  return(out)
  }
rm.ice <- partial(bm1, pred.var = "anger", pred.fun = pred.ice)
# Figure 9
plotPartial(rm.ice, rug = TRUE, train = boston, alpha = 0.3)





