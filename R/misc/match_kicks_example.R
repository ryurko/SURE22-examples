# PURPOSE: Demonstrate the use of matching with MatchIt

# Load NFL FG data --------------------------------------------------------

library(tidyverse)
nfl_fg_attempts <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/glm_examples/nfl_fg_attempt_data.csv")
nfl_fg_attempts

# I'm going to add a column to make my fake treatment variable that is not 
# randomly assigned: I randomly decide to ask Thor to help the kicker make the 
# field goal ONLY when the kick distance is less than or equal (<=) to 25 yards, 
# i.e., I'm going to just call a sample of kicks that are less than 25 yards to
# be "treated", while all other kicks are "controls". 

# THIS IS CLEARLY NOT RANDOM ASSIGNMENT! Thus making it useful to demonstrate matching...

set.seed(2022)
nfl_fg_attempts <- nfl_fg_attempts %>%
  # create my fake ask_thor variable (it's the product of indicator for kick
  # less than or equal to 25 with randomly assigned 0/1 to multiply by when I ask Thor...)
  mutate(ask_thor = as.numeric(kick_distance <= 25) * 
           sample(0:1, n(), replace = TRUE, prob = c(.4, .6)))
table(nfl_fg_attempts$ask_thor)
#   0     1 
# 9819  992 
# Way more controls than treated...

# Fit initial logit model -------------------------------------------------

# Now let's estimate the effect of my treatment of interest: asking Thor to help
# the kicker make the field goal... so I can just go ahead and naively fit a logistic
# regression model to estimate this variable's effect while controlling for the
# kick distance:
init_dumb_logit <- glm(is_fg_made ~ kick_distance + ask_thor, 
                       data = nfl_fg_attempts, family = "binomial")
summary(init_dumb_logit)
# Call:
#   glm(formula = is_fg_made ~ kick_distance + ask_thor, family = "binomial", 
#       data = nfl_fg_attempts)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9810   0.1977   0.4113   0.6299   1.4898  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    5.767229   0.154319  37.372   <2e-16 ***
#   kick_distance -0.101204   0.003438 -29.435   <2e-16 ***
#   ask_thor       0.688139   0.271840   2.531   0.0114 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 9593.1  on 10810  degrees of freedom
# Residual deviance: 8269.7  on 10808  degrees of freedom
# AIC: 8275.7
# 
# Number of Fisher Scoring iterations: 6

# LOOK! Asking Thor to help the kicker works because my p-value for ask_thor
# is < .05, it's a significant relationship!

# THIS IS BULLSHIT!

# My ask_thor variable is nonsense because I clearly based it on the kick distance!
# Thus meaning that kick_distance helped determine the treatment variable in this case,
# resulting in covariate imbalance between my treatment and controls, i.e., my 
# treatment rows were easier kicks - I only asked Thor to help with short kicks!

# The kick distance is a confounding variable in this case...

# Match kicks based on kick_distance --------------------------------------

# I'm now going to use the MatchIt package to perform some type of matching
# procedure to address my confounding problem by kick distance. First you need
# to install the package: (type no when asked about installing from source!)
# install.packages("MatchIt")
library(MatchIt)

# You should read through this demo:
# https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html

# There are a variety of matching approaches one can take, I'm going to start
# with a basic version where I'm going to compute a type of distance based only
# on the kick_distance variable. There are a variety of approaches to take for
# matching, you should read this page to find out more: 
# https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html

# The function you use is called `matchit` which first takes in a formula of the
# form: treatment_variable ~ variables_to_match_on + ....

# For this particular matching, I'm going to just match observations based on 
# the kick_distance using an 'nearest' approach to find controls to match with
# the treatments, using a two to one ratio of controls to treatment:
kick_distance_match <- 
  matchit(ask_thor ~ kick_distance, 
          data = nfl_fg_attempts, method = "nearest",
          distance = "mahalanobis",
          replace = FALSE, # do not reuse controls
          ratio = 2)
# This will be pretty quick - try switching method to "optimal", then it 
# should take a decent chunk of time to run...

# Now we can view the output summarizing the matching:
summary(kick_distance_match)
# Call:
#   matchit(formula = ask_thor ~ kick_distance, data = nfl_fg_attempts, 
#           method = "nearest", distance = "mahalanobis", replace = FALSE, 
#           ratio = 2)
# 
# Summary of Balance for All Data:
#               Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# kick_distance       22.3448       39.1301         -8.8684     0.0402    0.3228   0.9312
# 
# 
# Summary of Balance for Matched Data:
#               Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max Std. Pair Dist.
# kick_distance       22.3448       25.9582         -1.9091     0.3996    0.0695   0.6593          1.9166
# 
# Percent Balance Improvement:
#               Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# kick_distance            78.5       71.5      78.5     29.2
# 
# Sample Sizes:
#   Control Treated
# All          9819     992
# Matched      1984     992
# Unmatched    7835       0
# Discarded       0       0

# Looking at this summary, we can see the change in the mean kick distance
# before and after matching with the 'Summary of Balance' portions.

# From this summary, we can make the Love plot that was discussed by Prof Branson:
# plot(summary(kick_distance_match))
# If you run this you'll only see one point because I was just considering to 
# use a single variable. Check out this vignette with examples of more than
# one variable: https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html


# Re-fit with matched data ------------------------------------------------

# With this matching process complete, I can grab from it a dataset that only
# contains the treatments with the matched controls using:
matched_nfl_fg_attempts <- match.data(kick_distance_match)
table(matched_nfl_fg_attempts$ask_thor)
#    0    1 
# 1984  992 
# Note this now contains far fewer rows than before...
# Now I can refit the model just with this matched data:
init_matched_logit <- glm(is_fg_made ~ kick_distance + ask_thor, 
                          data = matched_nfl_fg_attempts, family = "binomial")
summary(init_matched_logit)
# Call:
#   glm(formula = is_fg_made ~ kick_distance + ask_thor, family = "binomial", 
#       data = matched_nfl_fg_attempts)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.0545   0.1681   0.2154   0.2872   0.3739  
# 
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    8.04879    1.28309   6.273 3.54e-10 ***
#   kick_distance -0.18078    0.04699  -3.847  0.00012 ***
#   ask_thor       0.22232    0.34856   0.638  0.52359    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 806.98  on 2975  degrees of freedom
# Residual deviance: 776.53  on 2973  degrees of freedom
# AIC: 782.53
# 
# Number of Fisher Scoring iterations: 7

# The ask_thor variable is no longer signficant! In fact, its effect size shrunk
# and notably the strength of the kick_distance effect increased! That's because
# we're now primarily only comparing kicks of similar distance to each other
# to decide if ask_thor has any effect. 


# Propensity score matching -----------------------------------------------

# Instead of finding matches based on the kick_distance directly, we can use a 
# model to predict the probability of the treatment as a function of the 
# kick_distance to arrive at propensity scores. We can do this manually
# such as with glm:
thor_logit <- glm(ask_thor ~ kick_distance, data = nfl_fg_attempts,
                  family = "binomial")
# or with gam:
library(mgcv)
thor_logit_gam <- gam(ask_thor ~ s(kick_distance), data = nfl_fg_attempts,
                      family = "binomial")

# We can add the predicted probabilities to the dataset to view:
nfl_fg_attempts <- nfl_fg_attempts %>%
  mutate(glm_score = thor_logit$fitted.values,
         gam_score = thor_logit_gam$fitted.values)

# View the distribution with kick distance by the treatment variable, first
# with the glm version:
nfl_fg_attempts %>%
  ggplot(aes(x = kick_distance, y = glm_score)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ ask_thor, ncol = 1)

# Next with the gam version:
nfl_fg_attempts %>%
  ggplot(aes(x = kick_distance, y = gam_score)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ ask_thor, ncol = 1)

# For both of these, we can see how the highest probabilities are for short 
# kicks - aligning with when I randomly asked Thor to help with the kicks...
# so we could match directly on this propensity score instead of kick distance
# itself! You can imagine, I don't need to only use a single variable for this -
# I could have a classification model based on a variety of features to predict
# the treatment probability. 

# Rather than doing this manually, we could also use the matchit function to
# get the propensity scores for us. For instance, the following code matches
# data based on the GAM propensity score from above:
gam_propensity_match <- 
  matchit(ask_thor ~ s(kick_distance), 
          data = nfl_fg_attempts, method = "nearest",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 2)

# We can visualize the distribution of propensity scores with:
plot(gam_propensity_match, type = "jitter", interactive = FALSE)
# Here we see that the matched treatments and controls have very similar 
# propensity scores, for the most part excluding the clump of matched controls
# with low values.

# We can also view the same summary as before:
summary(gam_propensity_match)

# Call:
#   matchit(formula = ask_thor ~ s(kick_distance), data = nfl_fg_attempts, 
#           method = "nearest", distance = "gam", replace = FALSE, ratio = 2)
# 
# Summary of Balance for All Data:
#               Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# distance             0.5947        0.0409         12.4875     0.0871    0.5774   0.9312
# kick_distance       22.3448       39.1301         -8.8684     0.0402    0.3228   0.9312
# 
# 
# Summary of Balance for Matched Data:
#               Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max Std. Pair Dist.
# distance             0.5947        0.2026          8.8417     0.0249    0.3807   0.6593          8.8417
# kick_distance       22.3448       30.5146         -4.3165     0.0389    0.1571   0.6593          4.7841
# 
# Percent Balance Improvement:
#               Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# distance                 29.2      -51.3      34.1     29.2
# kick_distance            51.3       -1.0      51.3     29.2
# 
# Sample Sizes:
#   Control Treated
# All          9819     992
# Matched      1984     992
# Unmatched    7835       0
# Discarded       0       0

# Notice that it now includes this distance variable - which is simply the
# propensity score (ie predicted probability of ask_thor given kick_distance
# using the gam model). But interestingly, the mean difference for kick distance
# after matching does not appear as improved as before when we just matched on
# kick distance! That's because the ask_thor variable was just constructed based
# on a distance indicator variable with some randomness, so the model for its
# probability may not be that great... nevertheless, we can use it as before.

# As before, we can pull out the matched data:
gam_matched_nfl_fg_attempts <- match.data(gam_propensity_match)
table(gam_matched_nfl_fg_attempts$ask_thor)
#    0    1 
# 1984  992 

# Notice that this dataset contains additional columns: distance, weights, subclass
# But run the following line:
with(gam_matched_nfl_fg_attempts, plot(gam_score, distance))
# What do you notice about the distance column and gam_score?
# The weight column here is just 1, but for other approaches this could be 
# soft between 0 and 1. But as before, we can just refit our logit with this
# new matched data:

gam_matched_logit <- glm(is_fg_made ~ kick_distance + ask_thor, 
                         data = gam_matched_nfl_fg_attempts, family = "binomial")
summary(gam_matched_logit)

# Call:
#   glm(formula = is_fg_made ~ kick_distance + ask_thor, family = "binomial", 
#       data = gam_matched_nfl_fg_attempts)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9983   0.1793   0.2193   0.2949   1.6121  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    6.365074   0.314177   20.26   <2e-16 ***
#   kick_distance -0.120428   0.007708  -15.62   <2e-16 ***
#   ask_thor       0.527091   0.301159    1.75   0.0801 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1594.5  on 2975  degrees of freedom
# Residual deviance: 1204.0  on 2973  degrees of freedom
# AIC: 1210
# 
# Number of Fisher Scoring iterations: 6

# Once again the ask_thor variable is no longer significant - although this
# type of matching did not weaken the fake ask_thor effect as much as before.

# This concludes the matching demo for now, make sure to read through more info
# about the MatchIt package if you're interested! https://kosukeimai.github.io/MatchIt/index.html

