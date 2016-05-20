## READ IN DATA ####
source("scripts/rcourse_lesson6_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
# Accuracy data
data_accuracy_stats = data_accuracy_clean

# RT data
data_rt_stats = data_rt_clean

# Check within or between variables for random effects structure
xtabs(~subject_id+congruency+half, data_accuracy_stats)
xtabs(~item+congruency+half, data_accuracy_stats)
xtabs(~item+half, data_accuracy_stats)


## BUILD MODEL FOR ACCURACY ANALYSIS ####
# First model failed to converge
accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency*half|subject_id) +
                         (1+half|item), family = "binomial",
                       data = data_accuracy_stats)

# Model converges with reduction of random effects
accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency|subject_id) +
                         (1|item) +
                         (0+half|item), family = "binomial",
                       data = data_accuracy_stats)

# Summarise model and save
accuracy.glmer_sum = summary(accuracy.glmer)
accuracy.glmer_sum

# Get coefficients and save
accuracy.glmer_coef = coef(accuracy.glmer)
accuracy.glmer_coef


## BUILD MODEL FOR REACTION TIME ANALYSIS ####
# RT analysis (first model converged)
rt_log10.lmer = lmer(rt_log10 ~ congruency * half +
                         (1+congruency*half|subject_id) +
                         (1+half|item),
                       data = data_rt_stats)

# Summarise model and save
rt_log10.lmer_sum = summary(rt_log10.lmer)
rt_log10.lmer_sum

# Get coefficients and save
rt_log10.lmer_coef = coef(rt_log10.lmer)
rt_log10.lmer_coef
