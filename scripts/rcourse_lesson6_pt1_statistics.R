## READ IN DATA ####
source("scripts/rcourse_lesson6_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
# Accuracy data
data_accuracy_stats = data_accuracy_clean

# RT data
data_rt_stats = data_rt_clean

# Check within or between variables
xtabs(~subject_id+congruency+half, data_accuracy_stats)
xtabs(~item+congruency+half, data_accuracy_stats)
xtabs(~item+half, data_accuracy_stats)


## BUILD MODELS ####
# Accuracy analysis
# First models fail to converge
accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency*half|subject_id) +
                         (1+half|item), family = "binomial",
                       data = data_accuracy_stats)

accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency+half|subject_id) +
                         (1+half|item), family = "binomial",
                       data = data_accuracy_stats)

accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency|subject_id) +
                         (1+half|item), family = "binomial",
                       data = data_accuracy_stats)

accuracy.glmer = glmer(accuracy ~ congruency * half +
                         (1+congruency|subject_id) +
                         (1|item), family = "binomial",
                       data = data_accuracy_stats)

accuracy.glmer_sum = summary(accuracy.glmer)
accuracy.glmer_sum

accuracy.glmer_coef = coef(accuracy.glmer)
accuracy.glmer_coef

# RT analysis
rt_log10.lmer = lmer(rt_log10 ~ congruency * half +
                         (1+congruency*half|subject_id) +
                         (1+half|item),
                       data = data_rt_stats)

rt_log10.lmer_sum = summary(rt_log10.lmer)
rt_log10.lmer_sum

rt_log10.lmer_coef = coef(rt_log10.lmer)
rt_log10.lmer_coef
