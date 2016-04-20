## READ IN DATA ####
source("scripts/rcourse_lesson6_cleaning.R")


## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## ORGANIZE DATA ####
# Accuracy data
data_accuracy_figs = data_accuracy_clean %>%
  group_by(subject_id, congruency, half) %>%
  summarise(perc_correct = mean(accuracy) * 100) %>%
  ungroup() %>%
  mutate(congruency = factor(congruency, levels = c("con", "incon"),
                             labels = c("congruent", "incongruent")))

# RT data
data_rt_figs = data_rt_clean %>%
  mutate(congruency = factor(congruency, levels = c("con", "incon"),
                             labels = c("congruent", "incongruent")))


## SET COLORS FOR FIGURES ####
cols = brewer.pal(5, "PuOr")
col_con = cols[1]  
col_incon = cols[5]


## MAKE FIGURES ####
# Accuracy figure
accuracy.plot = ggplot(data_accuracy_figs,
                       aes(x = half,
                           y = perc_correct,
                           fill = congruency)) +
  geom_boxplot() +
  ylim(0, 100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c(col_con, col_incon))

pdf("figures/accuracy.pdf")
accuracy.plot
dev.off()

# RT histogram non-transformed
rt_histogram.plot = ggplot(data_rt_figs,
                           aes(x = rt,
                               fill = congruency)) +
  geom_histogram() +
  facet_grid(half ~ congruency) +
  scale_fill_manual(values = c(col_con, col_incon))

pdf("figures/rt_histogram.pdf")
rt_histogram.plot
dev.off()

# RT histogram transformed
rt_log10_histogram.plot = ggplot(data_rt_figs,
                                 aes(x = rt_log10,
                                     fill = congruency)) +
  geom_histogram() +
  facet_grid(half ~ congruency) +
  scale_fill_manual(values = c(col_con, col_incon))

pdf("figures/rt_log10_histogram.pdf")
rt_log10_histogram.plot
dev.off()

# RT boxplot log 10 tranformed
rt_log10_boxplot.plot = ggplot(data_rt_figs, aes(x = half,
                                                 y = rt_log10,
                                                 fill = congruency)) +
  geom_boxplot() +
  scale_fill_manual(values = c(col_con, col_incon))

pdf("figures/rt_log10.pdf")
rt_log10_boxplot.plot
dev.off()
