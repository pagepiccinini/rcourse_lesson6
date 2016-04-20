## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA
# Read in results
data_results = list.files(path = "data/results", full.names = T) %>%
  map(read.table, header = T, sep="\t") %>%
  reduce(rbind)

# Read in subject information
data_subjects = read.table("data/rcourse_lesson6_data_subjects.txt", header=T, sep="\t")

# Read in item information
data_items = read.table("data/rcourse_lesson6_data_items.txt", header=T, sep="\t")


## CLEAN DATA ####
# Fix and update columns for results data, combine with other data
data_clean = data_results %>%
  rename(trial_number = SimpleRTBLock.TrialNr.) %>%
  rename(congruency = Congruency) %>%
  rename(correct_response = StroopItem.CRESP.) %>%
  rename(given_response = StroopItem.RESP.) %>%
  rename(accuracy = StroopItem.ACC.) %>%
  rename(rt = StroopItem.RT.) %>%
  select(subject_id, block, item, trial_number, congruency, correct_response,
         given_response, accuracy, rt) %>%
  inner_join(data_subjects) %>%
  inner_join(data_items) %>%
  mutate(half = ifelse(block == "one" | block == "two", "first", "second"))

# Get RT outlier information
data_rt_sum = data_clean %>%
  group_by(subject_id, congruency, half) %>%
  summarise(rt_mean = mean(rt),
            rt_sd = sd(rt)) %>%
  ungroup() %>%
  mutate(rt_high = rt_mean + (2 * rt_sd)) %>%
  mutate(rt_low = rt_mean - (2 * rt_sd))

# Remove data points with slow RTs for accuracy data
data_accuracy_clean = data_clean %>%
  inner_join(data_rt_sum) %>%
  filter(rt = rt < rt_high) %>%
  filter(rt = rt > rt_low)

# Remove data points with incorrect response for RT data
data_rt_clean = data_accuracy_clean %>%
  filter(accuracy == "1") %>%
  # Wait to add until after figure
  mutate(rt_log10 = log10(rt))





