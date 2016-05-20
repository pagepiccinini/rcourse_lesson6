## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA
# Read in full results
data_results = list.files(path = "data/results", full.names = T) %>%
  # Run read.table call on all files
  map(read.table, header = T, sep="\t") %>%
  # Combine all data frames into a single data frame by row
  reduce(rbind)

# Read in extra data about specific subjects
data_subjects = read.table("data/rcourse_lesson6_data_subjects.txt", header=T, sep="\t")

# Read in extra data about specific items
data_items = read.table("data/rcourse_lesson6_data_items.txt", header=T, sep="\t")


## CLEAN DATA ####
# Fix and update columns for results data, combine with other data
data_clean = data_results %>%
  # Rename columns with unclear names
  rename(trial_number = SimpleRTBLock.TrialNr.) %>%
  rename(congruency = Congruency) %>%
  rename(correct_response = StroopItem.CRESP.) %>%
  rename(given_response = StroopItem.RESP.) %>%
  rename(accuracy = StroopItem.ACC.) %>%
  rename(rt = StroopItem.RT.) %>%
  # Keep only specific columns in data frame
  select(subject_id, block, item, trial_number, congruency, correct_response,
         given_response, accuracy, rt) %>%
  # Combine results with subject specific information
  inner_join(data_subjects) %>%
  # Combine results with items specific information
  inner_join(data_items) %>%
  # Make a new column for experiment half
  mutate(half = ifelse(block == "one" | block == "two", "first", "second"))

# Get RT outlier information
data_rt_sum = data_clean %>%
  # Group data by subject and each experimental variable to be investigated
  group_by(subject_id, congruency, half) %>%
  # Get mean and standard deviation of reaction times
  summarise(rt_mean = mean(rt),
            rt_sd = sd(rt)) %>%
  ungroup() %>%
  # Calculate reaction times 2 standard devations above and below the mean
  mutate(rt_high = rt_mean + (2 * rt_sd)) %>%
  mutate(rt_low = rt_mean - (2 * rt_sd))

# Remove data points with slow RTs for accuracy data
data_accuracy_clean = data_clean %>%
  # Combine data with reaction time summary information
  inner_join(data_rt_sum) %>%
  # Remove high and low outliers
  filter(rt = rt < rt_high) %>%
  filter(rt = rt > rt_low)

# Remove data points with incorrect response for RT data
data_rt_clean = data_accuracy_clean %>%
  # Remove any incorrect responses
  filter(accuracy == "1") %>%
  # Make a column to log 10 transform the reaction times
  mutate(rt_log10 = log10(rt))





