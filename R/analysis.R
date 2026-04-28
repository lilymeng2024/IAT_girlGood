library(dplyr)
library(jsonlite)
df <- read.csv("C:/Users/25688/OneDrive - University of Manitoba/桌面/IAT_girlGood./data/data.csv")

#Keep only blocks 3,4,6,7
df <- df %>% filter(block %in% c(3,4,6,7))

#--------------Apply exclusion criteria-----------------#

#1. Remove trials > 10,000 ms
df <- df %>% filter(rt <= 10000) 

# 2. Remove subjects with >10% trials < 300 ms
fast_trials <- df %>%
  group_by(subject) %>%
  summarise(prop_fast = mean(rt < 300, na.rm = TRUE), .groups = "drop")

valid_subjects <- fast_trials %>%
  filter(prop_fast <= 0.10) %>%
  pull(subject)

df <- df %>% filter(subject %in% valid_subjects)

#3. remnove trials < 400 ms
df <- df %>% filter(rt >= 400)

#---------------------------------------------------------#
#mean rt of correct trials for each block
block_means <- df %>%
  filter(correct == TRUE) %>%
  group_by(subject, block) %>%
  summarise(mean_rt = mean(rt), .groups = "drop")

#pooled SD
#3&6
sd_36 <- df %>%
  filter(block %in% c(3,6), correct == TRUE) %>%
  group_by(subject) %>%
  summarise(sd_36 = sd(rt), .groups = "drop")
#4&7
sd_47 <- df %>%
  filter(block %in% c(4,7), correct == TRUE) %>%
  group_by(subject) %>%
  summarise(sd_47 = sd(rt), .groups = "drop")

#replace incorrect trials
#join block means first
df <- df %>%
  left_join(block_means, by = c("subject", "block"))

df <- df %>%
  mutate(rt_adj = ifelse(correct == FALSE, mean_rt + 600, rt))

#-------------D-score-------------------------#
#---------------------------------------------#

#compute RT mean after replacement
block_avg <- df %>%
  group_by(subject, block) %>%
  summarise(mean_rt_adj = mean(rt_adj), .groups = "drop")

# compute differences
#reshape wide
block_wide <- block_avg %>%
  tidyr::pivot_wider(names_from = block, values_from = mean_rt_adj, names_prefix = "block_")

block_wide <- block_wide %>%
  mutate(
    diff_36 = block_6 - block_3,
    diff_47 = block_7 - block_4
  )

#divided by pooled SD
#join SDs
block_wide <- block_wide %>%
  left_join(sd_36, by = "subject") %>%
  left_join(sd_47, by = "subject")
#compute quotients
block_wide <- block_wide %>%
  mutate(
    d1 = diff_36 / sd_36,
    d2 = diff_47 / sd_47
  )

#Final D-score
block_wide <- block_wide %>%
  mutate(D_score = (d1 + d2) / 2)

#Final output
final_scores <- block_wide %>%
  select(participant_ID, D_score)

head(final_scores)


