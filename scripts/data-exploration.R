#### Preamble ####
# Purpose: Explore possible data features
# Author: Kimlin Chin
# Data: 7 March 2022
# Contact: kimlin.chin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("inputs/data/AAae9eTh.csv")
gss_data <- read_csv("inputs/data/gss.csv")

# Data wrangling for household
household_data <- raw_data %>%
  select(hsdsizec, famtype) %>%
  mutate(household_size = case_when(hsdsizec == 6 ~ "6+",
                                    TRUE ~ as.character(hsdsizec)), 
         family_type = case_when(famtype == 1 ~ "Couple",
                                 famtype == 2 ~ "Intact family*",
                                 famtype == 3 ~ "Step-family with common child",
                                 famtype == 4 ~ "Step-family without a common child",
                                 famtype == 5 ~ "Single-parent family",
                                 famtype == 6 ~ "No spouse/partner or children in the
household"))

# PLot for household size
household_data %>%
  ggplot(aes(x = household_size, fill = family_type)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 8000, by = 1000)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(title = "Distribution of household size",
       x = "Household size",
       y = "Number of responses",
       fill = "Family Type",
       caption = "*Intact family = family in which both biological parents are present in the home.")

# PLot for age of first child
gss_data %>%
  select(age_first_child) %>%
  ggplot(aes(x = age_first_child)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(title = "Distribution of age of first child",
       x = "Age of first child",
       y = "Number of responses")

# Data wrangling for total number of children
gss_data <- gss_data %>%
  mutate(total_children1 = case_when(total_children == 7 ~ "7+",
                                     TRUE ~ as.character(total_children)
                                     ))

# PLot for total number of children
gss_data %>%
  select(total_children1) %>%
  ggplot(aes(x = total_children1)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(title = "Distribution of total number of children per family",
       x = "Number of children",
       y = "Number of responses")

# PLot for age at first marriage
gss_data %>%
  select(age_at_first_marriage) %>%
  ggplot(aes(x = age_at_first_marriage)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(title = "Distribution of age at first marriage",
       x = "Age at first marriage",
       y = "Number of responses")

# PLot for age at first birth
gss_data %>%
  select(age_at_first_birth) %>%
  ggplot(aes(x = age_at_first_birth)) +
  geom_histogram() +
  # scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  # scale_y_continuous(breaks = seq(0, 160, by = 10)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  labs(title = "Distribution of age at first birth",
       x = "Age at first birth",
       y = "Number of responses")

# Data wrangling for place of birth
birth_place_data <- gss_data %>%
  select(place_birth_canada, place_birth_father, place_birth_mother, place_birth_province) %>%
  mutate(place_birth_parent = case_when(place_birth_father == "Born outside Canada" & place_birth_mother == "Born outside Canada" ~ "Both parents born outside Canada",
                                        place_birth_father == "Born outside Canada" | place_birth_mother == "Born outside Canada" ~ "One parent born outside Canada",
                                        place_birth_father == "Born in Canada" & place_birth_mother == "Born in Canada" ~ "Both parents born in Canada",
                                        TRUE ~ "NA"),
         place_birth_province = case_when(place_birth_canada == "Born outside Canada" ~ "Outside Canada",
                                          TRUE ~ place_birth_province)
         ) %>%
  mutate(place_birth_parent = factor(place_birth_parent, levels = c("Both parents born in Canada", "One parent born outside Canada", "Both parents born outside Canada", "NA"))) %>%
  add_count(place_birth_province)
  

# PLot for place of birth
birth_place_data %>%
  ggplot(aes(x = reorder(place_birth_province, n), fill = place_birth_parent)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, by = 500)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Distribution of place of birth",
       x = "Place of birth",
       y = "Number of responses",
       fill = "Birth place of parents") +
  coord_flip()


#### What's next? ####