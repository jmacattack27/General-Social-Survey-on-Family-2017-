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

# add labels for 5-year age groups
five_year_groupings <- raw_data %>%
  mutate(age_group_5 = case_when(agegr5 == 1 ~ "15 to 19 years",
                                 agegr5 == 2 ~ "20 to 24 years",
                                 agegr5 == 3 ~ "25 to 29 years",
                                 agegr5 == 4 ~ "30 to 34 years",
                                 agegr5 == 5 ~ "35 to 39 years",
                                 agegr5 == 6 ~ "40 to 44 years",
                                 agegr5 == 7 ~ "45 to 49 years",
                                 agegr5 == 8 ~ "50 to 54 years",
                                 agegr5 == 9 ~ "55 to 59 years",
                                 agegr5 == 10 ~ "60 to 64 years",
                                 agegr5 == 11 ~ "65 to 69 years",
                                 agegr5 == 12 ~ "70 to 74 years",
                                 agegr5 == 13 ~ "75 to 79 years",
                                 agegr5 == 14 ~ "80 years and over"))

# Plot for total number of children
five_year_groupings %>%
  ggplot(aes(x = age_group_5)) +
  geom_bar() +
  # scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 50, hjust=1)) +
  labs(title = "Distribution of respondents' ages",
       x = "Five-year Age groups",
       y = "Number of responses")

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

# Plot for household size
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

# Plot for age of first child
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

# Plot for total number of children
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

# Plot for age at first marriage
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

# Plot for age at first birth
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
  add_count(place_birth_province) %>% 
  na.omit()
<<<<<<< HEAD


=======
  

>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051
# Plot for place of birth
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


#### What's next - Jacob? ####

# Data wrangling for marital status
marital_status_data <- gss_data %>%
  select(marital_status) %>% 
  add_count(marital_status) %>% 
  na.omit()

# Plot for marital status
marital_status_data %>%
  ggplot(aes(x = reorder(marital_status, n), fill = marital_status)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        legend.position = "none") +
  labs(title = "Distribution of marital status",
       x = "Marital Status",
       y = "Number of responses") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()

# Data wrangling for education
education_data <- gss_data %>%
<<<<<<< HEAD
  select(education) %>% 
  add_count(education) %>% 
  na.omit()

# Plot for education
education_data %>%
  ggplot(aes(x = reorder(education, n), fill = education)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        legend.position = "none") +
  labs(title = "Distribution of education in the population",
       x = "Education",
       y = "Number of responses") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()
=======
  select(education, age) %>%
  filter(age >= 30) %>% 
  mutate(place_birth_mother == "Born outside Canada" ~ "Both parents born outside Canada",
                                        place_birth_father == "Born outside Canada" | place_birth_mother == "Born outside Canada" ~ "One parent born outside Canada",
                                        place_birth_father == "Born in Canada" & place_birth_mother == "Born in Canada" ~ "Both parents born in Canada",
                                        TRUE ~ "NA") %>% 
  na.omit()
  
# Plot for education
education_data %>%
  ggplot(aes(x = age, fill = education)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Distribution of education in the population",
       x = "Education",
       y = "Number of responses") +
  scale_fill_brewer(palette = "Set1")
>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051

# Data wrangling for income
income_data <- gss_data %>%
<<<<<<< HEAD
  select(income_family, hh_size) %>%
=======
<<<<<<< HEAD
  select(income_family) %>% 
=======
  select(income_family) %>%
>>>>>>> 49d553bfa760231cd02354b8c3ec9697da4d6cd5
  mutate(income_family = factor(income_family, levels = c("Less than $25,000", "$25,000 to $49,999", 
                                                          "$50,000 to $74,999", 
                                                          "$75,000 to $99,999", 
                                                          "$100,000 to $ 124,999", 
<<<<<<< HEAD
                                                          "$125,000 and more")),
         hh_size = as.factor(hh_size)) %>% 
=======
                                                          "$125,000 and more"))) %>% 
>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051
>>>>>>> 49d553bfa760231cd02354b8c3ec9697da4d6cd5
  add_count(income_family) %>% 
  na.omit()

# Plot for income
income_data %>%
<<<<<<< HEAD
  ggplot(aes(x = income_family, fill = hh_size)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
=======
<<<<<<< HEAD
  ggplot(aes(x = reorder(income_family, n), fill = income_family)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
=======
  ggplot(aes(x = income_family, fill = income_family)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051
>>>>>>> 49d553bfa760231cd02354b8c3ec9697da4d6cd5
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Distribution of family income for various household sizes",
       x = "Family Income",
       y = "Number of responses",
       fill = "Household size") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()


# Data wrangling for religion
religion_data <- gss_data %>%
  select(regilion_importance) %>% 
  add_count(regilion_importance) %>% 
  na.omit()

# we can maybe look at religion importance for a subset of the population i.e.
# location like "plot for place of birth"?
# Plot for religion
religion_data %>%
  ggplot(aes(x = reorder(regilion_importance, n), fill = regilion_importance)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        legend.position = "none") +
  labs(title = "Distribution of Religion Importance",
       x = "Religion Importance",
       y = "Number of responses") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()


# Data wrangling for hours worked
hrs_worked_data <- gss_data %>%
  select(average_hours_worked) %>% 
  add_count(average_hours_worked) %>% 
  na.omit()

# we can maybe look at hrs worked for a subset of the population i.e.
# location like "plot for place of birth"?
# Plot for hours worked
hrs_worked_data %>%
<<<<<<< HEAD
  ggplot(aes(x = reorder(average_hours_worked, n), fill = average_hours_worked)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
=======
  ggplot(aes(x = average_hours_worked, fill = average_hours_worked)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        legend.position = "none") +
  labs(title = "Distribution of average number of hours worked per week",
       x = "Average number of hours worked per week",
       y = "Number of responses") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()


# Data wrangling for lang spoken
lang_spoke_data <- gss_data %>%
  select(language_home, province) %>% 
  add_count(language_home) %>% 
  na.omit()

# Plot for lang spoken
lang_spoke_data %>%
  ggplot(aes(x = reorder(province, n), fill = language_home)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Distribution of language spoken in household between provinces",
       x = "Province",
       y = "Number of responses",
       fill = "Language spoken in household") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()


# Data wrangling for mental health
mental_health_data <- gss_data %>%
  select(self_rated_mental_health, age) %>% 
  mutate(self_rated_mental_health = factor(self_rated_mental_health, 
                                           levels = c("Don't know", "Poor", "Fair",
                                                      "Good", "Very good", "Excellent")),
         age_grp = case_when(age < 20 ~ "< 20",
                             age >= 20 & age < 30 ~ "20-30",
                             age >= 30 & age < 40 ~ "30-40",
                             age >= 40 & age < 50 ~ "40-50",
                             age >= 50 & age < 60 ~ "50-60",
                             age >= 60 & age < 70 ~ "60-70",
                             age >= 70 & age < 80 ~ "70-80",
                             age >= 80 ~ ">= 80")) %>% 
  add_count(self_rated_mental_health) %>% 
  na.omit()

# Plot for mental health
mental_health_data %>%
  ggplot(aes(x = self_rated_mental_health, fill = age_grp)) + # https://datavizpyr.com/re-ordering-bars-in-barplot-in-r/
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Distribution of self rated mental health",
       x = "Self rated mental health",
       y = "Number of responses", 
       fill = "Age groups") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()
<<<<<<< HEAD


=======
<<<<<<< HEAD

=======
>>>>>>> 3fe1827e43162f7bf0307dcee9b306f4470ca051
>>>>>>> 49d553bfa760231cd02354b8c3ec9697da4d6cd5
