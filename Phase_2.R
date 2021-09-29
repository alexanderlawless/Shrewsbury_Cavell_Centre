## Phase 2
library(tidyverse)
library(readxl)
library(janitor)

# Population prediction
ons_pop_male <-
  read_excel("ons_population_predictions_2018.xls", 
             sheet = "Males", skip = 6) %>% 
  clean_names() %>% 
  filter(code == "E38000147") %>% 
  select(-code, -area) %>% 
  pivot_longer(cols = -age_group, 
               names_to = "year",
               values_to = "pop") %>% 
  mutate(pop = pop*0.128) %>% 
  pivot_wider(id_cols = age_group, 
              names_from = "year",
              values_from = "pop")

ons_pop_female <-
  read_excel("ons_population_predictions_2018.xls", 
             sheet = "Females", skip = 6) %>% 
  clean_names() %>% 
  filter(code == "E38000147") %>% 
  select(-code, -area) %>% 
  pivot_longer(cols = -age_group, 
               names_to = "year",
               values_to = "pop") %>% 
  mutate(pop = pop*0.128) %>% 
  pivot_wider(id_cols = age_group, 
              names_from = "year",
              values_from = "pop")

practice_pop <-
  b_practice_pop_pyramid %>% 
  group_by(age) %>% 
  summarise(male = sum(male_patient_count),
            female = sum(female_patient_count))

practice_pop_ranges <-
  practice_pop %>% 
  drop_na(age) %>% 
  mutate(age_group = 
           case_when(
             age < 5 ~ "0-4",
             age >= 5 & age < 10 ~ "5-9",
             age >= 10 & age < 15 ~ "10-14",
             age >= 15 & age < 20 ~ "15-19",
             age >= 20 & age < 25 ~ "20-24",
             age >= 25 & age < 30 ~ "25-29",
             age >= 30 & age < 35 ~ "30-34",
             age >= 35 & age < 40 ~ "35-39",
             age >= 40 & age < 45 ~ "40-44",
             age >= 45 & age < 50 ~ "45-49",
             age >= 50 & age < 55 ~ "50-54",
             age >= 55 & age < 60 ~ "55-59",
             age >= 60 & age < 65 ~ "60-64",
             age >= 65 & age < 70 ~ "65-69",
             age >= 70 & age < 75 ~ "70-74",
             age >= 75 & age < 80 ~ "75-79",
             age >= 80 & age < 85 ~ "80-84",
             age >= 85 & age < 90 ~ "85-89",
             age >= 90 ~ "90+"
             )) %>% 
  group_by(age_group) %>% 
  summarise(male = sum(male),
            female = sum(female)) %>% 
  mutate(age_group = 
           factor(
             age_group,
             levels = c(
               "0-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80-84",
               "85-89",
               "90+"
               ))) %>% 
  arrange(age_group)

practice_pop_ranges %>% 
  select(-female) %>% 
  left_join(ons_pop_male, by = c("age_group"))

practice_pop_ranges %>% 
  select(-male) %>% 
  left_join(ons_pop_female, by = c("age_group"))


# Visualise population projections (male and female projections alongside each other)
(
ons_pop_male %>% 
  filter(age_group != "All ages") %>% 
  mutate(age_range = 
           case_when(
             age_group %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
             age_group %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
             age_group %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
             age_group %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
             age_group %in% c("80-84", "85-89", "90+") ~ "80+"
           )) %>%
  pivot_longer(cols = c(-age_group, -age_range)) %>% 
  group_by(age_range, name) %>% 
  summarise(value = sum(value)) %>% 
  mutate(name = as.numeric(substr(name,2,5))) %>% 
  
  ggplot(aes(x=name, y = value, colour = age_range)) +
  geom_smooth() +
  scale_color_SU() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Year",
       y = "Population",
       colour = "Age group",
       title = "Male")
) +
  (
  ons_pop_female %>% 
    filter(age_group != "All ages") %>% 
    mutate(age_range = 
           case_when(
             age_group %in% c("0-4", "5-9", "10-14", "15-19") ~ "0-19",
             age_group %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
             age_group %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
             age_group %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
             age_group %in% c("80-84", "85-89", "90+") ~ "80+"
           )) %>%
    pivot_longer(cols = c(-age_group, -age_range)) %>% 
    group_by(age_range, name) %>% 
    summarise(value = sum(value)) %>% 
    mutate(name = as.numeric(substr(name,2,5))) %>% 
    
    ggplot(aes(x=name, y = value, colour = age_range)) +
    geom_smooth() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_SU() +
    labs(x = "Year",
       y = "",
       colour = "Age group",
       title = "Female")
  ) + 
  plot_annotation(
    title = "Population projected to increase sharply in the over 60's",
    subtitle = "Predicted poplation change in Shrewsbury Health and Wellbeing Hub patient population, 2018-2043")


