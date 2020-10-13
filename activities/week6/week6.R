# Week 6 Activity

library(tidyverse)

nhanes_demo <- read_delim("nhanes_demo.csv", delim = ",")
nhanes_ohxden <- read_delim("nhanes_ohxden.csv", delim = ",")

nhanes_whole <- nhanes_demo %>%
                  left_join(select(nhanes_ohxden, SEQN, OHDDESTS), by = "SEQN")

nhanes_clean <- nhanes_whole %>%
  transmute(
    id = SEQN,
    gender = RIAGENDR,
    age = RIDAGEYR,
    under_20 = as.numeric(RIDAGEYR < 20),
    college = ifelse(is.na(DMDEDUC2), 0, 
                     ifelse(DMDEDUC2 == 4 | DMDEDUC2 == 5, 1, 0)),
    exam_status = ifelse(is.na(RIDSTATR), 0, RIDSTATR),
    ohx_status = ifelse(is.na(OHDDESTS), 0, OHDDESTS),
  ) %>%
  mutate(
    ohx = ifelse(is.na(exam_status) | is.na(ohx_status), "incomplete",
      ifelse(exam_status == 2 & ohx_status == 1, "complete", "incomplete"))
  ) %>%
  filter(exam_status == 2)


# columns - urban
# rows - type, division

table_nhanes <- 
  nhanes_clean %>%
  select(under_20, gender, college, ohx_status) %>%
  mutate(n = count(under_20, gender, college)) %>%
  pivot_wider(
    id_cols = c('under_20', 'gender', 'college'),
    names_from = ohx_status,
    values_from = OHX
  )

  