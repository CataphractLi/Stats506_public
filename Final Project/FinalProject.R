## R Script for the Final Project
##
##
## Author(s): Jialun Li, ljlstudy@umich.edu
## Updated: December 4th, 2020
#! Update the date every time you work on a script. 

#! Limit lines to 79 characters with rare exceptions. 
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# data: -----------------------------------------------------------------------

cbecs_clean <- "cbecs_clean.csv"
if (!file.exists(cbecs_clean)) {
  cbecs <- read_delim("2012_public_use_data_aug2016.csv", delim = ",") %>%
    select(PUBID, CENDIV, PBA, SQFT, WKHRS, WKHRSC, FINALWT)
  write_delim(cbecs, cbecs_clean, delim = ",")
} else {
  cbecs <- read_delim(cbecs_clean, delim = ",")
}

cbecs_rep <- read_delim("2012_public_use_data_aug2016.csv", delim = ",") %>%
  select(PUBID, starts_with("FINALWT")) %>%
  select(-FINALWT)

codebook <- readxl::read_xlsx("2012microdata_codebook.xlsx") %>%
  select(-c("File order", "Variable type", 
            "Len-\r\ngth" , "Format", "Label")) %>%
  rename( "Variable" = `Variable\r\nname`,
          "Format" = `Values/Format codes`) %>%
  filter(Variable %in% c("CENDIV", "PBA", "WKHRSC")) %>%
  mutate(Format = stringr::str_split(Format, pattern = "\\r\\n"))

cbecs <- cbecs %>%
  filter(PBA %in% c("02", "04", "06", "08", "18",  
                    "23", "24", "25", "26" ))


# Analysis: -------------------------------------------------------------------

long_weights <- cbecs_rep %>%
  pivot_longer(
    cols = starts_with('FINALWT'),
    names_to = 'RW',
    names_prefix = 'FINALWT',
    values_to = 'rw'
  ) %>%
  mutate( RW = as.integer(RW))

#! Per Building Analysis

## Mean Working Hours

wh_mean <-  cbecs %>%
  select(CENDIV, PBA, WKHRS, FINALWT) %>%
  mutate(CENDIV = as.factor(CENDIV)) %>%
  group_by(CENDIV, PBA) %>%
  summarise("Working Hours" = sum(WKHRS * FINALWT),
            "Total Weights" = sum(FINALWT), .groups = "drop") %>%
  mutate(Mean = `Working Hours` / `Total Weights`) %>% 
  select(CENDIV, PBA, Mean)

wh_mean_variance <- cbecs %>%
  select(PUBID, CENDIV, PBA, WKHRS) %>%
  mutate(CENDIV = as.factor(CENDIV)) %>%
  left_join(long_weights, by = "PUBID") %>%
  group_by(CENDIV, PBA, RW, WKHRS) %>%
  summarize(RWeights_T = sum(rw), .groups = "drop") %>%
  group_by(CENDIV, PBA, RW) %>%
  summarize(Mean_repl = sum(WKHRS * RWeights_T) / sum(RWeights_T),
            .groups = "drop") %>%
  left_join(wh_mean, by = c("CENDIV", "PBA")) %>%
  group_by(CENDIV, PBA) %>%
  summarise(Variance = mean( {Mean_repl - Mean}^2 / {{1-0.5}^2 }), 
            .groups = 'drop') %>%
  left_join(wh_mean, by = c("CENDIV", "PBA")) %>%
  mutate(
    lwr = Mean - qnorm(0.975) * sqrt(Variance),
    upr = Mean + qnorm(0.975) * sqrt(Variance)
  ) %>%
  select(-Variance)

wh_mean_plot <- wh_mean_variance %>%
  ggplot(aes(x = Mean, y = PBA, 
             color = PBA)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +  
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                      position = position_dodge(width = 0.5),
                      height = 0.5) +
  facet_wrap(~`CENDIV`) +
  theme_bw()

wh_mean_table <- wh_mean_variance %>%
  mutate(
    Mean = 
      sprintf('%4.1f (%4.1f, %4.1f)', Mean, lwr, upr)
  ) %>%
  select(CENDIV, PBA, Mean) %>%
  pivot_wider(
    id_cols = c("PBA"),
    names_from = `CENDIV`,
    values_from = Mean
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 


## Proportion Working Hours

wh_proportion <- cbecs %>%
  select(CENDIV, PBA, WKHRSC, FINALWT) %>%
  mutate(CENDIV = as.factor(CENDIV),
         WKHRSC = as.factor(WKHRSC)) %>%
  group_by(CENDIV, PBA, WKHRSC) %>%
  summarise(Weights = sum(FINALWT), .groups = "drop") %>%
  group_by(CENDIV) %>%
  mutate(Proportions = Weights / sum(Weights)) %>%
  select(-Weights)

wh_proportion_variance <- cbecs %>%
  select(PUBID, CENDIV, PBA, WKHRSC) %>%
  mutate(CENDIV = as.factor(CENDIV),
         WKHRSC = as.factor(WKHRSC)) %>%
  left_join(long_weights, by = "PUBID") %>%
  group_by(CENDIV, PBA, RW, WKHRSC) %>%
  summarize(RWeights_T = sum(rw), .groups = "drop_last") %>%
  mutate(Prop_repl = RWeights_T / sum(RWeights_T)) %>%
  ungroup() %>%
  select(-RWeights_T) %>%
  left_join(wh_proportion, by = c("CENDIV", "PBA", "WKHRSC")) %>%
  group_by(CENDIV, PBA, WKHRSC) %>%
  summarise(Variance = mean( {Prop_repl - Proportions}^2 / {{1-0.5}^2 }), 
            .groups = 'drop') %>%
  left_join(wh_proportion, by = c("CENDIV", "PBA", "WKHRSC")) %>%
  mutate(
    lwr = pmax(Proportions - qnorm(0.975) * sqrt(Variance), 0),
    upr = pmin(Proportions + qnorm(0.975) * sqrt(Variance), 1)
  ) %>%
  select(-Variance)

wh_proportion_plot <- wh_proportion_variance %>%
  ggplot(aes(x = Proportions, y = WKHRSC, 
             color = PBA)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +
  facet_wrap(~`CENDIV`) +
  theme_bw()

wh_proportion_table <- wh_proportion_variance %>%
  mutate(
    Proportions = 
      sprintf('%4.2f (%4.2f, %4.2f)', Proportions, lwr, upr)
  ) %>%
  select(CENDIV, PBA, WKHRSC, Proportions) %>%
  pivot_wider(
    id_cols = c("CENDIV", "PBA"),
    names_from = `WKHRSC`,
    values_from = Proportions
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 


#! Per Square Footage Analysis

## Mean Working Hours

whsq_mean <-  cbecs %>%
  select(CENDIV, PBA, WKHRS, SQFT, FINALWT) %>%
  mutate(CENDIV = as.factor(CENDIV)) %>%
  group_by(CENDIV, PBA) %>%
  summarise("Working Hours" = sum(WKHRS * SQFT * FINALWT),
            "Total Weights" = sum(SQFT * FINALWT), .groups = "drop") %>%
  mutate(Mean = `Working Hours` / `Total Weights`) %>% 
  select(CENDIV, PBA, Mean)

whsq_mean_variance <- cbecs %>%
  select(PUBID, CENDIV, PBA, WKHRS, SQFT) %>%
  mutate(CENDIV = as.factor(CENDIV)) %>%
  left_join(long_weights, by = "PUBID") %>%
  group_by(CENDIV, PBA, RW, WKHRS, SQFT) %>%
  summarize(RWeights_T = sum(rw), .groups = "drop") %>%
  group_by(CENDIV, PBA, RW) %>%
  summarize(Mean_repl = sum(WKHRS * SQFT * RWeights_T) / sum(SQFT * RWeights_T),
            .groups = "drop") %>%
  left_join(whsq_mean, by = c("CENDIV", "PBA")) %>%
  group_by(CENDIV, PBA) %>%
  summarise(Variance = mean( {Mean_repl - Mean}^2 / {{1-0.5}^2 }), 
            .groups = 'drop') %>%
  left_join(whsq_mean, by = c("CENDIV", "PBA")) %>%
  mutate(
    lwr = Mean - qnorm(0.975) * sqrt(Variance),
    upr = Mean + qnorm(0.975) * sqrt(Variance)
  ) %>%
  select(-Variance)

whsq_mean_plot <- whsq_mean_variance %>%
  ggplot(aes(x = Mean, y = PBA, 
             color = PBA)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +  
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  facet_wrap(~`CENDIV`) +
  theme_bw()

whsq_mean_table <- whsq_mean_variance %>%
  mutate(
    Mean = 
      sprintf('%4.1f (%4.1f, %4.1f)', Mean, lwr, upr)
  ) %>%
  select(CENDIV, PBA, Mean) %>%
  pivot_wider(
    id_cols = c("PBA"),
    names_from = `CENDIV`,
    values_from = Mean
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 


## Proportion Working Hours

whsq_proportion <- cbecs %>%
  select(CENDIV, PBA, WKHRSC, SQFT, FINALWT) %>%
  mutate(CENDIV = as.factor(CENDIV),
         WKHRSC = as.factor(WKHRSC)) %>%
  group_by(CENDIV, PBA, WKHRSC) %>%
  summarise(Weights = sum(SQFT * FINALWT), .groups = "drop") %>%
  group_by(CENDIV) %>%
  mutate(Proportions = Weights / sum(Weights)) %>%
  select(-Weights)

whsq_proportion_variance <- cbecs %>%
  select(PUBID, CENDIV, PBA, WKHRSC, SQFT) %>%
  mutate(CENDIV = as.factor(CENDIV),
         WKHRSC = as.factor(WKHRSC)) %>%
  left_join(long_weights, by = "PUBID") %>%
  group_by(CENDIV, PBA, RW, WKHRSC) %>%
  summarize(RWeights_T = sum(SQFT * rw), .groups = "drop_last") %>%
  mutate(Prop_repl = RWeights_T / sum(RWeights_T)) %>%
  ungroup() %>%
  select(-RWeights_T) %>%
  left_join(wh_proportion, by = c("CENDIV", "PBA", "WKHRSC")) %>%
  group_by(CENDIV, PBA, WKHRSC) %>%
  summarise(Variance = mean( {Prop_repl - Proportions}^2 / {{1-0.5}^2 }), 
            .groups = 'drop') %>%
  left_join(wh_proportion, by = c("CENDIV", "PBA", "WKHRSC")) %>%
  mutate(
    lwr = pmax(Proportions - qnorm(0.975) * sqrt(Variance), 0),
    upr = pmin(Proportions + qnorm(0.975) * sqrt(Variance), 1)
  ) %>%
  select(-Variance)

whsq_proportion_plot <- whsq_proportion_variance %>%
  ggplot(aes(x = Proportions, y = WKHRSC, 
             color = PBA)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +
  facet_wrap(~`CENDIV`) +
  theme_bw()

whsq_proportion_table <- whsq_proportion_variance %>%
  mutate(
    Proportions = 
      sprintf('%4.2f (%4.2f, %4.2f)', Proportions, lwr, upr)
  ) %>%
  select(CENDIV, PBA, WKHRSC, Proportions) %>%
  pivot_wider(
    id_cols = c("CENDIV", "PBA"),
    names_from = `WKHRSC`,
    values_from = Proportions
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 

#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------

