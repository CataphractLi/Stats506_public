## R Script for the Final Project
##
## This R script is used for my final project.
## It basically analyze the 2012 CBECS data and
## answer the following question: For what portion 
## of the day are different types of commercial 
## buildings typically in use? The library I mainly 
## use is "tidyverse"
##
## Author(s): Jialun Li, ljlstudy@umich.edu
## Updated: December 14th, 2020
#! Update the date every time you work on a script. 

#! Limit lines to 79 characters with rare exceptions. 
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# data: -----------------------------------------------------------------------

#! Main data
cbecs <- read_delim("2012_public_use_data_aug2016.csv", delim = ",") %>%
  select(PUBID, CENDIV, PBA, SQFT, ZSQFT,
         WKHRS, ZWKHRS, FINALWT) %>%
  filter(ZWKHRS == 0, ZSQFT == 0) %>% # Delete the imputed observations
  select(-ZWKHRS, -ZSQFT) %>%
  mutate(PBA = as.numeric(PBA))

#! Replicate weights
cbecs_rep <- read_delim("2012_public_use_data_aug2016.csv", delim = ",") %>%
  select(PUBID, starts_with("FINALWT")) %>%
  select(-FINALWT) %>%
  mutate_if(is.numeric, function(x) x + 0.1)

#! Codebook
codebook <- readxl::read_xlsx("2012microdata_codebook.xlsx") %>%
  select(-c("File order", "Variable type", 
            "Len-\r\ngth" , "Format", "Label")) %>%
  rename( "Variable" = `Variable\r\nname`,
          "Format" = `Values/Format codes`) %>%
  filter(Variable %in% c("CENDIV", "PBA")) %>%
  mutate(Format = stringr::str_split(Format, pattern = "\\r\\n"))


#! Only select a subset of the buildings according to PBA
cbecs <- cbecs %>%
  filter(PBA %in% c(2, 4, 6, 8, 18,  
                    23, 24, 25, 26))

#! Assign labels to levels
for (var in c("CENDIV", "PBA")) {
  tibble <- codebook %>%
    filter(Variable == var)
  sep_list <- str_split(tibble$Format[[1]], pattern = " = ")
  levels <- c()
  labels <- c()
  for (j in 1:length(sep_list)) {
      levels <- c(levels, as.numeric(gsub("'", "",sep_list[[j]][1])))
      labels <- c(labels, gsub("'", "",sep_list[[j]][2]))
    }
  cbecs <- cbecs %>%
    mutate_at(var, ~factor(., levels, labels))
}

# Analysis: -------------------------------------------------------------------

#! Mutate the working hours into proportions

cbecs <- cbecs %>%
  mutate(WKHRS = WKHRS / (24*7))

long_weights <- cbecs_rep %>%
  pivot_longer(
    cols = starts_with('FINALWT'),
    names_to = 'RW',
    names_prefix = 'FINALWT',
    values_to = 'rw'
  ) %>%
  mutate(RW = as.integer(RW))

#! Per Building Analysis

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
  select(-Variance) %>%
  rename("Census Division" = CENDIV,
         "Building Activity" = PBA,
         "Proportion of the day" = Mean)

##! Construct plot and table

wh_mean_plot <- wh_mean_variance %>%
  ggplot(aes(x = `Proportion of the day`, y = `Building Activity`, 
             color = `Building Activity`)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +  
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                      position = position_dodge(width = 0.5),
                      height = 0.5) +
  facet_wrap(~`Census Division`) +
  theme_bw()

wh_mean_table <- wh_mean_variance %>%
  mutate(
    `Proportion of the day` = 
      sprintf('%2.2f (%2.2f, %2.2f)', `Proportion of the day`, lwr, upr)
  ) %>%
  select(`Census Division`, `Building Activity`, `Proportion of the day`) %>%
  pivot_wider(
    id_cols = c(`Building Activity`),
    names_from = `Census Division`,
    values_from = `Proportion of the day`
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 


#! Per Square Footage Analysis

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
  select(-Variance) %>%
  rename("Census Division" = CENDIV,
         "Building Activity" = PBA,
         "Proportion of the day" = Mean)

##! Construct plot and table

whsq_mean_plot <- whsq_mean_variance %>%
  ggplot(aes(x = `Proportion of the day`, y = `Building Activity`, 
             color = `Building Activity`)) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +  
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  facet_wrap(~`Census Division`) +
  theme_bw()

whsq_mean_table <- whsq_mean_variance %>%
  mutate(
    `Proportion of the day` = 
      sprintf('%2.2f (%2.2f, %2.2f)', `Proportion of the day`, lwr, upr)
  ) %>%
  select(`Census Division`, `Building Activity`, `Proportion of the day`) %>%
  pivot_wider(
    id_cols = c(`Building Activity`),
    names_from = `Census Division`,
    values_from = `Proportion of the day`
  ) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) 

#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------

