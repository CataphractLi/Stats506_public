# Functional programming with dplyr
# 
# In this script, we write functions that use dplyr to
# compute weighted means.
#
# Data Source:
# https://www.eia.gov/consumption/residential/data/2015/index.php
#   ?view=microdata
#
# Updated: October 20, 2020
# Author: Jialun, Li
# 79: -------------------------------------------------------------------------
 
# run tests at the end?: ------------------------------------------------------
TEST = FALSE

# libraries: ------------------------------------------------------------------
library(tidyverse)

# data: -----------------------------------------------------------------------
url = paste0(
  'https://www.eia.gov/consumption/residential/data/2015/csv/',
  'recs2015_public_v4.csv'
)
local_file = './recs2015_public_v4.csv'

# use local file if it exists, if not use url and save locally
if ( !file.exists(local_file) ) {
  recs = read_delim(url, delim = ',')
  write_delim(recs, path = local_file, delim = ',')
} else {
  recs = read_delim(local_file, delim = ',')
}

# clean up key variables used in this problem: --------------------------------
recs_core = 
  recs %>% 
  transmute( 
    # id variables
    id = DOEID,
    weight = NWEIGHT,
    
    # grouping factor
    region = factor(REGIONC, labels = c('Northeast', 'Midwest', 'South', 'West')),
    
    # case selection
    heat_home = factor(HEATHOME, 0:1, c('No', 'Yes') ),
    
    # temp variables
    temp_home = TEMPHOME, 
    temp_gone = TEMPGONE,
    temp_night = TEMPNITE
  ) %>%
  # Convert negative numbers to missing, for temps. 
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))

# filter cases to those that use space heating in winter: ---------------------

## shows why we want to do this, temps are missing if space heating not used
#recs_core %>% 
#  filter(heat_home == 'No') %>%
#  summarize_all( .funs = function(x) sum(is.na(x)) )

recs_core = filter(recs_core, heat_home == 'Yes')

# point estimates for winter temperatures by region: --------------------------

## manually type out
temps_by_region0 = 
  recs_core %>% 
  group_by(region) %>%
  summarize( 
    avg_temp_home = sum(temp_home * weight) / sum(weight),
    avg_temp_gone = sum(temp_gone * weight) / sum(weight),
    avg_temp_night = sum(temp_night * weight) / sum(weight),
    .groups = 'drop'
  )

## task 1 - replace the repetition above using `across()`
temps_by_region1 = recs_core %>% 
  group_by(region) %>%
  summarize(
    across(starts_with("temp"), 
           function(x) sum(x*weight)/sum(weight)), .groups = "drop"
  )

## task 2 - write a function using the pattern above
recs_mean0 = function(df, vars) {
  # Inputs
  #  df: a (possibly grouped) tibble or data.frame object to be summarized
  #      df must have a variable 'weight' for the weighted sums. 
  #  vars: a character vector of numeric variables in df
  #
  # Outputs: a tibble with one row (per group) as returned by summarize_at

  # <task2>
  df_sum = df %>% 
    group_by(region) %>%
    summarize(
      across({{vars}}, 
             function(x) sum(x*weight)/sum(weight)), .groups = "drop"
    )
  df_sum
}

recs_mean0(recs_core, starts_with("temp"))



# Don't be afraid to do some of the work outside the function
temps_by_region = recs_core %>% 
  group_by(region) %>%
  recs_mean0( vars = c('temp_home', 'temp_gone', 'temp_night') )

## task 3: write a function `add_groups()` to group a data frame
add_groups = function(df, group = NULL) {
  # adds grouping variables to a data.frame and/or tibble
  # Inputs:
  #   df - an object inheriting from the data.frame class, commonly a tibble
  #   group - (optional, defaults to NULL) a character vector of column
  #    names in df to form groups by.

  # <task 3> 
  df <- df %>%
    group_by({{group}})
  df
}

## task 4: write a functional version with groups
recs_mean1 = function(df, vars, group = NULL) {
  # Inputs
  #  df: a (possibly grouped) tibble or data.frame object to be summarized
  #      df must have a variable 'weight' for the weighted sums. 
  #  vars: a character vector of numeric variables in 
  #  group: a character vector with variable names to group by. If 
  #         NULL (the default) retains an group structure of `df` as passed.
  #
  # Outputs: a tibble with one row (per group) as returned by summarize_at

  # call `add_groups()`
  #<task 4a>
  df_groups <- add_groups(df, group)  
  # copy your summarize code from task 2 / `recs_mean0()`
  #<task 4b>
  df_groups <- recs_mean0(df_groups, vars)

  df_groups
}

## Example uses: 
if ( TEST ) {

 recs_mean1(recs_core, vars = c('temp_home', 'temp_gone', 'temp_night') )

 recs_core %>%
   group_by(region) %>%
   recs_mean1( vars = c('temp_home', 'temp_gone', 'temp_night'))

  recs_mean1(recs_core, vars = c('temp_home', 'temp_gone', 'temp_night'),
           group = 'region')


 ## pivot to a longer format
 df = 
  recs_core %>%
  select(id, weight, region, starts_with('temp_') ) %>%
  pivot_longer( 
    cols = starts_with('temp'),
    names_to = 'type',
    names_prefix = 'temp_',
    values_to = 'temp'
  )

 temps_by_type_region =
  df %>%
  #group_by(type, region) %>% recs_mean1( vars = 'temp' )
  recs_mean1( vars = c('temp'), group = c('type', 'region'))
}

