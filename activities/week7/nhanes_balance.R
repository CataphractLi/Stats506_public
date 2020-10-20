# Stats 506, F20
# Solution for week 6 activity
# 
# In this script we construct a limited balance table for whether NHANES
#  participatns have a complete dentition examantion. 
#
# Author: James Henderson, (jbhender@umich.edu)
# Updated: Oct 13, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# functions: ------------------------------------------------------------------
pwc = function(n) {
  # convert an integer to a string with a comma as the thousands separator
  # inputs: n - a numeric integer, e.g. 10000
  # outputs: a string, eg. 10,000
  format(n, trim = FALSE, big.mark = ',')
}

pretty_p = function(p, min = 1e-3, fmt = '%05.3f') {
  # format a p-value as a nice string
  ifelse(p < min, 
         sprintf(sprintf('p < %s', fmt), min),
         sprintf(sprintf('p = %s', fmt), p)
  )
}

# data: -----------------------------------------------------------------------
setwd('~/github/Stats506_F20/activities/week6')
demo = read_delim("../../problem_sets/data/nhanes_demo.csv", delim = ',')
ohx = read_delim("../../problem_sets/data/nhanes_ohxden.csv", delim = ',')

demo = demo %>%
  left_join(transmute(ohx, SEQN, OHDDESTS), by = 'SEQN')

# clean up: -------------------------------------------------------------------
demo = demo %>% 
  transmute(
    cohort = SEQN %/% 1e4,
    id = SEQN,
    gender = RIAGENDR,
    age = RIDAGEYR,
    under_20 = ifelse(age < 20, '<20', '20+'),
    exam_status = RIDSTATR,
    education = ifelse(is.na(DMDEDUC2), ifelse(age < 20, 10, NA), DMDEDUC2),
    ohx_status = OHDDESTS
  ) %>%
  filter(exam_status == 2)

# apply factor labels and collapse: -------------------------------------------
demo = demo %>%
  mutate(
    college = ifelse(education %in% 4:5, 
                     'College Graduate/Some College', 
                     'No College/Age <20'),
    gender = c('Male', 'Female')[gender]
  )

# ohx exam status: ------------------------------------------------------------
demo = demo %>%
  mutate(
    complete_ohx = ifelse( !is.na(ohx_status),
                           (exam_status == 2 & ohx_status == 1), FALSE ),
    ohx = ifelse(complete_ohx, 'complete', 'missing')
  ) 

# function to summarize a a category: ----------------------------------------
tab1 = function(df, g_row = 'under_20', g_col = 'ohx') {
  # summarize a data frame by two variables, one in a column and one in rows
  # Inputs: 
  #  df - a data frame
  #  g_row - a length one character vector specifying the row variable
  #  g_col - a length one character vector specifying the column variable
  # Outputs: 
  #   A list with two components:
  #   tab - a data.frame organized as a table
  #     p - a p-value for the chi-squared test of association between the
  #         row and column variables
  
  ## compute the p-value
  tab = df %>%
    group_by(.data[[g_row]], .data[[g_col]]) %>%
    summarize(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = all_of(g_col), values_from = 'n') 
  p = chisq.test(as.matrix(tab[, -1]))$p.value
  
  # format an output table
  tab = demo %>%
    group_by(.data[[g_row]], .data[[g_col]]) %>%
    summarize(n = n(), .groups = 'drop_last') %>%
    mutate( pct = sprintf('%4.1f (%s)', 100 * n / sum(n), pwc(n)) ) %>%
    pivot_wider(
      id_cols = all_of(g_row), 
      names_from = all_of(g_col), 
      values_from = 'pct'
    ) %>%
    ungroup()
  
  tab %>%
    mutate(
      var = {{g_row}},
      pval = c(pretty_p(p), rep('', n() - 1))
    ) %>%
    select(var, level = {{g_row}}, everything())
  
}

# loop over variables to create each sub-table: -------------------------------
row_vars = c('under_20', 'gender', 'college')

tab_list = vector(mode = 'list', length = length(row_vars))
names(tab_list) = row_vars

for ( var in row_vars ) {
  tab_list[[var]] = tab1(demo, g_row = var, g_col = 'ohx')
}

tab1 = bind_rows(tab_list)

# format as an html table: ----------------------------------------------------
tab1_html = tab1 %>%
  select(!var, Group = level, p = pval) %>%
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling("striped", full_width = TRUE) %>%
  kableExtra::add_header_above(
    header = c(' ' = 1, 'Dentition Exam' = 2, ' ' = 1)
  ) %>%
  kableExtra::group_rows("Age Group", start_row = 1, end_row = 2) %>%
  kableExtra::group_rows("Gender", start_row = 3, end_row = 4) %>%
  kableExtra::group_rows("Education", start_row = 5, end_row = 6) 
