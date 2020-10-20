# Stats 506, F20
# Problem Set 2
# Solution for Question 1
# 
# This script examines the number of television and the type of each 
# households primary TV by Census Division and rurality for 2009 and 2015
# using the RECS surveys from those  years.
#
# This script begins with data cleaned and output by `1-ps2_q1_prep_data.R`
#
# Author: James Henderson (jbhender@umich.edu)
# Updated: Oct 13, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# data: -----------------------------------------------------------------------
path = './'
file = sprintf('%s/ps2_q1.RData', path)
foo = load(file) # recs09, w09, recs15, w15

# functions: ------------------------------------------------------------------
est_recs = function(df, w, var, 
                    grps = '', id_var = 'id', w_var = 'w',
                    fay = 0.5, conf_level = 0.95) {
      
 # Create weighted estimates and std errors by group using BRR weights
 # Inputs:
 #  - 
 # Outputs: a tibble with one row per group and columns for:
 #   - 
   
 # is grouping requested?
 any_groups = length(setdiff(grps, '')) > 0
 df_grouped = length(groups(df)) > 0
 
 if ( any_groups && df_grouped ) {
    msg = paste0(
       'You passed a grouped data frame to `est_recs()` and set `grps`. ',
       'Using `groups(df)` = %s.'
    )
    grps = paste(groups(df), collapse = ', ')
    msg = sprintf(msg, grps)
    warning(msg)
 }
 
 if ( df_grouped ) {
    grps = groups(ungroup(df)) 
    any_groups = TRUE
 }
 
 # error checking
 stopifnot( length(var) == 1 )
 stopifnot( all(var %in% names(df)) )
 stopifnot( all(c(id_var, w_var) %in% names(df)) )
 stopifnot( all(c(id_var, w_var) %in% names(w)) )
 
 if ( any_groups ) {
   stopifnot( all(grps %in% names(df)) )
 }

 # group df by the variables listed in groups or create artificial group
 #  for temporary use
 if ( any_groups ) {
  df = group_by(df, across({{grps}}))
  stopifnot( all( unlist(groups(df)) == grps ) )
 } else {
   df[['.gid']] = 1
   df = group_by(df, .gid)
   grps = '.gid'
 }
 
 # class of `var`
 # - use weighted mean for numeric or logical classes
 # - weighted grouped sums for factor and character classes
 # - error otherwise
 var_class = class(df[[var]])
 
 # compute point estimates
 if ( var_class %in% c('numeric', 'integer', 'logical') ) {
    est = 
       summarize(
          df,
          across(
           .cols = all_of(var),
           .fns = ~ sum( .x * .data[[w_var]]) / sum(.data[[w_var]]),
          ), 
          .groups = 'drop'
       )
 } else {
    stopifnot( var_class %in% c('factor', 'character'))
    est = 
       group_by(df, .data[[var]], .add = TRUE) %>%
       summarize( p = sum(.data[[w_var]]), .groups = 'drop_last') %>%
       mutate( p = p / sum(p) ) %>%
       ungroup()
 }
 
 # data for replicate estimates
 df_long = left_join(select(ungroup(df), !{{w_var}}), w, by = id_var)    
  
 # group df_long by the variables listed in group and replicate
 grp_w = setdiff(names(w), c(id_var, w_var))
 grp_w = c(grps, grp_w)
 df_long = group_by(df_long, across({{grp_w}}))

 # compute replicate estimates
 if ( var_class %in% c('numeric', 'integer', 'logical') ) {
    repl_est = 
       summarize(
          df_long,
          across(
           .cols = all_of(var),
           .fns = ~ sum(.x * .data[[w_var]]) / sum(.data[[w_var]]),
           .names = "{.col}_r"
          ), 
          .groups = 'drop'
       )
 } else {
    repl_est = 
       group_by(df_long, .data[[var]], .add = TRUE) %>%
       summarize( p_r = sum(.data[[w_var]]), .groups = 'drop_last') %>%
       mutate( p_r = p_r / sum(p_r) ) %>% 
       ungroup()
 }
 
 # estimate std errors
 join_by = intersect(names(est), names(repl_est))
 repl_est = left_join(repl_est, est, by = join_by) %>% 
     group_by(across({{join_by}}))
 
 # function for use in across() to compute std errors using two columns
 se = function(x, col) {
   col_r = paste0(cur_column(),'_r')
   sqrt( mean( {x  - cur_data()[[col_r]]}^2 ) / {1 - fay}^2 )
 }
 
 # "p" and "p_r" are the relevant columns for factor/character "var"
 if ( var_class %in% c('factor', 'character') ) {
    var = "p"
 }
 std_err = 
   repl_est %>%
   summarize(
     across(.cols = all_of(var),
            .fns = ~ se(.x),
            .names = "{.col}_se"
     ), 
     .groups = 'drop'
   ) 
 
 # compute CI's based on std error
 lwr = function(x, a = conf_level) {
   col_se = paste0(cur_column(), '_se')
   m = qnorm({1 - a} / 2)
   x + m * cur_data()[[col_se]]
 }
 
 upr = function(x, col, a = conf_level) {
   col_se = paste0(cur_column(), '_se')
   m = qnorm(1 - {1 - a} / 2)
   x + m * cur_data()[[col_se]]
 }

 out = left_join(est, std_err, by = join_by) %>%
   mutate(     
     across(.cols = all_of(var),
            .fns = list( lwr = ~ lwr(.x), upr = ~ upr(.x)),
            .names = "{.col}_{.fn}"
   ))
 
 # remove .gid when no groups
 if ( !any_groups ) {
   out = select(out, !.gid)
 }
 
 out
}

# number of tvs: --------------------------------------------------------------

## estimate means for each year
tv_n_15 = est_recs(recs15, w15, grps = c('division', 'rurality'), var = 'tv_n')
tv_n_09 = est_recs(recs09, w09, grps = c('division', 'rurality'), var = 'tv_n')

## bind together for plotting
tv_n = 
   bind_rows( mutate(tv_n_15, year = '2015'), mutate(tv_n_09, year = '2009') )

## compute diffs
m = qnorm(.975)
tv_n_diff = tv_n %>%
   group_by(division, rurality) %>%
   arrange(year) %>%
   summarize(
      diff = diff(tv_n),
      se = sqrt(sum(tv_n_se^2)),
      .groups = 'drop' 
   ) %>%
   mutate(
      lwr = diff - m * se,
      upr = diff + m * se
   )


# tv_type: --------------------------------------------------------------------

## estimate proportions for each type and each year
tv_type_15 = 
   est_recs(recs15, w15, grps = c('division', 'rurality'), var = 'tv_type')
tv_type_09 = 
   est_recs(recs09, w09, grps = c('division', 'rurality'), var = 'tv_type')

## bind together for plotting and diffs
tv_type = 
   bind_rows( 
      mutate(tv_type_15, year = '2015'), 
      mutate(tv_type_09, year = '2009') 
   ) %>%
   mutate(
      p = 100 * p,
      p_lwr = pmax(100 * p_lwr, 0), 
      p_upr = pmin(100 * p_upr, 100)
   )

## compute diffs and std error
tv_type_diff = tv_type %>%
   group_by(division, rurality, tv_type) %>%
   arrange(year) %>%
   summarize(
      tv_type_diff = diff(p),
      se_diff = 100 * sqrt(sum(p_se^2)),
      .groups = 'drop' 
   ) %>%
   mutate(
      d = tv_type_diff,
      d_lwr = tv_type_diff - m * se_diff, 
      d_upr = tv_type_diff + m * se_diff
   )


# 79: -------------------------------------------------------------------------


