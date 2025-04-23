# % childhood years hh married --------------------------------------------

sample <- sample %>%
  mutate(
    child_hh_married =
      case_when(
        ((year - birth) < 15 & (year - birth) > 0) &
          relation == 3 & married == 1 ~ 1,
        ((year - birth) < 15 & (year - birth) > 0) &
          relation == 3 & married == 0 ~ 0,
        TRUE ~ NA_real_
      )
  ) %>%
  group_by(pid) %>%
  mutate(
    pct_hh_married = (mean(child_hh_married, na.rm = TRUE)) * 100,
    pct_hh_married = na_if(pct_hh_married, -Inf)
  ) %>%
  ungroup() %>%
  select(-c(child_hh_married, marital))


# Proportion in single parent hh ages 0-14 --------------------------------------------
sample <- sample %>%
  mutate(has_single_parent =
           case_when(
             coresiding_parent_count == 1 ~ 1,
             !is.na(coresiding_parent_count) ~ 0,
             TRUE ~ NA_real_
           )) %>%
  group_by(pid) %>%
  mutate(
    prop_single_parent =
      efficient_mean(has_single_parent, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(prop_single_parent = ifelse(is.nan(prop_single_parent),
                                     NA_real_, prop_single_parent
  )) %>%
  select(-has_single_parent)

# Calculate parent-child co-residence
parents_in_hh <- sample %>% 
  left_join(p_fims,
            by = "pid",
            relationship = "many-to-many") %>% 
  mutate(parent_pid =
           case_when(
             (year - birth) < 15 & (year - birth) > 0 ~ parent_pid,
             TRUE ~ NA_real_)) %>% 
  group_by(year, interview) %>% 
  mutate(all_pids_in_hh = list(pid)) %>% 
  ungroup() %>% 
  mutate(is_parent_resident = map2_lgl(parent_pid, all_pids_in_hh, ~ .x %in% .y),
         parent_counter = case_when(
           is_parent_resident == TRUE ~ 1,
           is.na(parent_pid) ~ NA_real_,
           TRUE ~ 0))

parents_per_child <- parents_in_hh %>% 
  filter(!is.na(parent_counter)) %>% 
  group_by(year, interview, pid) %>% 
  summarize(coresiding_parent_count = sum(parent_counter, na.rm = TRUE), .groups = 'drop') %>% 
  ungroup()

sample <- sample %>% 
  left_join(parents_per_child, by = c("year", "interview", "pid"))

rm(parents_per_child, parents_in_hh, p_fims) 

# Clean Year of Death ----------------------------------------------------------

sample <- sample %>%
  mutate(
    str_yod = as.character(yod),
    start_year = str_sub(str_yod, start = 1, end = -3),
    end_year = str_sub(str_yod, start = -2, end = -1),
    dbl_start = as.double(start_year),
    dbl_end = as.double(end_year)
  ) %>%
  rowwise() %>%
  mutate(
    ave_year = round(mean(
      c_across(
        c(
          dbl_start,
          dbl_end
        )
      ),
      na.rm = TRUE
    ))
  ) %>%
  ungroup()

sample <- sample %>%
  mutate(
    ave_char = case_when(
      ave_year < 10 ~ str_c("200", as.character(ave_year)),
      (ave_year < 20 & ave_year > 9) ~
        str_c("20", as.character(ave_year)),
      ave_year > 20 ~ str_c("19", as.character(ave_year)),
      TRUE ~ NA_character_
    ),
    final_year = as.integer(case_when(
      yod == 9999 ~ NA_real_,
      (dbl_start != 20 & dbl_start != 19) &
        (ave_char > 1900 & ave_char < 2200) ~
        as.double(ave_char),
      TRUE ~ yod
    ))
  ) %>%
  select(-c(
    ave_char, ave_year, dbl_end, dbl_start, end_year,
    start_year, str_yod, yod
  )) %>%
  rename(yod = final_year)

# When available, the exact year of death is recorded.
# When a range of years was reported, this variable contains
# a four digit code in which the first two digits represent the first
# possible year of death, and the last two digits represent the last
# possible year.

# Select Weights  ---------------------------------------------------------

sample <- sample %>%
  arrange(pid, year) %>%
  group_by(pid) %>%
  mutate(last_weight = last(weight)) %>%
  ungroup() %>%
  mutate(
    tmp_year =
      case_when(
        last_weight == weight ~ year,
        TRUE ~ NA_integer_
      ),
    weight_in_2021 =
      case_when(
        year == 2021 ~ cross_weight,
        TRUE ~ NA_integer_
      )
  ) %>%
  group_by(pid) %>%
  mutate(last_weight_year = last(tmp_year)) %>%
  ungroup() %>%
  select(-c(weight, cross_weight, tmp_year))



#   ------------------------------------------------------

# TODO: clean gene after import again

# 0	non-sample and not part of the elderly group
# 1	original sample (ER30002=001-026)
# 2	born-in sample (ER30002=030-169)
# 3	moved-in sample
# 4	joint inclusion sample
# 5	a non-sample parent that requires following
# 6	non-sample elderly



# Discard NUMFU -----------------------------------------------------------

# Not using number in family unit for now
sample <- sample %>%
  select(-numfu)


# Make Time Invariant Constant for Reshape   _----------------------------
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    across(
      .cols = c(
        weight_in_2021,
        major, college_degree
      ),
      .fns = ~ efficient_max(.x)
    )
  ) %>%
  ungroup()

# #TODO: Removed gene, cluster, stratum, children from above because now fixed in import stage.
#  Delete this comment once confirmed it runs.


# Reshape Dataframe -------------------------------------------------------

time_variant_vars <- c(
  "sequence", "wealth", "id1968",
  "relation", "income", "own_home", "state",
  "married", "parental_income", "parental_own_home",
  "parental_wealth", "family_business"
)

reshaped <- sample %>%
  pivot_wider(
    names_from = year,
    values_from = all_of(time_variant_vars),
    names_sep = "."
  )


# Mark Focal Children -----------------------------------------------------

reshaped <- reshaped %>%
  mutate(
    sample_child =
      case_when(
        birth < 1980 ~ 0,
        max_year - birth >= 25 ~ 1,
        TRUE ~ 0
      )
  )


# Saves file -----------------------------------------------------------

save(reshaped, file = file.path(
  path, "clean_merged",
  "output", "clean_merged.Rds"
))