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


sample <- sample %>% # Parental homeownership
  mutate(
    parental_own_home =
      case_when(
        relation == 3 ~ own_home,
        TRUE ~ NA_real_
      ),
    own_home = # Only head and spouse actually own
      case_when(
        relation == 1 | relation == 2 ~ own_home,
        is.na(own_home) ~ NA_real_,
        TRUE ~ 0
      )
  )