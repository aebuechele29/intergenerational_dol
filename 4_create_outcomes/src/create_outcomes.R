# INPUTS: 3_create_households/src/households.R
# OUTPUTS: 4_create_outcomes/output/analysis.rds

# LOAD DATA ------------------------------------------------------------------
households <- readRDS(here("3_create_households", "output", "households.rds"))

# FILTER SAMPLE -------------------------------------------------------------
households <- households %>%
  mutate(
    births = if_else(
      (births_head > 0 | births_spouse > 0), 
      1, 
      0,
      missing = 0  
    )
  )

n_total <- nrow(households)

step1 <- households %>% filter(!is.na(hw_head))
n1 <- nrow(step1)

step2 <- step1 %>% filter(!is.na(hw_spouse))
n2 <- nrow(step2)

step3 <- step2 %>% filter(!is.na(earn_head))
n3 <- nrow(step3)

step4 <- step3 %>% filter(!is.na(earn_spouse))
n4 <- nrow(step4)

step5 <- step4 %>% filter(!is.na(sex_spouse))
n5 <- nrow(step5)

step6 <- step5 %>% filter(sex_spouse != sex_head)
n6 <- nrow(step6)

step7 <- step6 %>% filter(marital_head == 1)
n7 <- nrow(step7)

step8 <- step7 %>% filter(births != 0)
n8 <- nrow(step8)

step9 <- step8 %>% filter(!(earn_head == 0 & earn_spouse == 0))
n9 <- nrow(step9)

households <- step9

filter_summary <- tribble(
  ~Step, ~Description, ~Remaining, ~Lost,
  0, "Total rows", n_total, 0,
  1, "Drop missing hw_head", n1, n_total - n1,
  2, "Drop missing hw_spouse", n2, n1 - n2,
  3, "Drop missing earn_head", n3, n2 - n3,
  4, "Drop missing earn_spouse", n4, n3 - n4,
  5, "Drop missing sex_spouse", n5, n4 - n5,
  6, "Drop same-sex couples", n6, n5 - n6,
  7, "Keep only married heads", n7, n6 - n7,
  8, "Keep those with children", n8, n7 - n8,
  9, "Drop if both earners have zero income", n9, n8 - n9
)

print(filter_summary)

rm(step1, step2, step3, step4, step5, step6, step7, step8, step9)

# CLEAN UP CHILD UNDER 5
households <- households %>%
  mutate(
    child_under5 = if_else(child_under5_head == 1 | child_under5_spouse == 1, 1, 0)
  ) %>%
  select(-child_under5_head, -child_under5_spouse, -births_head, -births_spouse)

# CREATE COUPLE LEVEL ID
households <- households %>% 
  group_by(pid_head, pid_spouse) %>%
  mutate(couple_id = cur_group_id()) %>%
  ungroup() 

unique_couples <- households %>% 
  distinct(couple_id) %>% 
  nrow()

print(unique_couples)

# CREATE OUTCOMES -------------------------------------------------------------
households <- households %>%
  mutate(
    # full time defined as working 35 hours per week, 48 weeks per year
    ft_head = work_head >= 1680,
    ft_spouse = work_spouse >= 1680,

    earn_total = earn_head + earn_spouse,
    hw_total = hw_head + hw_spouse,

    earn_head_share = earn_head / earn_total,
    earn_spouse_share = earn_spouse / earn_total,

    hw_head_share = hw_head / hw_total,
    hw_spouse_share = hw_spouse / hw_total,

    household_type = case_when(
      # Husband is sole breadwinner
      earn_spouse_share <= .1 & hw_spouse > hw_head ~ "Sole Breadwinner",
      # Husband is primary breadwinner
      earn_spouse_share > .60  & hw_spouse > hw_head ~ "Primary Breadwinner",
      TRUE ~ "Other"
    )
  ) %>%
  select(-earn_total, -hw_total, -age_head, -age_spouse, -yob_spouse, -age_diff, -marital_head, -race,
  -cc_head, -cc_spouse, -id1968_head, -id1968_spouse, -relation_head, -relation_spouse)

households <- households %>%
  mutate(
    traditional = if_else(household_type %in% c("Sole Breadwinner", "Primary Breadwinner"), 1, 0),
    spouse_earnings = if_else(earn_spouse_share > 0.6, 1, 0),
    spouse_housework = if_else(hw_spouse_share > 0.6, 1, 0),
    spouse_working = if_else(ft_spouse == TRUE, 1, 0)
  )

# ADD EXTENDED FAMILY VARIABLES --------------------------------------------------
outcomes <- c("traditional", "spouse_earnings", "spouse_housework", "spouse_working")

build_parent_outcome <- function(df, outcome_var) {
  
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # HEAD - PARENT 1
  p1 <- df %>%
    select(parent1_head, pid) %>%
    rename(parent_pid = parent1_head, original_pid = pid) %>%
    left_join(df %>% select(pid, !!sym(outcome_var), year) %>%
                rename(parent_pid = pid, val = !!sym(outcome_var), yr = year),
              by = "parent_pid") %>%
    rename(pid_head = original_pid) %>%
    group_by(pid_head) %>%
    summarise(
      val1 = get_mode(val),
      yr1 = get_mode(yr),
      .groups = "drop"
    )
  
  # HEAD - PARENT 2
  p2 <- df %>%
    select(parent2_head, pid) %>%
    rename(parent_pid = parent2_head, original_pid = pid) %>%
    left_join(df %>% select(pid, !!sym(outcome_var), year) %>%
                rename(parent_pid = pid, val = !!sym(outcome_var), yr = year),
              by = "parent_pid") %>%
    rename(pid_head = original_pid) %>%
    group_by(pid_head) %>%
    summarise(
      val2 = get_mode(val),
      yr2 = get_mode(yr),
      .groups = "drop"
    )
  
  # Combine Head's Parents
  head_combined <- full_join(p1, p2, by = "pid_head") %>%
    mutate(head_val = coalesce(val1, val2),
           head_year = coalesce(yr1, yr2)) %>%
    select(pid_head, head_val, head_year)
  
  # SPOUSE - PARENT 1
  sp1 <- df %>%
    select(parent1_spouse, pid) %>%
    rename(parent_pid = parent1_spouse, original_pid = pid) %>%
    left_join(df %>% select(pid, !!sym(outcome_var), year) %>%
                rename(parent_pid = pid, val = !!sym(outcome_var), yr = year),
              by = "parent_pid") %>%
    rename(pid_spouse = original_pid) %>%
    group_by(pid_spouse) %>%
    summarise(
      val1 = get_mode(val),
      yr1 = get_mode(yr),
      .groups = "drop"
    )
  
  # SPOUSE - PARENT 2
  sp2 <- df %>%
    select(parent2_spouse, pid) %>%
    rename(parent_pid = parent2_spouse, original_pid = pid) %>%
    left_join(df %>% select(pid, !!sym(outcome_var), year) %>%
                rename(parent_pid = pid, val = !!sym(outcome_var), yr = year),
              by = "parent_pid") %>%
    rename(pid_spouse = original_pid) %>%
    group_by(pid_spouse) %>%
    summarise(
      val2 = get_mode(val),
      yr2 = get_mode(yr),
      .groups = "drop"
    )
  
  # Combine Spouse's Parents
  spouse_combined <- full_join(sp1, sp2, by = "pid_spouse") %>%
    mutate(spouse_val = coalesce(val1, val2),
           spouse_year = coalesce(yr1, yr2)) %>%
    select(pid_spouse, spouse_val, spouse_year)
  

  df <- df %>%
    left_join(head_combined, by = c("pid" = "pid_head")) %>%
    left_join(spouse_combined, by = c("pid" = "pid_spouse")) %>%
    mutate(
      !!paste0("parent_", outcome_var) := coalesce(head_val, spouse_val),
      !!paste0("parent_", outcome_var, "_side") := case_when(
        !is.na(head_val) ~ "head",
        !is.na(spouse_val) ~ "spouse",
        TRUE ~ NA_character_
      ),
      !!paste0("parent_", outcome_var, "_year") := coalesce(head_year, spouse_year)
    ) %>%
    select(-head_val, -spouse_val, -head_year, -spouse_year)
  
  return(df)
}

# Apply the function to all outcomes
for (o in outcomes) {
  households <- build_parent_outcome(households, o)
}

# Combine parent years into parent_data_year
households <- households %>%
  mutate(
    parent_data_year = pmax(
      parent_traditional_year,
      parent_spouse_earnings_year,
      parent_spouse_housework_year,
      parent_spouse_working_year,
      na.rm = TRUE
    )
  )

# Create a temporary ID to help track the source
households <- households %>%
  mutate(
    parent_data_side = case_when(
      parent_data_year == parent_traditional_year ~ parent_traditional_side,
      parent_data_year == parent_spouse_earnings_year ~ parent_spouse_earnings_side,
      parent_data_year == parent_spouse_housework_year ~ parent_spouse_housework_side,
      parent_data_year == parent_spouse_working_year ~ parent_spouse_working_side,
      TRUE ~ NA_character_
    )
  )
  
# Remove interim variables
households <- households %>%
  select(-parent_traditional_side, -parent_spouse_earnings_side, -parent_spouse_working_side, -parent_spouse_housework_side,
  -parent_traditional_year, -parent_spouse_earnings_year, -parent_spouse_working_year, -parent_spouse_housework_year,
  -parent1_head, -parent2_head, -parent1_spouse, -parent2_spouse, -pid_head, -pid_spouse, -sex_head, -sex_spouse, -births)

households <- households %>%
  select(-starts_with("grandparent")) 

# SAVE OUTPUT -----------------------------------------------------------------
saveRDS(households, here("4_create_outcomes", "output", "analysis.rds"))

rm(hh_limit, hh_limit_sp1, hh_limit_sp2, lookup_p1_spouse, lookup_p2_spouse, 
lookup_parent1, lookup_parent2, parent1, parent1_spouse, parent1_spouse_unique, parent2, 
parent2_spouse, parent2_spouse_unique, parent2_unique, parent1_unique)

