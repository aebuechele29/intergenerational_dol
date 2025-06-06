# INPUTS: 1_build_panel/output/build.rds, functions/src/functions.R
# OUTPUTS: 2_clean_panel/output/clean.rds

# LOAD DATA ------------------------------------------------------------------
# Each row represents a person-year, data is collected for the individual or the family
build <- readRDS(here("1_build_panel", "output", "build.rds"))


# CLEAN INDIVIDUAL DATA --------------------------------------------------------------------------------------------------

# CLEAN IDENTIFIERS AND LIMIT SAMPLE -------------------------------------------------------------
  # 1968 1 head 2 wife/ spouse 3, child 4-7 other 8 spouse 9/ 0 NA
  # 1969-1982 8 other 9 spouse 0 NA
  # 1983-1999 10 head 20 22 88 90 spouse 30-38 83 child 40-75 95-98 other 0 NA
  # 2017 92 spouse
  # 1968-1982 4 = sibling of head; 5 = parent of head; 7 = other adult relative
  # 1983-  40, 47, 48 = sibling; 50, 57, 58 = parent; 
  # 60, 65 grand/ great-grandson/ daughter
  # 66, 67, 68, 69 = grand/ great-grandparent;
  # 70, 71 = nephew; 72, 73 = uncle/ aunt; 74, 75 = cousin; 95, 96, 97 = other 

# Recode relation to head, spouse, child, or other 
build <- build %>%
  mutate(
    relation =
      case_when(
        # Head
        relation == 1 | relation == 10 ~ 1, 
        # Spouse
        relation == 2 | relation == 20 | relation == 22 | 
          (relation == 8 & year == 1968) |
          (relation == 9 & (year > 1968 & year < 1983)) |
          (relation > 87 & relation < 93) ~ 2,
        # Child
        (relation == 3 | relation == 83 |
           (relation > 29 & relation < 39)) ~ 3,
        # Other
        (relation > 3 & relation < 8) |
          (relation > 39 & relation < 76) |
          (relation > 94 & relation < 99) |
          (relation == 8 & (year > 1968 & year < 1983)) ~ 4,
        relation == 0 | is.na(relation) |
          (relation == 9 & year == 1968) ~ 0,
        TRUE ~ relation
      ) 
    )

# CLEAN INDIVIDUAL DEMOGRAPHICS -------------------------------------------------

# CREATE BIRTH COHORTS --------------------------------------------------------
max_year <- efficient_max(build$year)

build <- build %>%
  mutate(
    age_head = if_else(age_head > 998, NA_real_, age_head),
    age_spouse = if_else(age_spouse > 998, NA_real_, age_spouse),
    yob_head = round(year - age_head),
    yob_spouse = round(year - age_spouse),
    age_diff = age_head - age_spouse
  ) %>%
  mutate(
    birth_cohort_head = case_when(
      yob_head < 1900 ~ 1,
      yob_head < 1910 ~ 2,
      yob_head < 1920 ~ 3,
      yob_head < 1930 ~ 4,
      yob_head < 1940 ~ 5,
      yob_head < 1950 ~ 6,
      yob_head < 1960 ~ 7,
      yob_head < 1970 ~ 8,
      yob_head < 1980 ~ 9,
      yob_head < 1990 ~ 10,
      yob_head < 2000 ~ 11,
      TRUE ~ NA_real_
    ),
    birth_cohort_spouse = case_when(
      yob_spouse < 1900 ~ 1,
      yob_spouse < 1910 ~ 2,
      yob_spouse < 1920 ~ 3,
      yob_spouse < 1930 ~ 4,
      yob_spouse < 1940 ~ 5,
      yob_spouse < 1950 ~ 6,
      yob_spouse < 1960 ~ 7,
      yob_spouse < 1970 ~ 8,
      yob_spouse < 1980 ~ 9,
      yob_spouse < 1990 ~ 10,
      yob_spouse < 2000 ~ 11,
      TRUE ~ NA_real_
    )
  )

# GENDER ----------------------------------------------------------------
build <- build %>%
  mutate(
    sex =
      case_when(
        sex == 9 ~ NA_real_,
        sex == 2 ~ 0,
        TRUE ~ sex
      )
  )

# CLEAN INDIVIDUAL EDUCATION ----------------------------------------------------
  # After cleaning: edu_cat = 0 = High school, 1 = Some post-high school, 2 = Bachelor's or higher.

build <- build %>%
  mutate(
    edu = if_else(edu %in% c(97, 98, 99), NA_real_, as.double(edu))
  )

  build <- build %>%
  mutate(
    edu_cat =
      case_when(
        edu <= 12 ~ 0,
        edu <= 15 ~ 1,
        (edu > 15 & edu < 99) ~ 2,
        edu == 99 ~ NA_real_
      ))


# CLEAN INDIVIDUAL FAMILY CHARACTERISTICS ---------------------------------------
# Clean Births   ----------------------------
build <- build %>%
  mutate(
    births = as.numeric(births),
    first_birth = as.numeric(first_birth),
    last_birth = as.numeric(last_birth),
    second_birth = as.numeric(second_birth),
    third_birth = as.numeric(third_birth),
    fourth_birth = as.numeric(fourth_birth),

    births = if_else(births %in% c(98, 99), NA_real_, births),
    first_birth = if_else(first_birth > 9998, NA_real_, first_birth),
    last_birth = if_else(last_birth > 9998, NA_real_, last_birth),
    second_birth = if_else(second_birth > 9998, NA_real_, second_birth),
    third_birth = if_else(third_birth > 9998, NA_real_, third_birth),
    fourth_birth = if_else(fourth_birth > 9998, NA_real_, fourth_birth),

    first_child_age = round(year - first_birth),
    last_child_age = round(year - last_birth),
    second_child_age = round(year - second_birth),
    third_child_age = round(year - third_birth),
    fourth_child_age = round(year - fourth_birth),

    child_under5 = if_else(
      pmin(first_child_age, second_child_age, third_child_age, fourth_child_age, last_child_age, na.rm = TRUE) < 5,
      1, 0, missing = 0
    )
  ) %>%
  select(-first_birth, -last_birth, -second_birth, -third_birth, -fourth_birth,
  -first_child_age, -second_child_age, -third_child_age, -fourth_child_age, -last_child_age)


# CLEAN FAMILY DATA -------------------------------------------------------------------------------------------------

# CLEAN INCOME VARIABLES -------------------------------------------------------------
build <- build %>%
  mutate(
    hrly_head = ifelse(hrly_head >= 9997 | (year == 1968 & hrly_head >= 98 & hrly_head <= 100), NA, hrly_head),
    hrly_spouse = ifelse(hrly_spouse >= 9997 | (year == 1968 & hrly_spouse >= 98 & hrly_spouse <= 100), NA, hrly_spouse),
    work_head = ifelse(work_head >= 9997, NA, work_head),
    work_spouse = ifelse(work_spouse >= 9997, NA, work_spouse)
  )

money_vars <- c("fam_inc", "hrly_head", "hrly_spouse")

build <- build %>%
  mutate(across(
    all_of(money_vars),
    ~ if_else(.x >= 98 & .x <= 99, 0, .x)
  )) %>%

  # Join CPI and adjust for inflation
  left_join(cpi, by = "year", relationship = "many-to-one") %>%
  mutate(across(
    all_of(money_vars),
    ~ .x * inflation_value / 100,
    .names = "infl_{.col}"
  )) %>%
  select(-all_of(money_vars), -inflation_value) %>%
  rename_with(~ gsub("^infl_", "", .x), starts_with("infl_")) %>%

  # Calculate annual earnings
  mutate(
    earn_head = hrly_head * work_head,
    earn_spouse = hrly_spouse * work_spouse
  )

# CLEAN HOUSEWORK HOURS -------------------------------------------------------------
build <- build %>%
  mutate(
    hw_head = if_else(hw_head >= 99, NA_real_, hw_head),
    hw_spouse = if_else(hw_spouse >= 99, NA_real_, hw_spouse)
  )


# CLEAN FAMILY CHARACTERISTICS --------------------------------------------------
  
# Clean homeownership -----------------------------------------------------
  # Homeownership: 1 = own; 5 = rents; 8 = neither;
  # from 1994 9 = DK/ NA/ refused, 0 = inappropriate
  # From 2007 9 = "wild code"

build <- build %>%
  rename(own_home = housing) %>%
  mutate(
    own_home =
      case_when(
        (own_home == 9 | own_home == 0 |
           is.na(own_home) ~ NA_real_),
        (own_home == 5 | own_home == 8) ~ 0,
        TRUE ~ own_home
      )
  )

# Clean State -------------------------------------------------------------
  # 1-51 PSID state code
  # 0 Inap. outside U.S. proper (from 1970)
  # 99 DK, NA (from 1972)

build <- build %>%
  mutate(
    state =
      case_when(
        state == 0 ~ "52",
        state == 99 ~ NA_character_,
        TRUE ~ as.character(state)
      )
  )

# CLEAN FAMILY RACE -------------------------------------------------------------
  # Recode Race
  # 1 white 2 black 3 4 7 other 8 9 0 NA
build <- build %>%
  mutate(
    across(
      .cols = contains("race"),
      ~ case_when(
        .x == 2 ~ 3, # black
        (.x > 2 & .x < 8) ~ 2, # other
        (.x == 0 | .x == 9 | .x == 8) ~ NA_real_, # NA
        TRUE ~ .x
      )
    )
  ) # white

build <- build %>%
  mutate(
    race_first = case_when(
      relation == 1 ~ race1_head,
      relation == 2 ~ race1_wife
    ),
    race_second = case_when(
      relation == 1 ~ race2_head,
      relation == 2 ~ race2_wife
    ),
    race_third = case_when(
      relation == 1 ~ race3_head,
      relation == 2 ~ race3_wife
    ),
    race_fourth = case_when(
      relation == 1 ~ race4_head,
      relation == 2 ~ race4_wife
    )
  ) %>%
  select(-contains("wife")) 

build <- build %>%
  mutate(
    max_race = pmax(
      !!!select(., contains("race_")),
      na.rm = TRUE
    ),
    max_race = if_else(max_race == -Inf, NA_real_, max_race)
  )

# According to codebook mention order doesn't matter. Black if black exists, then other then white -Sayer, Cohen, and Casper (2004)
build <- build %>%
  group_by(pid) %>%
  mutate(
    race_year = case_when(
      !is.infinite(max_race) & !is.na(max_race) ~ as.double(year),
      TRUE ~ 0
    ),
    max_race_year = efficient_max(race_year, na.rm = TRUE),
    # Determine race_pid based on max_race_year
    race_pid = case_when(
      max_race_year == year ~ max_race,
      TRUE ~ NA_real_
    ),
    race = efficient_max(race_pid, na.rm = TRUE)
  ) %>%
  mutate(race = na_if(race, -Inf)) %>%
  ungroup()

# Handle race for each pid/year combination
sample_fast <- build %>%                       
  summarise(                                    
    max_head_race  = efficient_max(race[relation == 1], na.rm = TRUE),
    max_other_race = efficient_max(race[relation != 1], na.rm = TRUE),
    .by = c(pid, year)                         
  ) %>% 
  mutate(                                       
    max_person_race = coalesce(
      na_if(max_head_race , Inf),               
      na_if(max_other_race, Inf)               
    )
  )

build <- left_join(build, sample_fast, by = c("pid", "year"))

# Uses a Summary + Join Approach to Improve Speed / Memory Usage
pid_lookup <- build %>%                                
  summarise(                                        
    max_max_race = efficient_max(max_person_race, na.rm = TRUE),
    .by = pid
  )

build <- build %>%
  left_join(pid_lookup, by = "pid") %>%
  mutate(
    race = coalesce(                       
      race, 
      na_if(max_max_race, Inf)                      
    ),
    black = case_when(                          
      race == 3        ~ 1,
      race %in% 1:2    ~ 0,
      TRUE             ~ race
    )
  ) %>%
 select(                                      
    -max_max_race, 
    -starts_with("max_"), 
    -ends_with("_race"),
    -race_first,
    -race_second,
    -race_third,
    -race_fourth,
    -race_pid
  )

# SAVE CLEAN DATA -------------------------------------------------------------
file.remove(list.files(here("2_clean_panel", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(build, here("2_clean_panel", "output", "clean.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(cpi, pid_lookup, sample_fast)
