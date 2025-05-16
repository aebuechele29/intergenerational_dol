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
          (relation == 9 & year == 1968) ~ NA_real_,
        TRUE ~ relation
      ) 
    )

build <- build %>%
  filter(
    !is.na(relation),
    sample != 0,
    sequence <= 20,
  )

# CLEAN INDIVIDUAL DEMOGRAPHICS -------------------------------------------------
# CREATE BIRTH COHORTS --------------------------------------------------------
max_year <- efficient_max(build$year)

build <- build %>%
  mutate(
    age = if_else(age > 998, NA_real_, age),
    yob = round(year - age)
  ) %>%
  filter(
    !is.na(age)
  ) %>%
  select(-c(age))

# Choose person median birth year
build <- build %>%
  group_by(pid) %>%
  mutate(
    yob = as.integer((median(yob, na.rm = TRUE)))
  ) %>%
  ungroup()

# Create birth cohorts from 1890 to 1990
build <- build %>%
  mutate(
    birth_cohort = case_when(
      yob < 1900 ~ 1,
      yob < 1910 ~ 2,
      yob < 1920 ~ 3,
      yob < 1930 ~ 4,
      yob < 1940 ~ 5,
      yob < 1950 ~ 6,
      yob < 1960 ~ 7,
      yob < 1970 ~ 8,
      yob < 1980 ~ 9,
      yob < 1990 ~ 10,
      yob < 2000 ~ 11,
      TRUE ~ NA_real_
    )
  )

# YEAR OF DEATH -----------------------------------------------------------------
  # When available, the exact year of death is recorded.
  # When a range of years was reported, this variable contains
  # a four digit code in which the first two digits represent the first
  # possible year of death, and the last two digits represent the last
  # possible year.
build <- build %>%
  mutate(
    start_year = str_sub(as.character(yod), 1, 2),
    end_year   = str_sub(as.character(yod), 3, 4),
    dbl_start  = as.double(start_year),
    dbl_end    = as.double(end_year),
    ave_year   = round((dbl_start + dbl_end) / 2),
    ave_char   = case_when(
      ave_year < 10 ~ str_c("200", ave_year),
      ave_year < 20 ~ str_c("20",  ave_year),
      ave_year >= 20 ~ str_c("19", ave_year),
      TRUE ~ NA_character_
    ),
    final_year = case_when(
      yod == 9999 ~ NA_real_,
      !dbl_start %in% c(19, 20) &
        as.double(ave_char) > 1900 &
        as.double(ave_char) < 2200 ~ as.double(ave_char),
      TRUE ~ as.double(yod)
    )
  ) %>%
  select(-c(start_year, end_year, dbl_start, dbl_end, ave_year, ave_char, yod)) %>%
  rename(yod = final_year)

# GENDER ----------------------------------------------------------------
build <- build %>%
  mutate(
    male =
      case_when(
        male == 9 ~ NA_real_,
        male == 2 ~ 0,
        TRUE ~ male
      )
  )

# CLEAN INDIVIDUAL EDUCATION ----------------------------------------------------
  # After cleaning:
    # edu_cat1: Education categories: 0 = High school, 1 = Post-high school education
    # edu_cat2: Education categories: 0 = High school, 1 = Some post-high school, 2 = Bachelor's or higher.
    # hs, ba, ma: Binary variables for high school, bachelor's, and master's degrees

build <- build %>%
  mutate(
    edu = if_else(edu %in% c(97, 98, 99), NA_real_, as.double(edu))
  )

  build <- build %>%
  mutate(
    edu2 =
      case_when(
        edu <= 12 ~ 0,
        edu <= 15 ~ 1,
        (edu > 15 & edu < 99) ~ 2,
        edu == 99 ~ NA_real_
      ),
    edu3 =
      case_when(
        edu <= 12 ~ 0,
        (edu >= 13 & edu < 99) ~ 1,
        edu == 99 ~ NA_real_
      ),
    hs =
      case_when(
        edu < 12 ~ 0,
        (edu >= 12 & edu < 99) ~ 1,
        edu == 99 ~ NA_real_
      ),
    ba =
      case_when(
        edu < 16 ~ 0,
        (edu >= 16 & edu < 99) ~ 1,
        edu == 99 ~ NA_real_
      ),
    ma =
      case_when(
        edu < 17 ~ 0,
        (edu >= 17 & edu < 99) ~ 1,
        edu == 99 ~ NA_real_
      )
  )

# Create person max education vars
build <- build %>%
  group_by(pid) %>%
  mutate(
    max_edu2 = efficient_max(edu2, na.rm = TRUE),
    max_edu3 = efficient_max(edu3, na.rm = TRUE),
    max_hs = efficient_max(hs, na.rm = TRUE),
    max_ba = efficient_max(ba, na.rm = TRUE),
    max_ma = efficient_max(ma, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    max_edu2 =
      case_when(
        is.infinite(max_edu2) ~ NA_real_,
        TRUE ~ max_edu2
      ),
    max_edu3 =
      case_when(
        is.infinite(max_edu3) ~ NA_real_,
        TRUE ~ max_edu3
      ),
    max_hs =
      case_when(
        is.infinite(max_hs) ~ NA_real_,
        TRUE ~ max_hs
      ),
    max_ba =
      case_when(
        is.infinite(max_ba) ~ NA_real_,
        TRUE ~ max_ba
      ),
    max_ma =
      case_when(
        is.infinite(max_ma) ~ NA_real_,
        TRUE ~ max_ma
      )
  ) %>%
  select(-c(edu, edu2, edu3, hs, ba, ma)) %>%
  rename(edu_cat1 = max_edu3, edu_cat2 = max_edu2, hs = max_hs, ba = max_ba, ma = max_ma)


# CLEAN INDIVIDUAL FAMILY CHARACTERISTICS ---------------------------------------
# Clean Number of Children (# Live Births)   ----------------------------
  # Children: 98 NA/ DK 99 No birth history collected
build <- build %>%
  mutate(
    children = if_else(children %in% c(98, 99), NA_real_, as.double(children))
  )


# SELECT WEIGHTS-------  ---------------------------------------------------------
build <- build %>%
  arrange(pid, year) %>%
  group_by(pid) %>%
  mutate(last_weight = last(ind_weight2)) %>%
  ungroup() %>%
  mutate(
    tmp_year =
      case_when(
        last_weight == ind_weight2 ~ year,
        TRUE ~ NA_integer_
      ),
    weight_in_2021 =
      case_when(
        year == 2021 ~ ind_cross_weight2,
        TRUE ~ NA_integer_
      )
  ) %>%
  group_by(pid) %>%
  mutate(last_weight_year = last(tmp_year)) %>%
  ungroup() %>%
  select(-c(ind_weight2, ind_cross_weight2, tmp_year, weight_in_2021))

# CLEAN FAMILY DATA -------------------------------------------------------------------------------------------------

# CLEAN INCOME AND WEALTH ------------------------------------------------------------------
    # Add indicator for top-coded values, then adjust for inflation

topcode_rules <- tribble(
  ~var,                 ~year_start, ~year_end, ~topcode,
  "inc_all",         1969,        1979,      99999,
  "inc_all",         1980,        1980,      999999,
  "inc_all",         1981,        1983,      9999999,
  "inc_all",         1984,        1985,      999999,
  "inc_all",         1986,        1993,      9999999,
  "inc_all",         1994,        1997,      9999998,
  "inc_all",         1999,        2021,      9999999,

  "inc_tax_hs",      1969,        1978,      99999,
  "inc_tax_hs",      1979,        1980,      999999,
  "inc_tax_hs",      1982,        1996,      9999999,
  "inc_tax_hs",      1999,        2021,      9999999,

  "inc_tax_o",       1969,        1983,      99999,
  "inc_tax_o",       1984,        1993,      999999,
  "inc_tax_o",       1994,        2021,      9999998,

  "inc_trans_hs",    1970,        1992,      99999,
  "inc_trans_hs",    1993,        1993,      999999,
  "inc_trans_hs",    1994,        1996,      9999998,
  "inc_trans_hs",    1997,        1997,      999999,
  "inc_trans_hs",    1999,        2009,      9999998,
  "inc_trans_hs",    2011,        2021,      9999997,

  "inc_trans_o1",    1970,        1992,      99999,
  "inc_trans_o1",    1993,        1993,      999999,
  "inc_trans_o1",    1994,        1996,      9999998,
  "inc_trans_o1",    1997,        1997,      999999,
  "inc_trans_o1",    1999,        2009,      9999998,
  "inc_trans_o1",    2011,        2021,      9999997,

  "inc_trans_o2",    1970,        1992,      99999,
  "inc_trans_o2",    1993,        1993,      999999,
  "inc_trans_o2",    1994,        1996,      9999998,
  "inc_trans_o2",    1997,        1997,      999999,
  "inc_trans_o2",    1999,        2009,      9999998,
  "inc_trans_o2",    2011,        2021,      9999997,

  "wealth_nohouse",  1984,        2005,      999999998,
  "wealth_nohouse",  2007,        2009,      999999996,
  "wealth_nohouse",  2011,        2021,      999999997,

  "wealth",          1984,        2005,      999999998,
  "wealth",          2007,        2009,      999999996,
  "wealth",          2011,        2021,      999999997,

  "wealth_farmbus",  1984,        2005,      999999998,
  "wealth_farmbus",  2007,        2009,      999999996,
  "wealth_farmbus",  2011,        2021,      999999997,

  "wealth_checking", 1984,        2003,      999999998,
  "wealth_checking", 2005,        2005,      999999999,
  "wealth_checking", 2007,        2009,      999999996,
  "wealth_checking", 2011,        2017,      999999997,

  "wealth_re",       1984,        2005,      999999998,
  "wealth_re",       2007,        2009,      999999996,
  "wealth_re",       2011,        2021,      999999997,

  "wealth_stocks",   1984,        2005,      999999998,
  "wealth_stocks",   2007,        2009,      999999996,
  "wealth_stocks",   2011,        2021,      999999997,

  "wealth_vehicles", 1984,        2005,      999999998,
  "wealth_vehicles", 2007,        2009,      999999996,
  "wealth_vehicles", 2011,        2021,      999999997,

  "wealth_other",    1984,        2005,      999999998,
  "wealth_other",    2007,        2009,      999999996,
  "wealth_other",    2011,        2021,      999999997,

  "wealth_debt",     1984,        2005,      999999999,
  "wealth_debt",     2007,        2009,      999999997,

  "wealth_home",     1984,        2021,      999999997,

  "student_loans",   2011,        2021,      9999997
  
)

topcodes <- topcode_rules %>%
  rowwise() %>%
  mutate(year = list(seq(year_start, year_end))) %>%
  unnest(year) %>%
  select(var, year, topcode) %>%
  ungroup()

topcode_wide <- topcodes %>%
  pivot_wider(names_from = var, values_from = topcode, names_prefix = "topcode_")

topcode_max <- topcodes %>%
  group_by(var) %>%
  summarise(topmax = max(topcode, na.rm = TRUE), .groups = "drop")

build <- build %>%
  left_join(topcode_wide, by = "year")

money_vars <- c(
  "inc_all", "inc_tax_hs", "inc_tax_o",
  "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_farmbus", "wealth_checking", "wealth_debt",
  "wealth_re", "wealth_stocks", "wealth_vehicles", "wealth_other",
  "wealth_home", "student_loans"
)

for (var in money_vars) {
  topcode_col <- paste0("topcode_", var)       # this is the threshold for the year
  indicator_col <- paste0("ind_top_", var)    # new binary indicator column

  # Get max topcode value across years for this variable
  topmax_val <- topcode_max %>%
    filter(var == !!var) %>%
    pull(topmax)

  # Replace topcoded values and create a flag
  build <- build %>%
    mutate(
      !!indicator_col := as.integer(.data[[var]] >= (.data[[topcode_col]] - 1)),
      !!var := if_else(
        .data[[indicator_col]] == 1,
        topmax_val,
        .data[[var]]
      )
    )
}

build <- build %>%
  select(-starts_with("topcode_"))


# Join inflation, adjust, and rename
build <- build %>%
  left_join(cpi, by = "year", relationship = "many-to-one") %>%
  mutate(across(
    all_of(money_vars),
    ~ .x * inflation_value / 100,
    .names = "infl_{.col}"
  )) %>%
  select(-all_of(money_vars), -inflation_value) %>%
  rename_with(~ gsub("^infl_", "", .x), starts_with("infl_"))


# CLEAN FAMILY CHARACTERISTICS --------------------------------------------------
  
# Clean homeownership -----------------------------------------------------
  # Homeownership: 1 = own; 5 = rents; 8 = neither;
  # from 1994 9 = DK/ NA/ refused, 0 = inappropriate
  # From 2007 9 = "wild code"

build <- build %>%
  rename(own_home = house_status) %>%
  mutate(
    own_home =
      case_when(
        (own_home == 9 | own_home == 0 |
           is.na(own_home) ~ NA_real_),
        (own_home == 5 | own_home == 8) ~ 0,
        TRUE ~ own_home
      )
  )

# Clean Marital Status --------------------------------------------
  # Marital status of head
  # 1 married (from 1977 also permanently cohabiting; 1982 switched from spouse
  # may not be in FU to spouse must be in FU);
  # 2 single; 3 widowed; 4 divorced;
  # 5 separated (1982 also got legally married but spouse not in FU);
  # 9 NA (1968, not 1969-1981, from 1982 also DK)
  # 1977 started other variable differentiating cohabiting vs legally married

build <- build %>%
  mutate(
    married = case_when(
      relation == 1 & marital > 1 & marital < 9 ~ 0,
      relation == 1 & marital == 9 ~ NA_real_,
      relation == 1 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-marital)

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
  select(-contains("wife")) %>%
  select(-contains("head"))

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
rm(cpi, pid_lookup, sample_fast, topcode_wide, topcode_rules, topcodes, topcode_max)
