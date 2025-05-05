# INPUTS: 1_build_panel/output/merged.rds, functions/src/functions.R
# OUTPUTS: 

# Load Merged Data ---------------------------------------------------
sample <- readRDS(here("1_build_panel", "output", "merged.rds"))

# Limit Sample & Create Variables for Clan Construction --------------
max_year <- efficient_max(sample$year)

sample <- sample %>%
  mutate(
    age = if_else(age == 999, NA_real_, age),
    birth = round(year - age)
  )

# Capture year censored
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    censored_year = efficient_max(year[year < max_year], na.rm = TRUE)
  ) %>%
  ungroup()

# Choose person median birth year
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    birth = as.integer((median(birth, na.rm = TRUE)))
  ) %>%
  ungroup()

# CLEAN IDENTIFIERS -------------------------------------------------------------
  # 1968 1 head 2 wife/ spouse 3, child 4-7 other 8 spouse 9/ 0 NA
  # 1969-1982 8 other 9 spouse 0 NA
  # 1983-1999 10 head 20 22 88 90 spouse 30-38 83 child 40-75 95-98 other 0 NA
  # 2017 92 spouse
  # 1968-1982 4 = sibling of head; 5 = parent of head; 7 = other adult relative
  # 1983-  40, 47, 48 = sibling; 50, 57, 58 = parent; 
  # 60, 65 grand/ great-grandson/ daughter
  # 66, 67, 68, 69 = grand/ great-grandparent;
  # 70, 71 = nephew; 72, 73 = uncle/ aunt; 74, 75 = cousin; 95, 96, 97 = other 

# I think the non_parent_fam is unnecessary but leaving for now 20250504
sample <- sample %>%
  mutate(
    non_parent_fam = 
      case_when(
        (relation == 4 | relation == 5 | relation == 7 |
           (relation > 39 & relation < 76) |
           (relation > 94 & relation < 98 )) &
          (year - birth) > 18 ~ 1,
        TRUE ~ 0),
    old_relation = relation,
    relation =
      case_when(
        relation == 1 | relation == 10 ~ 1,
        relation == 2 | relation == 20 | relation == 22 |
          (relation == 8 & year == 1968) |
          (relation == 9 & (year > 1968 & year < 1983)) |
          (relation > 87 & relation < 93) ~ 2,
        (relation == 3 | relation == 83 |
           (relation > 29 & relation < 39)) ~ 3,
        (relation > 3 & relation < 8) |
          (relation > 39 & relation < 76) |
          (relation > 94 & relation < 99) |
          (relation == 8 & (year > 1968 & year < 1983)) ~ 4,
        relation == 0 | is.na(relation) |
          (relation == 9 & year == 1968) ~ NA_real_,
        TRUE ~ relation
      ),
    child = ifelse(relation == 3, 1, 0)
  ) %>%
  group_by(pid) %>%
  mutate(ever_child = efficient_max(child)) %>%
  ungroup() %>%
  select(-child)

# CLEAN INCOME AND WEALTH ------------------------------------------------------------------
    # -------------------------------
    # Topcoding Rules 
    # -------------------------------

    # Total Income = income_all [V81]
    # 1969–1979: topcode = 99,999
    # 1980, 1984–1985: topcode = 999,999
    # 1981–1983, 1986–1993, 1999–2021: topcode = 9,999,999
    # 1994–1997: topcode = 9,999,998; 9,999,999 = Latino sample

    # Taxable Income Head & Spouse = income_tax_hs [V76]
    # 1968: -242 to 1 = loss; 1–80,000 = profit
    # 1969, 1971–1978: topcode = 99,999
    # 1979–1980: topcode = 999,999
    # 1982–1996, 1999–2021: topcode = 9,999,999
    # 1999: no topcode

    # Taxable Income – Others = income_tax_others [V79]
    # 1969–1983: topcode = 99,999
    # 1984–1993: topcode = 999,999
    # 1994–2021: topcode = 9,999,998; 9,999,999 = Latino sample (1994–1995)

    # Transfer Income – Head & Spouse = transfer_hs [V1220]
    # 1970–1992: topcode = 99,999+
    # 1993: topcode = 999,999+
    # 1994–1995: topcode = 9,999,998; 9,999,999 = Latino sample
    # 1996: topcode = 9,999,998
    # 1997: topcode = 999,999+
    # 1999–2009: topcode = 9,999,998+
    # 2011–2021: topcode = 9,999,997+
    # 1994–2021: excludes Social Security — add manually
    # Note: Exclude or adjust top 1% values (1994–2021)

    # Transfer Income – Others = transfer_others1, transfer_others 2 [V527 (1969–1993), V11576 (1985–2021)]
    # Same topcodes as Transfer – Head & Spouse
    # V527 is prorated; V11576 is not — merge carefully

    # Household Wealth Excl. Home Equity = wealth_nohouse [S116]
    # 1984–2005: top = 999,999,998; missing = -99,999,999
    # 2007–2009: top = 999,999,996
    # 2011–2021: top = 999,999,997
    # Valid ranges: negative (≤ -1), 0, positive (≥ 1)

    # Household Wealth Incl. Home Equity = wealth [S117]
    # Same ranges and topcodes as [S116]; includes home equity

    # Farm/Business Value = farm_bus [S103]
    # 1984–2005: top = 999,999,998
    # 2007–2009: top = 999,999,996
    # 2011–2021: top = 999,999,997
    # Valid: neg (≤ -1), 0, pos (≥ 1)

    # Checking/Saving Value = checking [S105]
    # 1984–2003: top = 999,999,998
    # 2005: includes valid negatives; top = 999,999,999
    # 2007–2009: top = 999,999,996
    # 2011–2017: top = 999,999,997
    # 0 = no assets

    # Other Real Estate Value = real_estate [S109]
    # Same coding as [S116]; top = 998/996/997 by year

    # Stocks Value = stocks [S111]
    # Same pattern as [S116]; top = 998/996/997 by year

    # Vehicle Values = vehicles [S113]
    # Same as [S116]; neg, 0, pos; top = 998/996/997 by wave

    # Other Assets = other_assets [S115]
    # Same topcoding pattern as [S116]; neg, 0, pos; top = 998/996/997

    # Other Debt = other_debt [S107]
    # 1984–2005: top = 999,999,999
    # 2007–2009: top = 999,999,997
    # 0 = no other debts

    # Student Loans = student_loans [ER48945]
    # 2011–2021: 1–9,999,997 = valid; 0 = none

money_vars <- c("income_all", "income_tax_hs", "income_tax_others",
  "transfer_hs", "transfer_others1", "transfer_others2", "wealth_nohouse", "wealth", "farm_bus", "checking", "debt",
  "real_estate", "stocks", "vehicles", "other_assets",
  "student_loans", "home_equity")

# Join inflation, adjust, and rename
sample <- sample %>%
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

sample <- sample %>%
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

sample <- sample %>% # Recode marital status
  mutate(
    married =
      case_when(
        marital > 1 & marital < 9 ~ 0,
        marital == 9 ~ NA_real_,
        TRUE ~ 1
      )
  )

# Clean State -------------------------------------------------------------
  # 1-51 PSID state code
  # 0 Inap. outside U.S. proper (from 1970)
  # 99 DK, NA (from 1972)

sample <- sample %>%
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
sample <- sample %>%
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

sample <- sample %>%
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

sample <- sample %>%
  mutate(
    max_race = pmax(
      !!!select(., contains("race_")),
      na.rm = TRUE
    ),
    max_race = if_else(max_race == -Inf, NA_real_, max_race)
  )

# According to codebook mention order doesn't matter. Black if black exists, then other then white -Sayer, Cohen, and Casper (2004)
sample <- sample %>%
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
sample_fast <- sample %>%                       
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

sample <- left_join(sample, sample_fast, by = c("pid", "year"))

# Uses a Summary + Join Approach to Improve Speed / Memory Usage
pid_lookup <- sample %>%                                
  summarise(                                        
    max_max_race = efficient_max(max_person_race, na.rm = TRUE),
    .by = pid
  )

sample <- sample %>%
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

# CLEAN INDIVIDUAL DEMOGRAPHICS -------------------------------------------------
sample <- sample %>%
  mutate(
    male =
      case_when(
        male == 9 ~ NA_real_,
        male == 2 ~ 0,
        TRUE ~ male
      )
  )

# Clean Year of Death ----------------------------------------------------------
  # When available, the exact year of death is recorded.
  # When a range of years was reported, this variable contains
  # a four digit code in which the first two digits represent the first
  # possible year of death, and the last two digits represent the last
  # possible year.
sample <- sample %>%
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

# CLEAN INDIVIDUAL EDUCATION ----------------------------------------------------
  # After cleaning:
    # edu_cat1: Education categories: 0 = High school, 1 = Post-high school education
    # edu_cat2: Education categories: 0 = High school, 1 = Some post-high school, 2 = Bachelor's or higher.
    # hs, ba, ma: Binary variables for high school, bachelor's, and master's degrees

sample <- sample %>%
  mutate(
    edu = if_else(edu %in% c(97, 98, 99), NA_real_, as.double(edu))
  )

  sample <- sample %>%
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
sample <- sample %>%
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
sample <- sample %>%
  mutate(
    children = if_else(children %in% c(98, 99), NA_real_, as.double(children))
  )

saveRDS(sample, here("2_clean_panel", "output", "clean.rds"))