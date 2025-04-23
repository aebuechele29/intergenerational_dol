# Load merged data
sample <- readRDS(here("1_build_panel", "output", "merged.rds"))

max_year <- max(sample$year)

sample <- sample %>% # Limit sample to SEO and SRO
  mutate(
    age = replace(age, age == 999, NA), # Change weird age code
    # sequence = case_when(
    # (relation != 0 & year == 1968) ~ 22, # Sequence NA in 1968
    # TRUE ~ sequence # Use the renamed `sequence` variable
    # ),
    birth = round(year - age) # Create Birth Year
  ) # %>%
# filter(sequence != 0 &
# (sequence < 71 | sequence > 89))

# Capture year censored
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    censored_year = max(year[year < max_year], na.rm = TRUE)
  ) %>%
  ungroup()

# Choose person median birth year
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    birth = as.integer((median(birth, na.rm = TRUE)))
  ) %>%
  ungroup()

# Recode Relation ---------------------------------------------------------
# 1968 1 head 2 wife/ spouse 3, child 4-7 other 8 spouse 9/ 0 NA
# 1969-1982 8 other 9 spouse 0 NA
# 1983-1999 10 head 20 22 88 90 spouse 30-38 83 child 40-75 95-98 other 0 NA
# 2017 92 spouse

# 1968-1982 4 = sibling of head; 5 = parent of head; 7 = other adult relative
# 1983-  40, 47, 48 = sibling; 50, 57, 58 = parent; 
# 60, 65 grand/ great-grandson/ daughter
# 66, 67, 68, 69 = grand/ great-grandparent;
# 70, 71 = nephew; 72, 73 = uncle/ aunt; 74, 75 = cousin; 95, 96, 97 = other

sample <- sample %>%
  mutate(
    # Creating variable to later calculate co-residence with family members over 18
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
  mutate(ever_child = max(child)) %>%
  ungroup() %>%
  select(-child)

# Remove heads of households who are in institutions (251 of them; created bugs)
# THIS LIMITS THE SAMPLE MORE THAN IT SHOULD (~3 MILLION PERSON YEARS TO <1 MILLION)
# sample <- sample %>%
# filter(relation != 1 |
# (relation == 1 & (sequence != 51 &
# sequence != 52)))


# Clean Education ---------------------------------------------------------
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
    max_edu2 = max(edu2, na.rm = TRUE),
    max_edu3 = max(edu3, na.rm = TRUE),
    max_hs = max(hs, na.rm = TRUE),
    max_ba = max(ba, na.rm = TRUE),
    max_ma = max(ma, na.rm = TRUE)
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
  rename(edu = max_edu3, edu2 = max_edu2, hs = max_hs, ba = max_ba, ma = max_ma)

# Clean Gender ------------------------------------------------------------
sample <- sample %>%
  mutate(
    male =
      case_when(
        male == 9 ~ NA_real_,
        male == 2 ~ 0,
        TRUE ~ male
      )
  )

# Clean Income ------------------------------------------------------------
# Income top coded at 999,999; from 1972 1 = 1 or less;
# 1980 top coded at 999,999;
# 1981 - 1983 top coded at 9,999,999; 1984-1985 top coded 999,999;
# 1986-1993 top 9,999,999
# 1994-1995 started including negatives and 9,999,999 all of a
# sudden means latino sample (which we don't have)
# 1996-1997 drop latino thing (no more 9,999,999);
# 1999 - 2019 bring back top coding of 9,999,999
sample <- sample %>% # Income only for heads and spouses
  mutate(
    parental_income =
      case_when(
        relation == 3 ~ income,
        TRUE ~ NA_real_
      ),
    income =
      case_when(
        relation == 1 | relation == 2 ~ income,
        TRUE ~ NA_real_
      )
  )

# Average all household income observations
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    income_all = mean(income, na.rm = TRUE)
  ) %>%
  ungroup()

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

# Clean Number of Children (# Live Births)   ----------------------------
# Children: 98 NA/ DK 99 No birth history collected
sample <- sample %>%
  mutate(
    children =
      case_when(
        children == 98 |
          children == 99 ~ NA_real_,
        TRUE ~ children
      )
  )

# Clean First Child and Marriage ----------------------------------------
# First child: 1900-2019 actual year, 9999 = missing or incomplete

# Recode first_child and first_marriage to integer first, then clean
sample <- sample %>%
  mutate(
    first_child = as.integer(first_child),
    first_marriage = as.integer(first_marriage)
  ) 

sample <- sample %>%
  mutate(
    first_child =
      case_when(
        first_child == 9999 ~ NA_real_,
        TRUE ~ first_child - birth
      ),
    teen_pregnant =
      case_when(
        is.na(first_child) ~ NA_real_,
        first_child < 20 ~ 1,
        TRUE ~ 0
      )
  )

# First marriage: 1900-2019 actual year, 9998 = NA/ DK, 9999 = not collected
sample <- sample %>%
  mutate(
    first_marriage =
      case_when(
        (first_marriage == 9998 |
           first_marriage == 9999) ~ NA_real_,
        TRUE ~ first_marriage
      )
  ) %>%
  mutate(
    first_marriage = first_marriage - birth,
    married_before26 = case_when(
      first_marriage < 26 ~ 1,
      first_marriage > 25 ~ 0,
      is.na(first_marriage) ~ NA_real_
    )
  )

# Clean Marriage --------------------------------------------
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

# Clean Race --------------------------------------------------------------
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

# According to codebook mention order doesn't matter
# Black if black exists, then other then white -
# Sayer, Cohen, and Casper (2004)
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    race_year = case_when(
      !is.infinite(max_race) & !is.na(max_race) ~ as.double(year),
      TRUE ~ 0
    ),
    max_race_year = max(race_year, na.rm = TRUE),
    # Determine race_pid based on max_race_year
    race_pid = case_when(
      max_race_year == year ~ max_race,
      TRUE ~ NA_real_
    ),
    race = max(race_pid, na.rm = TRUE)
  ) %>%
  mutate(race = na_if(race, -Inf)) %>%
  ungroup()

# Handle race for each pid/year combination
sample <- sample %>%
  group_by(pid, year) %>%
  mutate(
    # Determine head_race for relation == 1 (head of household)
    head_race = case_when(
      relation == 1 ~ race,
      TRUE ~ NA_real_
    ),
    # Determine other_race for non-head of household
    other_race = case_when(
      relation != 1 ~ race,
      TRUE ~ NA_real_
    ),
    # Get max values for head_race and other_race
    max_head_race = max(head_race, na.rm = TRUE),
    max_other_race = max(other_race, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Combine the two max_race values into max_person_race
    max_person_race = case_when(
      !is.infinite(max_head_race) & !is.na(max_head_race) ~ max_head_race,
      !is.infinite(max_other_race) & !is.na(max_other_race) ~ max_other_race,
      TRUE ~ NA_real_
    )
  )

# Final race assignment and cleanup
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    max_max_race = max(max_person_race, na.rm = TRUE),
    race = case_when(
      is.na(race) & !is.infinite(max_max_race) ~ max_max_race,
      is.na(race) & is.infinite(max_max_race) ~ NA_real_,
      TRUE ~ race
    )
  ) %>%
  ungroup() %>%
  select(-contains("max"), -contains("race_"), -contains("head"), -other_race)

# Recode black as 1 if race == 3, 0 if race is 1 or 2, and leave other values as race
sample <- sample %>%
  mutate(
    black = case_when(
      race == 3 ~ 1,
      race %in% c(1, 2) ~ 0,
      TRUE ~ race
    )
  )

# Create Sibling Vars   ------------------------------------------------------

# Count number of people by year, family, and relation
sample <- sample %>%
  add_count(year, fam_id, relation, name = "n")  # 'n' is now a column

# Count siblings
sample <- sample %>%
  mutate(
    y_n_sibs = case_when(
      relation == 3 ~ n - 1,
      TRUE ~ NA_real_
    )
  )

# Count individual max # of siblings over time
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    n_sibs = max(y_n_sibs, na.rm = TRUE),
    n_sibs = ifelse(is.infinite(n_sibs), NA_real_, n_sibs)
  ) %>%
  ungroup()

# Arrange by birth order
sample <- sample %>%
  mutate(one = 1) %>%
  group_by(year, fam_id, relation) %>%
  arrange(year, fam_id, relation, birth)

# Assign sibling rank w/in family 
sample <- sample %>%
  mutate(
    y_sib_rank = case_when(
      relation == 3 ~ cumsum(one),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Calculate number of children and adjust rank
sample <- sample %>%
  mutate(
    y_sib_rank = ifelse(n_sibs == 0, 0, y_sib_rank),
    n_children = n_sibs + 1
  )

# Save sibling rank
sample <- sample %>%
  group_by(pid) %>%
  mutate(
    sib_rank = max(y_sib_rank, na.rm = TRUE),
    sib_rank = ifelse(is.infinite(sib_rank), NA_real_, sib_rank)
  ) %>%
  ungroup()

sample <- sample %>%
  select(-c(y_sib_rank, one, y_n_sibs, n))