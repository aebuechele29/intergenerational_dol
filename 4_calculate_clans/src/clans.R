# INPUTS: 3_calculate_households/output/households.rds
# OUTPUTS: 

households <- readRDS(here("3_calculate_households", "output", "households.rds"))

# Add number of households within each clan by year
households <- households %>%
  group_by(year, id1968) %>%
  mutate(numclan = n()) %>%
  ungroup()

# Assign a clan number to each household per year
households <- households %>%
  group_by(id1968, year) %>%
  arrange(fam_id) %>%
  mutate(hh_number = row_number()) %>%
  ungroup() %>%
  # Deselecting variables that need to be taken care of in the household file, not clan
  select(-c(release, stratum, cluster, sequence_head, sequence_spouse, 
    last_weight_head, last_weight_spouse, last_weight_year_head, 
    last_weight_year_spouse, race_year_head, race_year_spouse))

# Removing topcodes for now 050517
households <- households %>%
  select(-starts_with("topcode"))

cols_to_pivot <- setdiff(names(households), c("id1968", "year", "hh_number", "numclan"))

clans <- households %>%
  pivot_wider(
    id_cols = c(id1968, year),
    names_from = hh_number,
    values_from = all_of(cols_to_pivot)
  )

# CLEAN/VALIDATE CLAN IDENTIFIERS
# id1968: Individual identifier anchored in 1968
if (nrow(distinct(households, id1968, year)) != nrow(clans)) {
  stop("Different number of distinct clans in household and clan file. Check merge.")
}

# year: Year of observation
if (nrow(clans) != nrow(distinct(clans, id1968, year))) {
  stop("Duplicate (id1968, year) combinations found in clans. Check merge.")
}

# numclan: Number of households in each clan



# CLEAN SAMPLE VARIABLES - REMOVED FOR NOW
# release_1` to `release_12: Sample release indicators per family unit
# stratum_1` to `stratum_12: Stratification variables for survey design
# cluster_1` to `cluster_12: Clustering variables for variance estimation
# weight_1` to `weight_12: Sampling weights for each family unitclans <- clans %>%

# CLEAN INCOME AND WEALTH VARIABLES
# Function to count and sum income and wealth variables
count_and_sum_safe <- function(df, prefix) {
  cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  mat <- df[cols]
  count <- rowSums(!is.na(mat))
  total <- pmap_dbl(as.list(as.data.frame(t(mat))), efficient_sum)
  list(count = count, total = total)
}

# Apply function to income variables
inc_all <- count_and_sum(clans, "inc_all_")
inc_tax_hs <- count_and_sum(clans, "inc_tax_hs_")
inc_tax_o <- count_and_sum(clans, "inc_tax_o_")
inc_trans_hs <- count_and_sum(clans, "inc_trans_hs_")
inc_trans_o1 <- count_and_sum(clans, "inc_trans_o1_")
inc_trans_o2 <- count_and_sum(clans, "inc_trans_o2_")

# Add income variables to clans
clans <- clans %>%
  mutate(
    inc_all_count_clan = inc_all$count,
    inc_all_clan = inc_all$total,
    inc_tax_hs_count_clan = inc_tax_hs$count,
    inc_tax_hs_clan = inc_tax_hs$total,
    inc_tax_o_count_clan = inc_tax_o$count,
    inc_tax_o_clan = inc_tax_o$total,
    inc_trans_hs_count_clan = inc_trans_hs$count,
    inc_trans_hs_clan = inc_trans_hs$total,
    inc_trans_o1_count_clan = inc_trans_o1$count,
    inc_trans_o1_clan = inc_trans_o1$total,
    inc_trans_o2_count_clan = inc_trans_o2$count,
    inc_trans_o2_clan = inc_trans_o2$total
  )  %>%
  select(
    -matches("^inc_all_\\d+$"),
    -matches("^inc_tax_hs_\\d+$"),
    -matches("^inc_tax_o_\\d+$"),
    -matches("^inc_trans_hs_\\d+$"),
    -matches("^inc_trans_o1_\\d+$"),
    -matches("^inc_trans_o2_\\d+$")
  )

# Apply function to wealth variables
wealth_nohouse <- count_and_sum_safe(clans, "wealth_nohouse_")
wealth_farmbus <- count_and_sum_safe(clans, "wealth_farmbus_")
wealth_checking <- count_and_sum_safe(clans, "wealth_checking_")
wealth_debt <- count_and_sum_safe(clans, "wealth_debt_")
wealth_re <- count_and_sum_safe(clans, "wealth_re_")
wealth_stocks <- count_and_sum_safe(clans, "wealth_stocks_")
wealth_vehicles <- count_and_sum_safe(clans, "wealth_vehicles_")
wealth_other <- count_and_sum_safe(clans, "wealth_other_")
wealth_home <- count_and_sum_safe(clans, "wealth_home_")
student_loans <- count_and_sum_safe(clans, "student_loans_")

# Add wealth variables to clans
clans <- clans %>%
  mutate(
    wealth_nohouse_count_clan = wealth_nohouse$count,
    wealth_nohouse_clan = wealth_nohouse$total,
    wealth_farmbus_count_clan = wealth_farmbus$count,
    wealth_farmbus_clan = wealth_farmbus$total,
    wealth_checking_count_clan = wealth_checking$count,
    wealth_checking_clan = wealth_checking$total,
    wealth_debt_count_clan = wealth_debt$count,
    wealth_debt_clan = wealth_debt$total,
    wealth_re_count_clan = wealth_re$count,
    wealth_re_clan = wealth_re$total,
    wealth_stocks_count_clan = wealth_stocks$count,
    wealth_stocks_clan = wealth_stocks$total,
    wealth_vehicles_count_clan = wealth_vehicles$count,
    wealth_vehicles_clan = wealth_vehicles$total,
    wealth_other_count_clan = wealth_other$count,
    wealth_other_clan = wealth_other$total,
    wealth_home_count_clan = wealth_home$count,
    wealth_home_clan = wealth_home$total,
    student_loans_count_clan = student_loans$count,
    student_loans_clan = student_loans$total
  ) %>%
  select(
    -matches("^wealth_nohouse_\\d+$"),
    -matches("^wealth_farmbus_\\d+$"),
    -matches("^wealth_checking_\\d+$"),
    -matches("^wealth_debt_\\d+$"),
    -matches("^wealth_re_\\d+$"),
    -matches("^wealth_stocks_\\d+$"),
    -matches("^wealth_vehicles_\\d+$"),
    -matches("^wealth_other_\\d+$"),
    -matches("^wealth_home_\\d+$"),
    -matches("^student_loans_\\d+$")
  )

# CLEAN HOUSEHOLD IDENTIFIERS
# fam_id_1` to `fam_id_12: Family identifiers across 12 generations or waves
# pid_head_1` to `pid_head_12: Personal ID of head
# id1968_head_1` to `id1968_head_12: 1968 ID of household head
# sequence_head_1` to `sequence_head_12: Sequence variable of head

# CLEAN HOUSEHOLD CHARACTERISTICS
# sex_head_1` to `sex_head_12: Sex of household head
# race_head_1 to `race_head_12: Race of household head
# edu_cat1_head_1 to `edu_cat1_head_12: Education level of household head
# hs_head_1` to `hs_head_12: High school completion of household head
# bs_head_1` to `bs_head_12: Bachelor's degree completion of household head
# ms_head_1` to `ms_head_12: Master's degree completion of household head
# own_home_1` to `own_home_12: Homeownership status
# state_1` to `state_12: State of residence for each wave
# numfu_1` to `numfu_12: Number of family units in household
# yob_head_1` to `yob_head_12: Year of birth of household head

# CLEAN HOUSEHOLD FAMILY LINKAGES
# parent1_head_1` to `parent1_head_12: ID of first parent
# parent2_head_1` to `parent2_head_12: ID of second parent
# grandparent1_head_1` to `grandparent1_head_12: ID of grandparent (line 1)
# grandparent2_head_1` to `grandparent2_head_12: ID of grandparent (line 2)

# Check that clan ids are shared between heads and spouses
# Save mismatched cases
  # mismatched <- households %>%
    # filter(
      # !is.na(id1968_head) & 
      # !is.na(id1968_spouse) & 
      # id1968_head != id1968_spouse
    # ) %>%
  # select(year, fam_id, pid_head, pid_spouse, id1968_head, id1968_spouse)

# Remove mismatched from the main dataset
  # households <- households %>%
  # filter(
    # is.na(id1968_head) | 
    # is.na(id1968_spouse) | 
    # id1968_head == id1968_spouse
  # )





