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
  select(-starts_with("ind_top_"))

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









