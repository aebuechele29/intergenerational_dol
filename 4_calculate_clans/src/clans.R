# INPUTS: 3_calculate_households/output/households.rds
# OUTPUTS: 4_calculate_clans/output/clans.rds

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
  ungroup() 

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


# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("4_calculate_clans", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(clans, here("4_calculate_clans", "output", "clans.rds"))





