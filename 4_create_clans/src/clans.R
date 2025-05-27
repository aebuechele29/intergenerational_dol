# INPUTS: 3_calculate_households/output/households.rds
# OUTPUTS: 4_create_clans/output/clans.rds

households <- readRDS(here("3_create_households", "output", "households.rds"))

# ADD CLAN DATA TO HOUSEHOLDS ------------------------------------------------------------------
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
clans <- clans %>%
  select(-any_of(grep("^(ind_top|last_weight|stratum|cluster|release)", names(clans), value = TRUE)))

# CLEAN HOUSEHOLD DATA BEFORE RESHAPING TO CLANS -----------------------------------
# Check that clan ids are shared between heads and spouses
# Save mismatched cases
  mismatched <- households %>%
    filter(
      !is.na(id1968) & 
      !is.na(id1968_spouse) & 
      id1968 != id1968_spouse
    ) 
  
  saveRDS(mismatched, here("4_create_clans", "output", "mismatched.rds"))

# Remove mismatched from the main dataset
  households <- households %>%
    filter(
      is.na(id1968) | 
       is.na(id1968_spouse) | 
       id1968 == id1968_spouse
   )

# Remove parent and grandparent variables for non-mismatched households
households <- households %>%
  select(
    -starts_with("parent1_"),
    -starts_with("parent2_"),
    -starts_with("grandparent1_"),
    -starts_with("grandparent2_"),
    -starts_with("grandparent3_"),
    -starts_with("grandparent4_")
  )

# Create household-level proportional variables
binary_vars <- c("male_", "edu_cat1_", "edu_cat2_", "hs_", "ba_", "ma_")
yob_vars <- "yob_"

get_vars_by_prefix <- function(prefix) {
  grep(paste0("^", prefix), names(df), value = TRUE)
}

all_binary_vars <- unlist(lapply(binary_vars, get_vars_by_prefix))
all_yob_vars <- get_vars_by_prefix(yob_vars)

households <- households %>%
    mutate(
        prop_male = rowSums(across(starts_with("male_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        prop_edu_cat1 = rowSums(across(starts_with("edu_cat1_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        prop_edu_cat2 = rowSums(across(starts_with("edu_cat2_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        prop_hs = rowSums(across(starts_with("hs_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        prop_ba = rowSums(across(starts_with("ba_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        prop_ma = rowSums(across(starts_with("ma_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        mean_yob = rowMeans(across(starts_with("yob_"), ~ as.numeric(.)), na.rm = TRUE)
    )

households <- households %>%
  select(-matches("(_head$|_spouse$|child_\\d+$|other_\\d+$)"))

# CREATE CLANS
cols_to_pivot <- setdiff(names(households), c("id1968", "year", "hh_number", "numclan"))

clans <- households %>%
  pivot_wider(
    id_cols = c(id1968, year, numclan),
    names_from = hh_number,
    values_from = setdiff(names(households), c("id1968", "year", "numclan", "hh_number"))
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
file.remove(list.files(here("4_create_clans", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(clans, here("4_create_clans", "output", "clans.rds"))







