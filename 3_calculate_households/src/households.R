# INPUTS: 2_clean_panel/output/clean.rds
# OUTPUTS: 3_calculate_households/output/households.rds

# LOAD DATA ------------------------------------------------------------------
clean <- readRDS(here("2_clean_panel", "output", "clean.rds"))

clean <- clean %>%
  select(-c(married, married_pair))

# Format as data table for speed
data <- as.data.table(clean)

# RESHAPE INDIVIDUAL DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS ------------------------------------------------------------------
# Limit to heads and spouses
ind <- data[relation %in% c(1, 2)]  # head = 1, spouse = 2

# Identify which columns are time-varying (these should be the ind cols)
varying <- ind[, lapply(.SD, function(x) length(unique(x)) > 1), by = .(year, fam_id)]
variation_summary <- varying[, lapply(.SD, any), .SDcols = !c("year", "fam_id")]

ind_cols <- names(variation_summary)[unlist(variation_summary)]
fam_cols <- names(variation_summary)[!unlist(variation_summary)]
fam_cols <- union(fam_cols, "id1968") # add manually
id_cols <- c("year", "fam_id", "pid")
rm(varying, variation_summary)

ind <- ind %>%
  mutate(role_tag = case_when(
    relation == 1 ~ "head",
    relation == 2 ~ "spouse",
    TRUE ~ NA_character_
  ))

ind <- ind[, c(id_cols, "role_tag", ind_cols), with = FALSE]

ind_long <- melt(ind,
  id.vars = c(id_cols, "role_tag"),
  measure.vars = ind_cols,
  variable.name = "variable",
  value.name = "value"
)

ind_long <- as_tibble(ind_long)

# Widen data using pivot_wider
dt_wide <- pivot_wider(
  ind_long,
  id_cols = c(year, fam_id, pid),  
  names_from = c(variable, role_tag),
  values_from = value,
  names_sep = "_"
)

ind_wide <- dt_wide %>%
  group_by(year, fam_id) %>%
  summarise(across(everything(), ~ .x[!is.na(.x)][1]), .groups = "drop")

ind_wide <- ind_wide %>%
 rename(
   sex_head = male_head,
   sex_spouse = male_spouse,
  ) %>%
  select(-c(relation_head, relation_spouse))


# RESHAPE FAMILY DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS ------------------------------------------------------------------
fam <- data[, c("year", "fam_id", "pid", fam_cols), with = FALSE]

fam <- fam %>%
  select(-c(pid))

fam_wide <- fam %>%
  group_by(year, fam_id) %>%
  slice(1) %>%
  ungroup()

# MERGE WIDE INDIVIDUAL AND FAMILY DATA ------------------------------------------------------------------
households <- fam_wide %>%
  left_join(ind_wide, by = c("year", "fam_id"))

households <- households %>%
  select(-c(pid))

households <- households %>%
  select(
    year, fam_id, id1968,
    release, stratum, cluster, state, numfu, own_home,
    
    # Income variables
    starts_with("inc_"),
    
    # Wealth variables
    starts_with("wealth_"),
    starts_with("student_loans"),
    
    # Head and Spouse variables
    ends_with("_head"), 
    ends_with("_spouse"),

    # Topcode variables
    starts_with("ind_top_")
  )

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("3_calculate_households", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(households, here("3_calculate_households", "output", "households.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(clean, data, dt_wide, fam, fam_wide, ind, ind_wide, ind_long)
