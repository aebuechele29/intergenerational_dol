# INPUTS: 2_clean_panel/output/clean.rds
# OUTPUTS: 3_create_households/output/households.rds

# LOAD DATA ------------------------------------------------------------------
clean <- readRDS(here("2_clean_panel", "output", "clean.rds"))
data <- clean

data <- data %>%
  group_by(pid) %>%
  mutate(
    sex = efficient_max(sex),
    births = efficient_max(births, na.rm = TRUE),
  ) %>%
  ungroup()

data <- data %>%
  group_by(pid) %>%
  mutate(
    parent1 = unique(na.omit(parent1))[1],
    parent2 = unique(na.omit(parent2))[1]
  ) %>%
  ungroup()

# RESHAPE DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS ------------------------------------------------------------------
# Format as data.table and separate family and individual data
dt <- as.data.table(data)

id_vars <- c("year", "fam_id")  

fam_vars <- c(
  "age_head", "age_spouse", "yob_head", "yob_spouse", "age_diff", 
  "marital_head", "num_fam", "own_home", "fam_inc", "region", "state",
  "race", "black", 
  "cc_head", "cc_spouse", "hw_head", "hw_spouse",
  "hrly_head", "hrly_spouse", "work_head", "work_spouse", "earn_head", "earn_spouse"
)

ind_vars <- c(
  "id1968", "pid", "parent1", "parent2", "grandparent1", "grandparent2", "grandparent3", "grandparent4",
  "child_under5", "births", "sex", "edu_cat", "relation"
)

dt <- dt %>%
  mutate(across(
    c(id1968, pid, parent1, parent2, grandparent1, grandparent2, grandparent3, grandparent4),
    as.character
  )) %>%
  mutate(across(
    c(child_under5, births, sex, edu_cat, relation),
    as.numeric
  )) 

# Create family-level data
dt_fam <- dt[, c("year", "fam_id", "pid", fam_vars), with = FALSE]

fam_wide <- dt_fam %>%
  group_by(year, fam_id) %>%
  slice(1) %>%
  ungroup()

rm(dt_fam)

# Create individual-level data
# Filter and tag head/spouse only
data_filtered <- dt %>%
  filter(relation %in% c(1, 2)) %>%
  mutate(role_tag = case_when(
    relation == 1 ~ "head",
    relation == 2 ~ "spouse"
  ))

# Melt to long by individual variables
long_dt <- melt(
  data_filtered,
  id.vars = c("fam_id", "year", "pid", "role_tag"),
  measure.vars = ind_vars,
  variable.name = "var"
)

# Create a variable name combining var and role_tag (e.g., edu_head, edu_spouse)
long_dt[, var_role := paste0(var, "_", role_tag)]

# Cast to wide format: one row per family-year
ind_wide <- dcast(
  long_dt,
  fam_id + year ~ var_role,
  value.var = "value",
  fun.aggregate = function(x) x[1]
)

# Merge family and individual data
households <- merge(
  fam_wide,
  ind_wide,
  by = c("year", "fam_id"),
  all.x = TRUE
)

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("3_create_households", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(households, here("3_create_households", "output", "households.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(clean, data, dt, fam_wide, ind_wide, long_dt)
