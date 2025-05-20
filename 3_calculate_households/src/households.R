# INPUTS: 2_clean_panel/output/clean.rds
# OUTPUTS: 3_calculate_households/output/households.rds

# LOAD DATA ------------------------------------------------------------------
clean <- readRDS(here("2_clean_panel", "output", "clean.rds"))
data <- clean

# RESHAPE DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS ------------------------------------------------------------------
data <- data %>%
  mutate(role_tag = case_when(
    relation == 1 ~ "head",
    relation == 2 ~ "spouse",
    relation == 3 ~ "child",
    relation == 4 ~ "other",
    relation == 0 ~ "unknown",
    TRUE ~ "unknown"
  )) %>%
  group_by(fam_id, year, role_tag) %>%
  mutate(role_num = row_number(),
         role_id = paste0(role_tag, "_", role_num)) %>%
  ungroup() %>%
  filter(relation != 0) %>%
  select(-role_tag, -role_num, -sequence, -relation) 

# Format as data.table and separate family and individual data
dt <- as.data.table(data)

id_vars <- c("year", "fam_id")  
fam_vars <- c(
  "ind_top_inc_all", "ind_top_inc_tax_hs", "ind_top_inc_tax_o",
  "ind_top_inc_trans_hs", "ind_top_inc_trans_o1", "ind_top_inc_trans_o2",
  "ind_top_wealth_nohouse", "ind_top_wealth", "ind_top_wealth_farmbus",
  "ind_top_wealth_checking", "ind_top_wealth_debt", "ind_top_wealth_re",
  "ind_top_wealth_stocks", "ind_top_wealth_vehicles", "ind_top_wealth_other",
  "ind_top_wealth_home", "ind_top_student_loans",
  "inc_all", "inc_tax_hs", "inc_tax_o", "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_farmbus", "wealth_checking", "wealth_debt",
  "wealth_re", "wealth_stocks", "wealth_vehicles", "wealth_other", "wealth_home",
  "student_loans", "race_year", "race", "black", "release", "state", "numfu"
)

ind_vars <- c(
  "id1968", "married_pair", "own_home", "age", "male", "sample", "children", "stratum",
  "cluster", "parent1", "parent2", "grandparent1", "grandparent2", "grandparent3",
  "grandparent4", "yob", "birth_cohort", "yod", "edu_cat2", "edu_cat1", "hs", "ba", 
  "ma", "last_weight", "last_weight_year"
)

# Create family-level data
dt_fam <- data[, c("year", "fam_id", "pid", fam_vars), with = FALSE]

dt_fam <- dt_fam %>%
  select(-c(pid))

fam_wide <- dt_fam %>%
  group_by(year, fam_id) %>%
  slice(1) %>%
  ungroup()

rm(dt_fam)

# Create individual-level data
long_dt <- melt(dt, 
                id.vars = c(id_vars, "pid", "role_id"), 
                measure.vars = ind_vars,
                variable.name = "var")

# Create role tag for pivot to wide
long_dt[, var_role := paste0(var, "_", role_id)]

# Cast to wide
ind_wide <- dcast(
  long_dt,
  year + fam_id ~ var_role,
  value.var = "value"
)

# Remove unnecessary columns
ind_wide <- ind_wide %>%
  select(
    -ends_with("spouse_2"),
    -ends_with("head_2"),
    -ends_with("head_3"),
    -starts_with("children_child"),
    -starts_with("children_other"),
    -starts_with("cluster_child"),
    -starts_with("cluster_other"),
    -starts_with("cluster_spouse"),
    -starts_with("fam_id_other"),
    -starts_with("fam_id_spouse"),
    -starts_with("fam_id_child"),
    -starts_with("grandparent1_child"),
    -starts_with("grandparent1_other"),
    -starts_with("grandparent2_child"),
    -starts_with("grandparent2_other"),
    -starts_with("grandparent3_child"),
    -starts_with("grandparent3_other"),
    -starts_with("grandparent4_child"),
    -starts_with("grandparent5_other"),
    -starts_with("parent1_child"),
    -starts_with("parent1_other"),
    -starts_with("parent2_child"),
    -starts_with("parent2_other"),
    -starts_with("id1968_child"),
    -starts_with("id1968_other"),
    -starts_with("last_weight_child"),
    -starts_with("last_weight_other"),
    -starts_with("last_weight_spouse"),
    -starts_with("married_pair_child"),
    -starts_with("married_pair_other"),
    -starts_with("married_pair_spouse"),
    -starts_with("own_home_child"),
    -starts_with("own_home_other"),
    -starts_with("own_home_spouse"),
    -starts_with("stratum_child"),
    -starts_with("stratum_other"),
    -starts_with("stratum_spouse")
  ) %>%
  rename(
    id1968 = id1968_head_1,
    id1968_spouse = id1968_spouse_1,
    married_pair = married_pair_head_1,
    own_home = own_home_head_1,
    stratum = stratum_head_1,
    cluster = cluster_head_1,
    last_weight = last_weight_head_1,
    last_weight_year = last_weight_year_head_1,
  )

# Merge family and individual data
households <- merge(
  fam_wide,
  ind_wide,
  by = c("year", "fam_id"),
  all.x = TRUE
)

households <- households %>%
  select(
    year, fam_id, id1968, id1968_spouse, release, 
    state, numfu, own_home, married_pair, race, black, race_year, 
    stratum, cluster, last_weight, last_weight_year,
    
    # Income variables
    starts_with("inc_"),
    
    # Wealth variables
    starts_with("wealth_"),
    starts_with("student_loans"),
    
    # Head and Spouse variables
    ends_with("_head_1"), 
    ends_with("_spouse_1"),

    # Topcode variables
    starts_with("ind_top_"),

    # Children and Other (1-12)
    matches("_child_([1-9]|1[0-2])$"),
    matches("_other_([1-9]|1[0-2])$")
  )

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("3_calculate_households", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(households, here("3_calculate_households", "output", "households.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(clean, data, dt, fam_wide, ind_wide, long_dt)
