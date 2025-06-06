# INPUTS: 0_data/psid.xlsx, 0_data/fims/, 0_data/fam_ind/
# OUTPUTS: Data frames for family data and indiviudal data, including parent and grandparent IDs from FIMs

setwd("/Users/amanda/Desktop/categorical_final")

# Load required libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")

pacman::p_load(
  tidyverse,
  dplyr,
  purrr,
  data.table, 
  easyPSID,
  openxlsx,
  psidR,
  here,
  readxl,
  data.table,
  tableone, 
  knitr,
  nnet,
  modelsummary,
  kableExtra,
  marginaleffects,
  car
)

here::i_am("1_build_panel/src/build_panel.R")

# Load the CWF Finder File ------------------------------------------------
  # This file indexes variable names across years, provided by the PSID
cwf <- openxlsx::read.xlsx(here("0_data", "psid.xlsx"))

# Load Inflation Data -----------------------------------------------------
  # This file contains annual consumer price index (CPI) data for inflation adjustment from the Bureau of Labor Statistics
cpi <- read.xlsx(here("0_data", "cpi", "cpi.xlsx")) %>%
  as_tibble() %>%
  rename(inflation_value = Jan, year = Year) %>%
  select(year, inflation_value) %>%
  filter(year >= 1967)


# Load PSID Data ----------------------------------------------------------
  # This data is downloaded from the PSID website using the cross-year selection tool
file.remove(list.files(here("1_build_panel", "output"), pattern = "\\.rds$", full.names = TRUE)) # Output directory needs to be empty
input_dir <- here("0_data", "fam_ind")
output_dir <- here("1_build_panel", "output")

easyPSID::convert_to_rds(
  in_direc = input_dir,
  out_direc = output_dir
)

psid_file <- list.files(output_dir, pattern = "\\.rds$", full.names = TRUE)
psid_data <- readRDS(psid_file)

# DEFINE FUNCTIONS ---------------------------------------------------------------------------
# Define Variables to Extract Lists -----------------------------------------
extract_psid_vartables <- function(var_object) {
  tmp <- getNamesPSID(var_object[["var"]], cwf, years = NULL)
  
  if (var_object[["varname"]] == "wealth") {
    tmp <-
      tmp %>%
      mutate(
        variable =
          case_when(
            str_detect(variable, "ER") ~ variable,
            TRUE ~ NA_character_
          )
      )
  }
  
  if (grepl("male|first|yod", var_object[["varname"]])) {
    # Time-invariant so should appear every year
    tmp <-
      tmp %>%
      fill(variable, .direction = "up")
  }
  
  var_object[["df"]] <- tmp
  return(var_object)
}

# Define Function to Remove "Y" from Year Var Name ----------------------------
remove_y <- function(var_object) {
  var_object[["df"]] <-
    var_object[["df"]] %>%
    mutate(year = as.integer(str_sub(year, 2, 5)))
  
  return(var_object)
}

# Define Function to Rename Var df Variable Name -------------------------------
rename_variable <- function(var_object) {
  varname <- var_object[["varname"]]
  
  var_object[["df"]] <-
    var_object[["df"]] %>%
    rename({{ varname }} := variable)
  
  return(var_object)
}

# Define Function to Create Individual and Family Lists -------------------
create_sublist <- function(var_object, type_str) {
  if (str_detect(var_object[["name"]], type_str)) {
    return(var_object[["df"]])
  }
}

# Complete List of Variables ---------------------------------------------------
vars <- list(
# Family Demographics ------------------------------------------------------------------
  list(name = "fam_head_age", varname = "age_head", var = "V117"), # Age of head [all years]
  list(name = "fam_spouse_age", varname = "age_spouse", var = "V118"), # Age of spouse [all years]
  list(name = "fam_head_marital", varname = "marital_head", var = "V239"), # Marital status head, cohab as married [all years]
  list(name = "fam_total_num", varname = "num_fam", var = "V115"), # Total number of family members [all years]
  list(name = "fam_housing", varname = "housing", var = "V103"), # Housing status [all years]
  list(name = "fam_inc", varname = "fam_inc", var = "V81"), # Total family income [all years]
  list(name = "fam_region", varname = "region", var = "V361"), # Family region [all years]
  list(name = "fam_state", varname = "state", var = "V93"), # Family state [all years]
  
# FAMILY RACE -------------------------------------------------------------
  list(name = "fam_headrace", varname = "race1_head", var = "V181"), # Race of household head [1968-2021]
  list(name = "fam_head2race", varname = "race2_head", var = "V11939"), # Race of household head 2 [1985-2021]
  list(name = "fam_head3race", varname = "race3_head", var = "ER3946"), # Race of household head 3 [1994-2021]
  list(name = "fam_head4race", varname = "race4_head", var = "ER11851"), # Race of household head 4 [1997-2021]
  list(name = "fam_wiferace", varname = "race1_wife", var = "V12293"), # Race of household "wife" [1985-2021]
  list(name = "fam_wife2race", varname = "race2_wife", var = "V12294"), # Race of household "wife" 2 [1985-2021]
  list(name = "fam_wife3race", varname = "race3_wife", var = "ER3885"), # Race of household "wife" 3 [1994-2021]
  list(name = "fam_wife4race", varname = "race4_wife", var = "ER11763"), # Race of household "wife" 4 [1997-2021]

# FAMILY TIME USE -------------------------------------------------------------
  list(name = "fam_head_child", varname = "cc_head", var = "ER66718"), # Head childcare time [2017-2021]
  list(name = "fam_spouse_child", varname = "cc_spouse", var = "ER66731"), # Spouse childcare time [2017-2021]
  list(name = "fam_head_hw", varname = "hw_head", var = "V4609"), # Head housework time [1976-2021]
  list(name = "fam_spouse_hw", varname = "hw_spouse", var = "V4768"), # Spouse housework time [1976-2021]
  
  list(name = "fam_head_hrly", varname = "hrly_head", var = "V337"), # Head hourly earnings (per hour) [all years]
  list(name = "fam_spouse_hrly", varname = "hrly_spouse", var = "V338"), # Spouse hourly earnings (per hour) [all years]
  list(name = "fam_head_work", varname = "work_head", var = "V47"), # Annual head work time (hours) [all years]
  list(name = "fam_spouse_work", varname = "work_spouse", var = "V53"), # Annual spouse work time (hours) [all years]

# INDIVIDUAL VARIABLES ----------------------------------------------------
  list(name = "ind_birthfirst", varname = "first_birth", var = "ER32024"), # First birth [all years]
  list(name = "ind_birthlast", varname = "last_birth", var = "ER32026"), # Last birth = youngest child if 2+ children [all years]
  list(name = "ind_birthsecond", varname = "second_birth", var = "ER32028"), # Second youngest child if 3+ children [all years]
  list(name = "ind_birththird", varname = "third_birth", var = "ER32030"), # Third youngest child if 4+ children [all years]
  list(name = "ind_birthfourth", varname = "fourth_birth", var = "ER32032"), # Fourth youngest child if 5+ children [all years]
  list(name = "ind_total_births", varname = "births", var = "ER32022"), # Total number of births [all years]
  list(name = "ind_sex", varname = "sex", var = "ER32000"), # Sex [all years]
  list(name = "ind_edu", varname = "edu", var = "ER30010"), # Years of education [1968-2021]

# IDENTIFIERS -------------------------------------------------------------
  list(name = "ind_sequence", varname = "sequence", var = "ER30021"), # Interview sequence number [all years]
  list(name = "ind_sample", varname = "sample", var = "ER32006"), # Sample
  list(name = "ind_relation", varname = "relation", var = "ER30003"), # Individual relation to head [all years]
  list(name = "fam_num", varname = "fam_id", var = "V3") # Family Number [all years]

)

# Process Variables Using the CWF File ------------------------------------------
vartable_list <-
  vars %>%
  map(extract_psid_vartables) %>%
  map(remove_y) %>%
  map(rename_variable) 

all_vars <- vartable_list %>%
  map(~ list(
    ind = create_sublist(.x, "ind"),
    fam = create_sublist(.x, "fam")
  )) %>%
  map(compact) %>%            
  flatten() %>%               
  reduce(inner_join, by = "year") 

# BUILD PANEL ---------------------------------------------------------------------------
# Define Function to Build Individual and Family Panel ---------------------------
extract_psid_data <- function(psid_data, mapping, id_vars = c("ER30000", "ER30001", "ER30002")) {
  year_list <- unique(mapping$year)
  result_list <- purrr::map(year_list, function(yr) {
    map_row <- mapping %>% filter(year == yr)
    var_map <- map_row 
    varnames <- var_map %>% unlist() %>% na.omit()
    valid_varnames <- varnames[varnames %in% names(psid_data)]
    valid_newnames <- names(var_map)[!is.na(var_map)][varnames %in% names(psid_data)]
    
    if (length(valid_varnames) == 0) return(NULL)
    year_rows <- psid_data %>%
      filter(if_any(all_of(valid_varnames), ~ !is.na(.)))
    
    selected <- year_rows %>% select(all_of(id_vars), all_of(valid_varnames))
    names(selected) <- c(id_vars, valid_newnames)
    selected$year <- yr
    return(selected)
  })
  
  bind_rows(compact(result_list))
}

psid_data <- extract_psid_data(psid_data, all_vars)


# Clean Individual_data and Family_data -------------------------------------------
psid_data <- psid_data %>%
  rename(
    release = ER30000,
    id1968 = ER30001,
    pernum = ER30002
  )

# Add Unique PID to Individual and Family Data -----------------------------------
psid_data <- psid_data %>%
  mutate(pid = (id1968 * 1000) + pernum) %>%
  select(year, pid, everything())

# Merge Family Identification Data ------------------------------------------
# Load FIMS data 
parents <- openxlsx::read.xlsx(here("0_data", "fims", "20250415_parents.xlsx"))
grandparents <- openxlsx::read.xlsx(here("0_data", "fims", "20250415_grandparents.xlsx"))

# Parent FIMS
p_fims <- parents %>%
  mutate(
    pid = (ID68 * 1000) + PN,
    parent_pid = (ID68P * 1000) + PNP
  ) %>%
  select(pid, parent_pid)

# Grandparent FIMS
g_fims <- grandparents %>%
  mutate(
    pid = (ID68 * 1000) + PN,
    grandparent_pid = (ID68GP * 1000) + PNGP
  ) %>%
  select(pid, grandparent_pid)

# Merge FIMS 
p_fims <- p_fims %>%
  group_by(pid) %>%
  mutate(parent_num = row_number()) %>%
  ungroup()

p_fims_wide <- p_fims %>%
  pivot_wider(
    names_from = parent_num,
    names_prefix = "parent",
    values_from = parent_pid   
  )

g_fims <- g_fims %>%
  group_by(pid) %>%
  mutate(grandparent_num = row_number()) %>%
  ungroup()

g_fims_wide <- g_fims %>%
  pivot_wider(
    names_from = grandparent_num,
    names_prefix = "grandparent",
    values_from = grandparent_pid   
  )

psid_data <- psid_data %>%
  left_join(p_fims_wide, by = "pid") %>%
  left_join(g_fims_wide, by = "pid")

# Transform FIMs IDs to Character Type
psid_data <- psid_data %>%
  mutate(
    across(
      starts_with("parent"),
      ~ ifelse(. == 0, NA, as.character(.))
    ),
    across(
      starts_with("grandparent"),
      ~ ifelse(. == 0, NA, as.character(.))
    )
  )

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("1_build_panel", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(psid_data, here("1_build_panel", "output", "build.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(cwf, all_vars, vars, vartable_list)
rm(p_fims, g_fims, parents, grandparents, g_fims_wide, p_fims_wide)


