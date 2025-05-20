# INPUTS: 0_data/psid.xlsx, 0_data/fims/, 0_data/fam_ind/
# OUTPUTS: Data frames for family data and indiviudal data, including parent and grandparent IDs from FIMs


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
  data.table
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

# EXTRACT VARIABLES ---------------------------------------------------------------------------
# Complete List of Variables ---------------------------------------------------
vars <- list(
  # IDENTIFIERS -------------------------------------------------------------
  list(name = "ind_sample", varname = "sample", var = "ER32006"), # Sample
  list(name = "ind_relation", varname = "relation", var = "ER30003"), # Individual relation to head [all years]
  list(name = "ind_relation_b", varname = "married_pair", var = "ER30005"), # Married pairs indicator for constructing head_id and spouse_id
  list(name = "fam_interview", varname = "interview", var = "V99"), # 1968 Interview Number [all years]
  # list(name = "fam_relation", varname = "relation_construct_3", var = "V180"), # Family interview respondent's relation to head [all years]

  # INCOME ------------------------------------------------------------------
  list(name = "fam_income1", varname = "inc_all", var = "V81"), # Total family income [all years]
  list(name = "fam_income2", varname = "inc_tax_hs", var = "V76"), # Total taxable income, head and spouse [all years]
  list(name = "fam_income3", varname = "inc_tax_o", var = "V79"), # Total taxable income, others in FU [all years]
  list(name = "fam_transfer1", varname = "inc_trans_hs", var = "V1220"), # Amount of total transfers, private + public for household heads and spouses [1970-2021]
  list(name = "fam_transfer3", varname = "inc_trans_o1", var = "V527"), # Amount of total transfers, private + public for others in FU, pro-rated [1969-1993]
  list(name = "fam_transfer4", varname = "inc_trans_o2", var = "V11576"), # Amount of total transfers, private + public for others in FU, not pro-rated [1985-2021]
  
  # WEALTH ------------------------------------------------------------------
  list(name = "fam_wealth_nohouse", varname = "wealth_nohouse", var = "S116"), # Family imputed wealth excluding house equity [all wealth supplement years]
  list(name = "fam_wealth", varname = "wealth", var = "S117"), # Family imputed wealth including house equity [all wealth supplement years]
  list(name = "fam_farmbus", varname = "wealth_farmbus", var = "S103"), # Imputed Value of farm and business assets [all wealth supplement years]
  list(name = "fam_checking", varname = "wealth_checking", var = "S105"), # Imputed value of checking and savings [1984-2017]
  list(name = "fam_debt", varname = "wealth_debt", var = "S107"), # Imputed Debt value [all wealth supplement years]
  list(name = "fam_real_estate", varname = "wealth_re", var = "S109"), # Imputed real estate value, excluding home [1984-2011]
  list(name = "fam_stocks", varname = "wealth_stocks", var = "S111"), # Imputed stock values [all wealth supplement years]
  list(name = "fam_vehicle", varname = "wealth_vehicles", var = "S113"), # Vehicle value [all wealth supplement years]
  list(name = "fam_other_assets", varname = "wealth_other", var = "S115"), # Bonds, insurance, and other collectible values [all wealth supplement years]
  list(name = "fam_student_loans", varname = "student_loans", var = "ER48945"), # Student loan value [2011 - 2021]
  list(name = "fam_home_equity", varname = "wealth_home", var = "S120"), # Home equity value [all wealth supplement years]

  
  # FAMILY CHARACTERISTICS --------------------------------------------------
  list(name = "fam_sex", varname = "head_sex", var = "V119"), # Sex of Household Head [all years]
  list(name = "fam_numfu", varname = "numfu", var = "V115"), # Number of People in Family Unit [all years]
  list(name = "fam_num", varname = "fam_id", var = "V3"), # Family Number [all years]
  list(name = "fam_marital", varname = "marital", var = "V239"), # Marital Status [all years]
  list(name = "fam_house", varname = "house_status", var = "V103"), # Housing Status [all years]
  # list(name = "fam_homevalue", varname = "home_value", var = "V5"), # Home Value [all years]
  list(name = "fam_state", varname = "state", var = "V93"), # State of Residence [all years]
  
  # FAMILY RACE -------------------------------------------------------------
  list(name = "fam_headrace", varname = "race1_head", var = "V181"), # Race of household head [1968-2021]
  list(name = "fam_head2race", varname = "race2_head", var = "V11939"), # Race of household head 2 [1985-2021]
  list(name = "fam_head3race", varname = "race3_head", var = "ER3946"), # Race of household head 3 [1994-2021]
  list(name = "fam_head4race", varname = "race4_head", var = "ER11851"), # Race of household head 4 [1997-2021]
  list(name = "fam_wiferace", varname = "race1_wife", var = "V12293"), # Race of household "wife" [1985-2021]
  list(name = "fam_wife2race", varname = "race2_wife", var = "V12294"), # Race of household "wife" 2 [1985-2021]
  list(name = "fam_wife3race", varname = "race3_wife", var = "ER3885"), # Race of household "wife" 3 [1994-2021]
  list(name = "fam_wife4race", varname = "race4_wife", var = "ER11763"), # Race of household "wife" 4 [1997-2021]
  
  # INDIVIDUAL DEMOGRAPHICS -------------------------------------------------
  list(name = "ind_age", varname = "age", var = "ER30004"), # Age of individual [all years]
  list(name = "ind_death", varname = "yod", var = "ER32050"), # Year of death [all years]
  list(name = "ind_male", varname = "male", var = "ER32000"), # Time invariant - gender [all years]
  
  # INDIVIDUAL EDUCATION ----------------------------------------------------
  list(name = "ind_edu", varname = "edu", var = "ER30010"), # Years of education [1968-2021]
  
  # INDIVIDUAL FAMILY CHARACTERISTICS ---------------------------------------
  list(name = "ind_children", varname = "children", var = "ER32022"), # Number of children [all years]
  
  # INDIVIDUAL WEIGHTS ------------------------------------------------------
  # list(name = "ind_cross_weight", varname = "ind_cross_weight", var = "ER12224"), # Cross-sectional weight [1997-2019]
  # list(name = "ind_weight", varname = "ind_weight", var = "ER12084"), # Longitudinal Combined Core [1997-2021]
  list(name = "ind_cross_weight2", varname = "ind_cross_weight2", var = "ER33438"), # Individual Cross-Sectional Weight [1997-2021]
  list(name = "ind_weight2", varname = "ind_weight2", var = "ER33430"), # Individual Weight [1997-2021]
  list(name = "ind_stratum", varname = "stratum", var = "ER31996"), # Sampling Strata [all years]
  list(name = "ind_cluster", varname = "cluster", var = "ER31997") # Sampling cluster [all years]

  
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
    sequence = ER30002
  )

# Add Unique PID to Individual and Family Data -----------------------------------
psid_data <- psid_data %>%
  mutate(pid = (id1968 * 1000) + sequence) %>%
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


