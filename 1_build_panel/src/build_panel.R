# SET WORKING DIRECTORY TO ENABLE RELATIVE PATHS
setwd("/Users/amanda/Desktop/clan_project")
# here::i_am("1_build_panel/src/build_panel.R")

# The purpose of this file is to build a panel by:
# 1. Initialize PSID data downloaded from the PSID cross-year selection tool 
# 2. Define functions to extract variables using the CWF file 
# 3. Extract family and individual variables 
# 5. Initialize FIMS data downloaded from the PSID website 
# 6. Merge FIMS data with family and individual data 

# Load required libraries -------------------------------------------------s
if (!require("pacman")) {
  install.packages(
    "pacman",
    repos = "http://cran.us.r-project.org"
  )
}

if (!require("here")) {
  install.packages("here", repos = "http://cran.us.r-project.org")
}
library(here)

pacman::p_load(
  tidyverse,
  dplyr,
  purrr,
  data.table,
  easyPSID,
  openxlsx,
  psidR       
)

# Load the CWF Finder File ------------------------------------------------
cwf <- openxlsx::read.xlsx(here("0_data", "psid.xlsx"))

# Load PSID Data ----------------------------------------------------------
# This data is downloaded from the PSID website using the cross-year selection tool
input_dir <- here("0_data", "fam_ind")
output_dir <- here("1_build_panel", "output")

easyPSID::convert_to_rds(
  in_direc = input_dir,
  out_direc = output_dir
)

psid_data <- readRDS(here("1_build_panel", "output", "J34346415.rds"))


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

# Define Function to Apply Vars with Only Most Recent Wave to All ---------
fill_columns_with_latest_value <- function(df) {
  # Identify columns where only the bottom row has a value
  bottom_row <- df %>% tail(1)
  cols_to_fill <- df %>%
    summarise(across(everything(), ~ sum(!is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "non_na_count") %>%
    filter(non_na_count == 1) %>%
    pull(column)
  
  filled_df <- df %>%
    mutate(
      across(
        all_of(cols_to_fill),
        ~ ifelse(is.na(.), bottom_row[[cur_column()]], .)
      )
    )
  
  return(filled_df)
}

# Complete List of Variables ---------------------------------------------------
vars <- list(
  # INCOME ------------------------------------------------------------------
  list(name = "fam_income", varname = "income", var = "V81"), # Total family income [all years]
  list(name = "fam_transferamount", varname = "priv_transfer_amount", var = "V266"), # Amount of total private transfers [all years]
  list(name = "fam_transfertotal", varname = "transfer_total", var = "V1220"), # Amount of total transfers, private + public [1970-2021]
  
  # WEALTH ------------------------------------------------------------------
  list(name = "fam_wealth", varname = "wealth", var = "S117"), # Family imputed wealth including house equity [all wealth supplement years]
  list(name = "fam_annuity_ira", varname = "annuity_ira", var = "S419"), # Annuity / IRA value [all wealth supplement years]
  list(name = "fam_home_equity", varname = "home_equity", var = "S120"), # Home equity value [all wealth supplement years]
  list(name = "fam_other_assets", varname = "other_assets", var = "S115"), # Bonds, insurance, and other collectible values [all wealth supplement years]
  list(name = "fam_debt", varname = "debt", var = "S107"), # Debt value [all wealth supplement years]
  list(name = "fam_stock", varname = "stock", var = "S111"), # Stock value [all wealth supplement years]
  list(name = "fam_student_loans", varname = "student_loans", var = "ER48945"), # Student loan value [2011 - 2021]
  list(name = "fam_wealth_nohouse", varname = "wealth_nohouse", var = "S116"), # Family imputed wealth excluding house equity [all wealth supplement years]
  list(name = "fam_vehicle", varname = "vehicle", var = "S113"), # Vehicle value [all wealth supplement years]
  
  # FAMILY CHARACTERISTICS --------------------------------------------------
  list(name = "fam_sex", varname = "head_sex", var = "V119"), # Sex of Household Head [all years]
  list(name = "fam_numfu", varname = "numfu", var = "V115"), # Number of People in Family Unit [all years]
  list(name = "fam_num", varname = "fam_id", var = "V3"), # Family Number [all years]
  list(name = "fam_marital", varname = "marital", var = "V239"), # Marital Status [all years]
  list(name = "fam_house", varname = "house_status", var = "V103"), # Housing Status [all years]
  list(name = "fam_homevalue", varname = "home_value", var = "V5"), # Home Value [all years]
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
  list(name = "ind_male", varname = "male", var = "ER32000"), # Time invariant - gender [all years]
  
  # INDIVIDUAL EDUCATION ----------------------------------------------------
  list(name = "ind_edu", varname = "edu", var = "ER30010"), # Years of education [1968-2021]
  list(name = "ind_college", varname = "college_degree", var = "ER34335"), # Whether received college degree [2015-2021]
  
  # INDIVIDUAL FAMILY CHARACTERISTICS ---------------------------------------
  list(name = "ind_firstchild", varname = "first_child", var = "ER32024"), # Time invariant - year first child [all years]
  list(name = "ind_children", varname = "children", var = "ER32022"), # Number of children [all years]
  list(name = "ind_firstmarriage", varname = "first_marriage", var = "ER32036"), # Time invariant - year first marriage [all years]
  
  # INDIVIDUAL WEIGHTS ------------------------------------------------------
  list(name = "ind_cross_weight", varname = "ind_cross_weight", var = "ER12224"), # Cross-sectional weight [1997-2019]
  list(name = "ind_weight", varname = "ind_weight", var = "ER12084"), # Longitudinal Combined Core [1997-2021]
  list(name = "ind_cross_weight2", varname = "ind_cross_weight2", var = "ER33438"), # Individual Cross-Sectional Weight [1997-2021]
  list(name = "ind_weight2", varname = "ind_weight2", var = "ER33430"), # Individual Weight [1997-2021]
  list(name = "ind_stratum", varname = "stratum", var = "ER31996"), # Sampling Strata [all years]
  list(name = "ind_cluster", varname = "cluster", var = "ER31997"), # Sampling cluster [all years]
  
  # IDENTIFIERS -------------------------------------------------------------
  list(name = "ind_sample", varname = "sample", var = "ER32006"), # Sample
  list(name = "ind_relation", varname = "relation", var = "ER30003"), # Individual relation to head [all years]
  list(name = "ind_relation_b", varname = "relation_construct_2", var = "ER30005"), # For constructing head_id and spouse_id
  list(name = "fam_interview", varname = "interview", var = "V99") # 1968 Interview Number [all years]
  
)

# Process Variables Using the CWF File ------------------------------------------
vartable_list <-
  vars %>%
  map(extract_psid_vartables) %>%
  map(remove_y) %>%
  map(rename_variable)

inds <-
  vartable_list %>%
  map(create_sublist, "ind") %>%
  compact() %>%
  reduce(inner_join, by = "year") %>%
  fill_columns_with_latest_value()

fams <-
  vartable_list %>%
  map(create_sublist, "fam") %>%
  compact() %>%
  reduce(inner_join, by = "year")

# Define Function to Build Individual and Family Panel ---------------------------
extract_psid_data_by_inds <- function(psid_data, mapping, id_vars = c("ER30000", "ER30001", "ER30002")) {
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

individual_data <- extract_psid_data_by_inds(psid_data, inds)
family_data <- extract_psid_data_by_inds(psid_data, fams)

# Clean Individual_data and Family_data -------------------------------------------
individual_data <- individual_data %>%
  rename(
    release = ER30000,
    id1968 = ER30001,
    sequence = ER30002
  )
family_data <- family_data %>%
  rename(
    release = ER30000,
    id1968 = ER30001,
    sequence = ER30002
  )

# Add Unique PID to Individual and Family Data -----------------------------------
individual_data <- individual_data %>%
  mutate(pid = (id1968 * 1000) + sequence) %>%
  select(year, pid, everything())

family_data <- family_data %>%
  mutate(pid = (id1968 * 1000) + sequence) %>%
  select(year, pid, everything())

# Merge Family and Individual Data by PID and Year
merged <- individual_data %>%
  left_join(family_data, by = c("pid", "year"))

merged <- merged %>%
  mutate(family_interview = if_else(!is.na(release.y), 1L, 0L))

expected_missing <- 1496629

actual_missing <- merged %>%
  filter(is.na(release.y)) %>%
  nrow()

if (actual_missing != expected_missing) {
  stop(paste0("Unexpected number of missing family interviews: expected ", 
              expected_missing, ", got ", actual_missing))
}

merged <- merged %>%
  select(-ends_with(".y"))

names(merged) <- gsub("\\.x$", "", names(merged))

# Merge Family Identification Data ------------------------------------------

# Load FIMS data ---------------------------------------------------------
parents <- openxlsx::read.xlsx(here("0_data", "fims", "20250415_parents.xlsx"))
grandparents <- openxlsx::read.xlsx(here("0_data", "fims", "20250415_grandparents.xlsx"))
siblings <- openxlsx::read.xlsx(here("0_data", "fims", "20250415_siblings.xlsx"))

# Process Parent FIMS -----------------------------------------------------
p_fims <- parents %>%
  mutate(
    pid = (ID68 * 1000) + PN,
    parent_pid = (ID68P * 1000) + PNP
  ) %>%
  select(pid, parent_pid)

# Process Grandparent FIMS ------------------------------------------------
g_fims <- grandparents %>%
  mutate(
    pid = (ID68 * 1000) + PN,
    grandparent_pid = (ID68GP * 1000) + PNGP
  ) %>%
  select(pid, grandparent_pid)

# Process Sibling FIMS ----------------------------------------------------
s_fims <- siblings %>%
  mutate(
    pid = (ID68 * 1000) + PN,
    sibling_pid = (ID68S * 1000) + PNS
  ) %>%
  rename(
    sibnum = SIBNUM,
    sibsex = SIBSEX,
    sibtype = SIBTYPE,
    pstring = PSTRING,
    sibmaker = SIBMAKER
  ) %>%
  select(pid, sibnum, sibsex, sibtype, pstring, sibmaker, sibling_pid)
# s_fims <- siblings %>%
#   mutate(
#     parent_pid = (ID68 * 1000) + PN,
#     uncle_aunt_pid = (ID68S * 1000) + PNS
#   ) %>%
#   select(parent_pid, uncle_aunt_pid)

# Merge FIMS Data with Merged Data ------------------------------------------
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

s_fims <- s_fims %>%
  group_by(pid) %>%
  mutate(sibling_num = row_number()) %>%
  ungroup()

s_fims_wide <- s_fims %>%
  pivot_wider(
    names_from = sibling_num,
    names_prefix = "sibling",
    values_from = sibling_pid
  )

merged <- merged %>%
  left_join(p_fims_wide, by = "pid") %>%
  left_join(g_fims_wide, by = "pid")
# left_join(s_fims_wide, by = "pid")

# Save merged data to file -----------------------------------------------
saveRDS(merged, here("1_build_panel", "output", "merged.rds"))


# Clean up temporary files --------------------------------------------------
rm(cwf, fams, inds, psid_data, vars, vartable_list, family_data, individual_data)
rm(p_fims, g_fims, s_fims, parents, grandparents, siblings)
file.remove(here("1_build_panel", "output", "J34346415.rds"))
