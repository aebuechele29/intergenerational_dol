# INPUTS: 4_create_clans/output/clans.rds
# OUTPUTS: 

clans <- readRDS(here("4_create_clans", "output", "clans.rds"))

# CLEAN INCOME AND WEALTH VARIABLES ----------------------------------------------
income_prefixes <- c(
  "inc_all_", "inc_tax_hs_", "inc_tax_o_", 
  "inc_trans_hs_", "inc_trans_o1_", "inc_trans_o2_"
)

wealth_prefixes <- c(
  "wealth_nohouse_", "wealth_farmbus_", "wealth_checking_",
  "wealth_debt_", "wealth_re_", "wealth_stocks_",
  "wealth_vehicles_", "wealth_other_", "wealth_home_",
  "student_loans_"
)

# Function for summary statistics
compute_summaries <- function(df, prefixes) {
  res <- list()
  
  for (prefix in prefixes) {
    cols <- grep(paste0("^", prefix, "\\d+$"), names(df), value = TRUE)
    if (length(cols) == 0) next

    mat <- as.matrix(sapply(df[cols], as.numeric))

    mean_val   <- rowMeans(mat, na.rm = TRUE)
    median_val <- apply(mat, 1, median, na.rm = TRUE)
    min_val    <- apply(mat, 1, min, na.rm = TRUE)
    max_val    <- apply(mat, 1, max, na.rm = TRUE)
    sd_val     <- apply(mat, 1, sd, na.rm = TRUE)
    count_val  <- rowSums(!is.na(mat))

    clean_name <- gsub("_$", "", prefix)

    res[[paste0(clean_name, "_mean_clan")]]   <- mean_val
    res[[paste0(clean_name, "_median_clan")]] <- median_val
    res[[paste0(clean_name, "_min_clan")]]    <- min_val
    res[[paste0(clean_name, "_max_clan")]]    <- max_val
    res[[paste0(clean_name, "_sd_clan")]]     <- sd_val
    res[[paste0(clean_name, "_count_clan")]]  <- count_val
  }

  as_tibble(res)
}

income_summary <- compute_summaries(clans, income_prefixes)
wealth_summary <- compute_summaries(clans, wealth_prefixes)

clans <- bind_cols(clans, income_summary, wealth_summary)

all_prefixes <- c(income_prefixes, wealth_prefixes)
remove <- paste0("^(", paste0(all_prefixes, collapse = "|"), ")\\d+$")

clans <- clans %>%
  select(-matches(remove))


# CLEAN RACE VARIABLES ------------------------------------------------------------------------
# race

# CLEAN EDUCATION VARIABLES -------------------------------------------------------------------
edu_prefixes <- c("prop_edu_cat1_", "prop_edu_cat2_", "prop_hs_", "prop_ba_", "prop_ma_")
numfu_prefix <- "numfu_"

# For each education variable, find all columns ending with _1, _2, ..., _numclan
# Then compute weighted average across these columns by family unit size (numfu_#),
# dividing the weighted sum by total family units in the clan (numclan).

clans <- clans %>%
  mutate(
    total_numfu = rowSums(across(starts_with(numfu_prefix)), na.rm = TRUE)
  )

for (edu_var in edu_prefixes) {
  cols <- grep(paste0("^", edu_var, "\\d+$"), names(clans), value = TRUE)
  numfu_cols <- sub(edu_var, numfu_prefix, cols)
  
  mat_props <- as.matrix(clans[cols])
  mat_numfu <- as.matrix(sapply(clans[numfu_cols], as.numeric))
  
  weighted_sum <- rowSums(mat_props * mat_numfu, na.rm = TRUE)
  
  total_numfu <- rowSums(mat_numfu, na.rm = TRUE)  # sum only relevant numfu cols for this edu_var
  
  weighted_prop <- weighted_sum / total_numfu
  
  new_var_name <- sub("_$", "", edu_var)
  clans[[new_var_name]] <- weighted_prop
}

cols_to_drop <- unlist(
  lapply(edu_prefixes, function(prefix) {
    grep(paste0("^", prefix, "\\d+$"), names(clans), value = TRUE)
  })
)

clans <- clans %>% select(-all_of(cols_to_drop))

# CLEAN YOB VARIABLES -------------------------------------------------------------------
yob_prefix <- "mean_yob_"

yob_cols <- grep(paste0("^", yob_prefix, "\\d+$"), names(clans), value = TRUE)

mat_yob <- as.matrix(clans[, yob_cols])

weighted_mean_yob <- rowSums(mat_yob * mat_numfu, na.rm = TRUE) / rowSums(mat_numfu, na.rm = TRUE)

median_yob <- apply(mat_yob, 1, function(x) stats::median(x, na.rm = TRUE))
min_yob <- apply(mat_yob, 1, function(x) min(x, na.rm = TRUE))
max_yob <- apply(mat_yob, 1, function(x) max(x, na.rm = TRUE))

clans <- clans %>%
  mutate(
    mean_yob_clan = weighted_mean_yob,
    median_yob_clan = median_yob,
    min_yob_clan = min_yob,
    max_yob_clan = max_yob
  )

clans <- clans %>%
  select(-all_of(yob_cols))


# CLEAN HOMEOWNERSHIP VARIABLES -------------------------------------------------------------------
own_home_prefix <- "own_home_"

own_home_cols <- grep(paste0("^", own_home_prefix, "\\d+$"), names(clans), value = TRUE)

mat_own_home <- as.matrix(sapply(clans[own_home_cols], as.numeric))

weighted_sum_own_home <- rowSums(mat_own_home * mat_numfu, na.rm = TRUE)
total_numfu_own_home <- rowSums(mat_numfu, na.rm = TRUE)

weighted_prop_own_home <- weighted_sum_own_home / total_numfu_own_home

clans <- clans %>%
  mutate(
    own_home_clan = weighted_prop_own_home
  )

clans <- clans %>% select(-all_of(own_home_cols))


# CLEAN SEX VARIABLES -----------------------------------------------------------------------------
prop_male_prefix <- "prop_male_"

prop_male_cols <- grep(paste0("^", prop_male_prefix, "\\d+$"), names(clans), value = TRUE)

mat_prop_male <- as.matrix(sapply(clans[prop_male_cols], as.numeric))

weighted_sum_prop_male <- rowSums(mat_prop_male * mat_numfu, na.rm = TRUE)

total_numfu_prop_male <- rowSums(mat_numfu, na.rm = TRUE)

weighted_prop_male <- weighted_sum_prop_male / total_numfu_prop_male

clans <- clans %>%
  mutate(
    prop_male_clan = weighted_prop_male
  ) %>%
  select(-all_of(prop_male_cols))


# CLEAN STATE VARIABLES -------------------------------------------------------------------
state_prefix <- "state_"

state_cols <- grep(paste0("^", state_prefix, "\\d+$"), names(clans), value = TRUE)

mat_states <- as.matrix(sapply(clans[state_cols], as.character))

num_unique_states <- apply(mat_states, 1, function(x) {
  length(unique(na.omit(x)))
})

multi_states_indicator <- as.integer(num_unique_states > 1)

clans <- clans %>%
  mutate(multi_states = multi_states_indicator) %>%
  select(-all_of(state_cols))


# CLEAN SIZE OF FAMILY VARIABLES -----------------------------------------------------------
numfu_cols <- grep(paste0("^", numfu_prefix, "\\d+$"), names(clans), value = TRUE)

total_people <- rowSums(mat_numfu, na.rm = TRUE)

num_families <- rowSums(!is.na(mat_numfu))

avg_family_size <- total_people / num_families

# Add to clans and optionally drop numfu_# columns
clans <- clans %>%
  mutate(
    clan_size = total_people,
    avg_family_size = avg_family_size
  )

clans <- clans %>% select(-all_of(numfu_cols))


# REMOVE UNNECESSARY COLUMNS -----------------------------------------------------------
married_prefix <- "married_pair_"
married_cols <- grep(paste0("^", married_prefix, "\\d+$"), names(clans), value = TRUE)

fam_id_prefix <- "fam_id_"
fam_id_cols <- grep(paste0("^", fam_id_prefix, "\\d+$"), names(clans), value = TRUE)

clans <- clans %>% select(-all_of(c(married_cols, fam_id_cols)))











