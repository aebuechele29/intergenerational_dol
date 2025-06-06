# INPUTS: 4_create_outcomes/src/analysis.R
# OUTPUTS: 5_analysis/output/

# LOAD DATA ------------------------------------------------------------------
analysis <- readRDS(here("4_create_outcomes", "output", "analysis.rds"))

analysis <- analysis %>%
  mutate(
    own_home = case_when(
      own_home == 1 ~ "Home Owner",
      own_home == 0 ~ "Renter",
      TRUE ~ NA_character_
    ),
    own_home = factor(own_home, levels = c("Renter", "Home Owner")),

    edu_cat_spouse = case_when(
      edu_cat_spouse == 0 ~ "High School or Less",
      edu_cat_spouse == 1 ~ "Some College",
      edu_cat_spouse == 2 ~ "Bachelor Degree or More",
      TRUE ~ NA_character_
    ),
    edu_cat_spouse = factor(edu_cat_spouse, levels = c("High School or Less", "Some College", "Bachelor Degree or More"))
  )

analysis <- analysis[complete.cases(analysis[, c("year", "own_home", "edu_cat_spouse", "num_fam", "child_under5")]), ]

# TABLE 1A AND 1B: DESCRIBE DATA FOR ANALYSIS -----------------------------------
datasummary(
  year + num_fam + child_under5 + spouse_working + spouse_earnings + spouse_housework + traditional ~ Mean + SD + Min + Max + N, data = analysis, 
  title = "Table 1A. Descriptive Statistics", 
  output = "/Users/amanda/Desktop/categorical_final/5_analysis/output/Table1A_num.docx")

  datasummary(
  own_home + edu_cat_spouse ~ N + Percent(), 
  data = analysis, title = "Table 1B. Descriptive Statistics", 
  output = "/Users/amanda/Desktop/categorical_final/5_analysis/output/Table1B_categ.docx")

# FIGURE 1 - SPECIFY LOGISTIC REGRESSIONS FOR FULL SAMPLE ------------------------
logit_working <- glm(spouse_working ~ year + num_fam + child_under5 + own_home + edu_cat_spouse,
                data = analysis,
                family = binomial(link = "logit")) 

logit_earnings <- glm(spouse_earnings ~ year + num_fam + child_under5 + own_home + edu_cat_spouse,
                data = analysis,
                family = binomial(link = "logit")) 

logit_housework <- glm(spouse_housework ~ year + num_fam + child_under5 + own_home + edu_cat_spouse,
                data = analysis,
                family = binomial(link = "logit")) 

logit_traditional <- glm(traditional ~ year + num_fam + child_under5 + own_home + edu_cat_spouse,
                data = analysis,
                family = binomial(link = "logit")) 

var_names <- c(
  'year' = "Survey Year", 
  'num_fam' = "Family Size", 
  'child_under5' = "Child < 5", # Reference is no child under 5 living with family
  'own_homeHome Owner' = "Home Owner",  # Renter is ref group
  'edu_cat_spouseSome College' = "Some College", # High School or Less is reference
  'edu_cat_spouseBachelor Degree or More' = "Bachelor Degree or More" 
)

modelsummary(
  list(
    "Spouse Working"     = logit_working,
    "Spouse Earning More than Half"    = logit_earnings, 
    "Spouse Completing More than Half of Housework"   = logit_housework,
    "Spouse in Traditional Division of Household Labor" = logit_traditional
  ), 
  exponentiate = TRUE,
  fmt = "%.3f",  # format to 3 decimal places
  gof_map = c("nobs"),
  gof_omit = "AIC|BIC|Log.Lik",
  stars = TRUE,
  title = "Logistic regression for components of the division of household labor",
  coef_map = var_names,
  notes = "Survey years from 1976 to 2021. Reference category for Home Owner is Renters, for Education is High School or Less",
  output = "/Users/amanda/Desktop/categorical_final/5_analysis/output/Figure1.docx"
)


# FIGURE 2 - PLOT PREDICTED PROBABILITIES FOR FULL SAMPLE ------------------------------
preds1 <- avg_predictions(
  logit_traditional,
  by = c("year", "edu_cat_spouse"),
  newdata = datagrid(
    year = unique(analysis$year),
    edu_cat_spouse = levels(analysis$edu_cat_spouse),
    grid_type = "counterfactual"
  ),
  type = "response"
)

plot1 <- ggplot(preds1, aes(x = year, y = estimate, color = edu_cat_spouse)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = edu_cat_spouse),
    alpha = 0.2, color = NA
  ) +
  scale_color_brewer(palette = "Set1", name = "Spouse Education") +
  scale_fill_brewer(palette = "Set1", name = "Spouse Education") +
  scale_y_continuous(limits = c(0, 1)) +   
  labs(
    x = "Survey Year",
    y = "Predicted Probability",
    title = "Predicted Probability of Traditional Household"
  ) +
  theme_minimal(base_size = 14)

ggsave("/Users/amanda/Desktop/categorical_final/5_analysis/output/Figure2_traditional_plot.pdf", plot = plot1, width = 8, height = 5)


preds2 <- avg_predictions(
  logit_working,
  by = c("year", "edu_cat_spouse"),
  newdata = datagrid(
    year = unique(analysis$year),
    edu_cat_spouse = levels(analysis$edu_cat_spouse),
    grid_type = "counterfactual"
  ),
  type = "response"
)

plot2 <- ggplot(preds2, aes(x = year, y = estimate, color = edu_cat_spouse)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = edu_cat_spouse),
    alpha = 0.2, color = NA
  ) +
  scale_color_brewer(palette = "Set1", name = "Spouse Education") +
  scale_fill_brewer(palette = "Set1", name = "Spouse Education") +
  scale_y_continuous(limits = c(0, 1)) +   
  labs(
    x = "Survey Year",
    y = "Predicted Probability",
    title = "Predicted Probability of Spouse Working"
  ) +
  theme_minimal(base_size = 14)

ggsave("/Users/amanda/Desktop/categorical_final/5_analysis/output/Figure2_working_plot.pdf", plot = plot2, width = 8, height = 5)


preds3 <- avg_predictions(
  logit_earnings,
  by = c("year", "edu_cat_spouse"),
  newdata = datagrid(
    year = unique(analysis$year),
    edu_cat_spouse = levels(analysis$edu_cat_spouse),
    grid_type = "counterfactual"
  ),
  type = "response"
)

plot3 <- ggplot(preds3, aes(x = year, y = estimate, color = edu_cat_spouse)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = edu_cat_spouse),
    alpha = 0.2, color = NA
  ) +
  scale_color_brewer(palette = "Set1", name = "Spouse Education") +
  scale_fill_brewer(palette = "Set1", name = "Spouse Education") +
  scale_y_continuous(limits = c(0, 1)) +   
  labs(
    x = "Survey Year",
    y = "Predicted Probability",
    title = "Predicted Probability of Spouse Earning Majority"
  ) +
  theme_minimal(base_size = 14)

ggsave("/Users/amanda/Desktop/categorical_final/5_analysis/output/Figure2_earnings_plot.pdf", plot = plot3, width = 8, height = 5)


preds4 <- avg_predictions(
  logit_housework,
  by = c("year", "edu_cat_spouse"),
  newdata = datagrid(
    year = unique(analysis$year),
    edu_cat_spouse = levels(analysis$edu_cat_spouse),
    grid_type = "counterfactual"
  ),
  type = "response"
)

plot4 <- ggplot(preds4, aes(x = year, y = estimate, color = edu_cat_spouse)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = edu_cat_spouse),
    alpha = 0.2, color = NA
  ) +
  scale_color_brewer(palette = "Set1", name = "Spouse Education") +
  scale_fill_brewer(palette = "Set1", name = "Spouse Education") +
  scale_y_continuous(limits = c(0, 1)) +   
  labs(
    x = "Survey Year",
    y = "Predicted Probability",
    title = "Predicted Probability of Spouse Doing Majority of Housework"
  ) +
  theme_minimal(base_size = 14)

ggsave("/Users/amanda/Desktop/categorical_final/5_analysis/output/Figure2_housework_plot.pdf", plot = plot4, width = 8, height = 5)


# TABLE 2A AND 2B - DESCRIBE PARENT DATA FOR ANALYSIS ---------------------------------
analysis2 <- analysis %>%
  filter(!is.na(parent_traditional))

unique_couples <- analysis2 %>% 
  distinct(couple_id) %>% 
  nrow()

print(unique_couples)

analysis2 <- analysis2[complete.cases(analysis2[, c("year", "own_home", "edu_cat_spouse", "num_fam", "child_under5", "parent_data_side")]), ]

datasummary(
  year + num_fam + child_under5 + spouse_working + spouse_earnings + spouse_housework + traditional +
  parent_spouse_working + parent_spouse_earnings + parent_spouse_housework + parent_traditional ~ Mean + SD + Min + Max + N, data = analysis2, 
  title = "Table 2A. Descriptive Statistics", 
  output = "/Users/amanda/Desktop/categorical_final/5_analysis/output/Table2A_num.docx")

  datasummary(
  own_home + edu_cat_spouse + parent_data_side ~ N + Percent(), 
  data = analysis2, title = "Table 2B. Descriptive Statistics", 
  output = "/Users/amanda/Desktop/categorical_final/5_analysis/output/Table2B_categ.docx")


# FIGURE 3 - SPECIFY LOGISTIC REGRESSIONS FOR PARENT-CHILD SAMPLE ------------------------
logit_working2 <- glm(spouse_working ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_working,
                data = analysis2,
                family = binomial(link = "logit")) 

logit_earnings2 <- glm(spouse_earnings ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_earnings,
                data = analysis2,
                family = binomial(link = "logit")) 

logit_housework2 <- glm(spouse_housework ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_housework,
                data = analysis2,
                family = binomial(link = "logit")) 

logit_traditional2 <- glm(traditional ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_traditional,
                data = analysis2,
                family = binomial(link = "logit")) 

var_names <- c(
  'year' = "Survey Year", 
  'num_fam' = "Family Size", 
  'child_under5' = "Child < 5", # Reference is no child under 5 living with family
  'own_homeHome Owner' = "Home Owner",  # Renter is reference group
  'edu_cat_spouseSome College' = "Some College", 
  'edu_cat_spouseBachelor Degree or More' = "Bachelor Degree or More",
  'parent_spouse_working' = "Parent: Spouse Working",
  'parent_spouse_earnings' = "Parent: Spouse Earns More",
  'parent_spouse_housework' = "Parent: Spouse Does More Housework",
  'parent_traditional' = "Parent: Traditional Household"
)

logit_working3 <- glm(spouse_working ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_working + parent_data_side,
                     data = analysis2,
                     family = binomial(link = "logit"))

logit_earnings3 <- glm(spouse_earnings ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_earnings + parent_data_side,
                      data = analysis2,
                      family = binomial(link = "logit"))

logit_housework3 <- glm(spouse_housework ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_spouse_housework + parent_data_side,
                       data = analysis2,
                       family = binomial(link = "logit"))

logit_traditional3 <- glm(traditional ~ year + num_fam + child_under5 + own_home + edu_cat_spouse + parent_traditional + parent_data_side,
                        data = analysis2,
                        family = binomial(link = "logit"))

var_names_updated <- c(
  'year' = "Survey Year", 
  'num_fam' = "Family Size", 
  'child_under5' = "Child < 5", # Reference is no child under 5 living with family
  'own_homeHome Owner' = "Home Owner",  # Renter is reference group
  'edu_cat_spouseSome College' = "Some College", 
  'edu_cat_spouseBachelor Degree or More' = "Bachelor Degree or More",
  'parent_spouse_working' = "Parent: Spouse Working",
  'parent_spouse_earnings' = "Parent: Spouse Earns More",
  'parent_spouse_housework' = "Parent: Spouse Does More Housework",
  'parent_traditional' = "Parent: Traditional Household",
  'parent_data_sidespouse' = "Parent: Data from Spouse's Side"
)

# This tests models with and without sides, not included in final paper
modelsummary(
  list(
    "Spouse Working" = logit_working2,
    "Spouse Working with Side" = logit_working3,
    "Spouse Earning More" = logit_earnings2,
    "Spouse Earning More with Side" = logit_earnings3,
    "Spouse Completing More than Half of Housework" = logit_housework2,
    "Spouse Completing More than Half of Housework with Side" = logit_housework3,
    "Traditional Division of Household Labor" = logit_traditional2,
    "Traditional Division of Household Labor with Side" = logit_traditional3
  ),
  exponentiate = TRUE,
  fmt = "%.3f",
  stars = TRUE,
  # gof_map = c("nobs", "AIC", "BIC", "Log.Lik"),  # explicitly include goodness-of-fit stats
  title = "Logistic Regressions With and Without Parent Data Side",
  coef_map = var_names_updated,
  output = "5_analysis/output/TEST_parent_models.docx"
)

# This creates Figure 3
modelsummary(
  list(
    "Spouse Working" = logit_working3,
    "Spouse Earning More than Half" = logit_earnings3,
    "Spouse Completing More than Half of Housework" = logit_housework3,
    "Traditional Division of Household Labor" = logit_traditional3
  ),
  exponentiate = TRUE,
  fmt = "%.3f",
  stars = TRUE,
  gof_map = c("nobs", "AIC", "BIC", "Log.Lik"),  
  title = "Logistic Regressions With Parent Data",
  coef_map = var_names_updated,
  output = "5_analysis/output/Figure3.docx"
)


# FIGURE 4 PLOT PREDICTED PROBABILITIES BASED ON FIGURE 3 ---------------------------
plot5 <- plot_predictions(
  model = logit_housework3,
  by = c("year", "parent_spouse_housework"),
  newdata = datagrid(
    parent_spouse_housework = 0:1,
    year = unique(analysis2$year),
    grid_type = "counterfactual"
  ),
  type = "response",
  draw = FALSE
)

p <- ggplot(plot5, aes(x = year, y = estimate, color = factor(parent_spouse_housework))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(parent_spouse_housework)), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Spouse Doing Majority of Housework Based on their Parents",
    x = "Survey Year",
    y = "Predicted Probability",
    color = "Mother Did Most Housework",
    fill = "Mother Did Most Housework"
  ) +
  theme_classic()

ggsave("5_analysis/output/Figure4_parent_housework.png", p, width = 8, height = 5, dpi = 300)


plot6 <- plot_predictions(
  model = logit_housework3,
  by = c("parent_data_side", "parent_spouse_housework"),
  newdata = datagrid(
    parent_spouse_housework = 0:1,
    parent_data_side = c("spouse", "head"),
    grid_type = "counterfactual"
  ),
  type = "response",
  draw = FALSE
)

p <- ggplot(plot6, aes(x = parent_data_side, y = estimate, color = factor(parent_spouse_housework), group = parent_spouse_housework)) +
  geom_line(position = position_dodge(width = 0.3), size = 1.2) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.3)) +
  labs(
    title = "Predicted Probability of Spouse Doing Majority of Housework Based on their Parents",
    x = "Data From Spouse's or Head's Parent",
    y = "Predicted Probability",
    color = "Mother Did Most Housework",
    fill = "Mother Did Most Housework"
  ) +
  theme_classic()

ggsave("5_analysis/output/Figure4_parent_housework_side.png", p, width = 8, height = 5, dpi = 300)


# TABLE 3 - DESCRIPTIVE TABLE OF PARENT / CHILD CHANGE IN HOUSEHOLD STRUCTURE ---------------
tab <- table(analysis2$traditional, analysis2$parent_traditional)
dimnames(tab) <- list(
  "Current Traditional" = c("No (0)", "Yes (1)"),
  "Parent Traditional"  = c("No (0)", "Yes (1)")
)
tab


# MODEL TESTING - NOT INCLUDED IN FINAL PAPER ------------------------------------------------
# VIF for Initial Models
vif_traditional <- vif(logit_traditional)
print(vif_traditional)

# VIF for logit_working
vif_working <- vif(logit_working)
print(vif_working)

# VIF for logit_earnings
vif_earnings <- vif(logit_earnings)
print(vif_earnings)

# VIF for logit_housework
vif_housework <- vif(logit_housework)
print(vif_housework)

# VIF for Parent Models
vif_traditional <- vif(logit_traditional3)
print(vif_traditional)

# VIF for logit_working3
vif_working <- vif(logit_working3)
print(vif_working)

# VIF for logit_earnings3
vif_earnings <- vif(logit_earnings3)
print(vif_earnings)

# VIF for logit_housework3
vif_housework <- vif(logit_housework3)
print(vif_housework)



