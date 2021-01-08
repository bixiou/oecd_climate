library(haven)
library(xtable)
library(stargazer)

Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

us <- readRDS("../data/US_pilot_clean.rds")


##### 1. Creation control variables #####

# TODO: all these variables should better be created inside "convert" in preparation.
# race TODO: problem: someone can be at the same time Hispanic and black or white. Why don't you keep the dummies race_white, race_black, race_hispanic?
us$race_white_only <- 0
us[us$race_white == TRUE & us$race_black == FALSE & us$race_hispanic == FALSE & us$race_asian == FALSE & us$race_native == FALSE, "race_white_only"] <- 1
#us[us$race_black == TRUE, "race"] <- "Black"
#us[us$race_hispanic == TRUE, "race"] <- "Hispanic"
#us[us$race_asian == TRUE | us$race_native == TRUE | us$race_hawaii == TRUE | us$race_other_choice == TRUE | us$race_pnr == TRUE , "race"] <- "Other"

#gender: Other set as Male for the moment, see if lot of similar answers in final data
us$gender_dum <- as.character(us$gender)
us[us$gender == "Other", "gender_dum"] <- "Male"


# children
us$children <- 0
us[us$nb_children >= 1, "children"] <- 1

# college
us$college <- "No college"
us[us$education >= 5, "college"] <- "College Degree"

# employment
us$employment_agg <-  "Not working"
us[us$employment_status == "Student", "employment_agg"] <- "Student"
us[us$employment_status == "Retired", "employment_agg"] <- "Retired"
us[us$employment_status == "Self-employed" | us$employment_status == "Full-time employed" | us$employment_status == "Part-time employed", "employment_agg"] <- "Working"

# age
us$age_agg <- NULL
us[us$age %in% 18:29, "age_agg"] <- "18-29"
us[us$age %in% 30:49, "age_agg"] <- "30-49"
us[us$age %in% 50:87, "age_agg"] <- "50-87"

# political position
us$vote_dum <- as.character(us$vote)
us[us$vote_participation == 2, "vote_dum"] <- "Non-voting"

# treatment
us$treatment_agg <- NULL
us[us$treatment_policy == 0 & us$treatment_climate == 0, "treatment_agg"] <- "None"
us[us$treatment_policy == 0 & us$treatment_climate == 1, "treatment_agg"] <- "Climate treatment only"
us[us$treatment_policy == 1 & us$treatment_climate == 0, "treatment_agg"] <- "Policy treatment only"
us[us$treatment_policy == 1 & us$treatment_climate == 1, "treatment_agg"] <- "Both"

# Controls var as factors
us$race_white_only <- as.factor(us$race_white_only)
us$gender_dum <- as.factor(us$gender_dum)
us$children <- as.factor(us$children)
us$college <- as.factor(us$college)
us$employment_agg <- as.factor(us$employment_agg)
us$income_factor <- as.factor(us$income)
us$age_agg <- as.factor(us$age_agg)
us$vote_dum <- as.factor(us$vote_dum)
us$treatment_agg <- as.factor(us$treatment_agg)

##### 2. Regressions #####

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum", "treatment_agg")
cov_lab = c("White only", "Male", "Children", "No college", "Retired" ,"Student", "Working", "Income Q2", "Income Q3", "Income Q4","30-49", "50-87", "Non voting", "Other", "Trump", "Climate treatment only", "No treatment", "Policy treatment only")

desc_table <- function(dep_vars, filename = NULL, data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL,
                       save_folder = "../tables/", dep.var.labels = dep_vars, dep.var.caption = NULL, digits= 3) {
  models <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    models[[i]] <- lm(as.formula(paste(dep_vars[i], "~", paste(indep_vars, collapse = '+'))), data = data, weights = weights)
    means[i] <- round(mean(us[dep_vars[i]], na.rm = T), d = digits)
  }
  if (missing(filename)) file_path <- NULL
  else file_path <- paste(save_folder, filename, ".tex", sep="")
  table <- do.call(stargazer, c(models,
                                list(out=file_path, header=F,
                                     covariate.labels = cov_lab, add.lines =list(c("Mean", means)),
                                     dep.var.labels = dep.var.labels,
                                     dep.var.caption = dep.var.caption,
                                     multicolumn = F, float = F, keep.stat = c("n")
                                )))
  return(table)
}

## Cause of CC
us$CC_exists_no <- (us$CC_exists == -1)
us$CC_exists_nat <- (us$CC_exists == 0)
us$CC_exists_anthro <- (us$CC_exists == 1)

temp <- desc_table(dep_vars = c("CC_exists_no", "CC_exists_nat", "CC_exists_anthro"), filename = "CC_exists",
                   dep.var.labels = c("not a reality","mainly due to natural climate variability", "mainly due to human activity"),
                   dep.var.caption = c("Climate change is…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Halving GHG
us$CC_dynamics_no <- (us$CC_dynamics == -2)
us$CC_dynamics_dec <- (us$CC_dynamics == -1)
us$CC_dynamics_stab <- (us$CC_dynamics == 0)
us$CC_dynamics_rise <- (us$CC_dynamics == 1)

temp <- desc_table(dep_vars = c("CC_dynamics_no", "CC_dynamics_dec", "CC_dynamics_stab", "CC_dynamics_rise"), filename = "CC_dynamics",
                   dep.var.labels = c("has no impact on temperatures","will decrease temperatures", "will stabilize temperatures", "will increase temperatures, just more slowly"),
                   dep.var.caption = c("Halving global GHG emissions"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Comparisons GHG questions
temp <- desc_table(dep_vars = c("CC_factor_beef", "CC_factor_nuclear", "CC_factor_car"), filename = "comparisons_GHG",
                   dep.var.labels = c("eating beef vs. two servings of pasta","eletricity produced by nuclear power vs. wind turbines",  "commuting by car vs. food waste"),
                    dep.var.caption = c("Does this activity emits fare more GHG than this other one?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Responsible party for CC
temp <- desc_table(dep_vars = c("CC_responsible_each", "CC_responsible_rich", "CC_responsible_govts", "CC_responsible_companies", "CC_responsible_past", "CC_responsible_foreign", "CC_responsible_nature", "CC_responsible_denial"), filename = "responsible_GHG",
                  dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "Climate change is not a reality"),
                  dep.var.caption = c("Which of the following is predominantly responsible for CC?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Possible to halt CC
us$CC_stoppable_no_infl <-- (us$CC_stoppable == "No influence")
us$CC_stoppable_adapt <-- (us$CC_stoppable == "Better to adapt")
us$CC_stoppable_should <-- (us$CC_stoppable == "Should but not happening")
us$CC_stoppable_policies <-- (us$CC_stoppable == "Policies & awareness will")
us$CC_stoppable_progress <-- (us$CC_stoppable == "Progress will suffice")

temp <- desc_table(dep_vars = c("CC_stoppable_no_infl", "CC_stoppable_adapt", "CC_stoppable_should", "CC_stoppable_policies", "CC_stoppable_progress"), filename = "CC_stoppable",
          dep.var.labels = c("Human have no noticeable influence","Better live with CC than try to halt it",  "Should stop emmissions, but not going to happen", "Ambitious policies and awareness will succeed", "Technologies and habits will suffice"),
          dep.var.caption = c("Can humanity halt CC?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Talks about CC
us$CC_talks_never <-- (us$CC_talks == -1)
us$CC_talks_yearly <-- (us$CC_talks == 0)
us$CC_talks_monthly <-- (us$CC_talks == 1)

temp <- desc_table(dep_vars = c("CC_talks_never", "CC_talks_yearly", "CC_talks_monthly"), filename = "CC_talks",
          dep.var.labels = c("Never","Yearly",  "Monthly"),
          dep.var.caption = c("How often do you talk about CC?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Generations most affected
temp <- desc_table(dep_vars = c("CC_affected_1960", "CC_affected_1990", "CC_affected_2020", "CC_affected_2050", "CC_affected_none"), filename = "CC_affected.",
          dep.var.labels = c("Born in 1960s", "Born in 1990s", "Born in 2020s", "Born in 2050s", "None of them"),
          dep.var.caption = c("Which generations will be seriously affected by CC?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Sustainable lifestyle
us$dummy_change_lifestyle <-- (us$change_lifestyle == "Yes")

temp <- desc_table(dep_vars = c("dummy_change_lifestyle"), filename = "change_lifestyle",
          dep.var.labels = c("Willing to change lifestyle"),
          dep.var.caption = c("Scenario: world consensus to fight CC and wider green transports and energy available"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Willing to change lifestyle
temp <- desc_table(dep_vars = c("change_condition_policies", "change_condition_income", "change_condition_all", "change_condition_no_rich", "change_condition_no_selfish", "change_condition_no_denial", "change_condition_already", "change_condition_try"), filename = "change_condition",
          dep.var.labels = c("Yes, if policies in the good direction","Yes, if financial means",  "Yes, if everyone does the same", "No, only rich should", "No, would affect me more than living with CC", "No, CC not a real problem", "Lifestyle already sustainable", "Trying, but trouble to change"),
          dep.var.caption = c("Would you be willing to change your lifestyle?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Effect of policies
temp <- desc_table(dep_vars = c("effect_policies_opportunity", "effect_policies_cost", "effect_policies_lifestyle"), filename = "effect_policies",
          dep.var.labels = c("be an opportunity for our economy and improve our lifestyle","be costly, but we would maintain our lifestyle", "would require deep change in our lifestyle"),
          dep.var.caption = c("The policies aimed at halting CC would "), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Issues to address to halt CC
#BP : might need to add this var. but empty for the moment us$kaya_other_choice
temp <- desc_table(dep_vars = c("kaya_techno", "kaya_waste", "kaya_wealth", "kaya_overconsumption", "kaya_overpopulation", "kaya_none"), filename = "kaya",
          dep.var.labels = c("Use of technologies that emit GHG", "Level of waste", "High standards of living", "Overconsumption", "Overpopulation", "None of them"),
          dep.var.caption = c("Which issues need to be addressed to halt CC?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)