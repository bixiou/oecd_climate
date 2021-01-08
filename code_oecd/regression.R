library(haven)
library(xtable)
library(stargazer)

setwd("/Users/Bluebii/Code/GitHub/oecd_climate/code_oecd")

df <- readRDS("../data/US_pilot_clean.rds") # Can we call it "us" instead? Or "e", as in the other files? It's cumbersome if we have two clones of the same dataframe.


##### 1. Creation control variables #####

# TODO: all these variables should better be created inside "convert" in preparation.
# race TODO: problem: someone can be at the same time Hispanic and black or white. Why don't you keep the dummies race_white, race_black, race_hispanic?
df$race <- NULL
df[df$race_white == TRUE, "race"] <- "White"
df[df$race_black == TRUE, "race"] <- "Black"
df[df$race_hispanic == TRUE, "race"] <- "Hispanic"
df[df$race_asian == TRUE | df$race_native == TRUE | df$race_hawaii == TRUE | df$race_other_choice == TRUE | df$race_pnr == TRUE , "race"] <- "Other"

#gender: delete 'other', as only 1 obs in it TODO: I think it's incorrect to delete observations. One obs. will not change the result anyway, so you can just use Gender=Female (or Male) as your dummy
df <- df[!(df$gender=="Other"),]

# children
df$children <- 0
df[df$nb_children >= 1, "children"] <- 1

# college
df$college <- "No college"
df[df$education >= 5, "college"] <- "College Degree"

# employment
df$employment_agg <-  "Not working"
df[df$employment_status == "Student", "employment_agg"] <- "Student"
df[df$employment_status == "Retired", "employment_agg"] <- "Retired"
df[df$employment_status == "Self-employed" | df$employment_status == "Full-time employed" |df$employment_status == "Part-time employed", "employment_agg"] <- "Working"

# age
df$age_agg <- NULL
df[df$age %in% 18:29, "age_agg"] <- "18-29"
df[df$age %in% 30:49, "age_agg"] <- "30-49"
df[df$age %in% 50:87, "age_agg"] <- "50-87"

# political position
df$vote_dum <- df$vote # TODO Many people define themselves as Center, why saying "None"? It's not the same thing as Indeterminate. I'd rather use Biden vs. Trump for this control.
df[df$far_left == TRUE | df$left == TRUE | df$liberal == TRUE, "pol_agg"] <- "Biden"
df[df$far_right == TRUE | df$right == TRUE | df$conservative == TRUE, "pol_agg"] <- "Conservative"

# treatment
df$treatment_agg <- NULL
df[df$treatment_policy == 0 & df$treatment_climate == 0, "treatment_agg"] <- "None"
df[df$treatment_policy == 0 & df$treatment_climate == 1, "treatment_agg"] <- "Climate treatment only"
df[df$treatment_policy == 1 & df$treatment_climate == 0, "treatment_agg"] <- "Policy treatment only"
df[df$treatment_policy == 1 & df$treatment_climate == 1, "treatment_agg"] <- "Both"

# Controls var as factors
df$race <- as.factor(df$race)
df$gender <- as.factor(df$gender)
df$children <- as.factor(df$children)
df$college <- as.factor(df$college)
df$employment_agg <- as.factor(df$employment_agg)
df$income_factor <- as.factor(df$income)
df$age_agg <- as.factor(df$age_agg)
df$pol_agg <- as.factor(df$pol_agg)
df$treatment_agg <- as.factor(df$treatment_agg)

##### 2. Regressions #####

control_variables <- c("race", "gender", "children", "college", "employment_agg", "income", "age_agg", "pol_agg", "treatment_agg")
cov_lab = c("Hispanic", "Other", "White", "Male","Children","No college","Retired" ,"Student", "Working", "Income Q2", "Income Q3", "Income Q4","30-49", "50-87", "Liberal", "None", "Climate treatment only", "No treatment", "Policy treatment only")

## Cause of CC
df$CC_exists_no <- (df$CC_exists == -1)
df$CC_exists_nat <- (df$CC_exists == 0)
df$CC_exists_anthro <- (df$CC_exists == 1)

# TODO you can automatize this process using the function below
# I have stopped here but we can automatize further, e.g. to generate CC_exists_no, etc. automatically from CC_exists' levels (ask me if needed)
# (In general, we don't need to redefine dummies for each variable level and it's better to have a shorter, more readable code.)
desc_table <- function(dep_vars, filename = NULL, data = df, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL,
                       save_folder = "../tables/", dep.var.labels = dep_vars, dep.var.caption = NULL, digits= 3) {
  models <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    models[[i]] <- lm(as.formula(paste(dep_vars[i], "~", paste(indep_vars, collapse = '+'))), data = data, weights = weights)
    means[i] <- round(mean(df[dep_vars[i]], na.rm = T), d = digits)
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

temp <- desc_table(dep_vars = c("CC_exists_no", "CC_exists_nat", "CC_exists_anthro"), filename = "CC_exists",
           dep.var.labels = c("not a reality","mainly due to natural climate variability", "mainly due to human activity"),
           dep.var.caption = c("Climate change is…"), data = df, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

model1 = lm(CC_exists_no ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(CC_exists_nat ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(CC_exists_anthro ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$CC_exists_no, na.rm = T), d=3)
mean2 = round(mean(df$CC_exists_nat, na.rm = T), d=3)
mean3 = round(mean(df$CC_exists_anthro, na.rm = T), d=3)

stargazer(model1, model2, model3, out="../output/tables/CC_exists.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3)),
          dep.var.labels = c("not a reality","mainly due to natural climate variability", "mainly due to human activity"),
          dep.var.caption = c("Climate change is…"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Halving GHG
df$CC_dynamics_no <- (df$CC_dynamics == -2)
df$CC_dynamics_dec <- (df$CC_dynamics == -1)
df$CC_dynamics_stab <- (df$CC_dynamics == 0)
df$CC_dynamics_rise <- (df$CC_dynamics == 1)

model1 = lm(CC_dynamics_no ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(CC_dynamics_dec ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(CC_dynamics_stab ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(CC_dynamics_rise ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$CC_dynamics_no, na.rm = T), d=3)
mean2 = round(mean(df$CC_dynamics_dec, na.rm = T), d=3)
mean3 = round(mean(df$CC_dynamics_stab, na.rm = T), d=3)
mean4 = round(mean(df$CC_dynamics_rise, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, out="../output/tables/CC_dynamics.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4)),
          dep.var.labels = c("has no impact on temperatures","will decrease temperatures", "will stabilize temperatures", "will increase temperatures, just more slowly"),
          dep.var.caption = c("Halving global GHG emissions"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Comparisons GHG questions
df$dummy_CC_factor_beef <- (df$CC_factor_beef == TRUE)
df$dummy_CC_factor_nuclear <- (df$CC_factor_nuclear == TRUE)
df$dummy_CC_factor_car <- (df$CC_factor_car == TRUE)

model1 = lm(dummy_CC_factor_beef ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_CC_factor_nuclear ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_CC_factor_car ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)


mean1 = round(mean(df$dummy_CC_factor_beef, na.rm = T), d=3)
mean2 = round(mean(df$dummy_CC_factor_nuclear, na.rm = T), d=3)
mean3 = round(mean(df$dummy_CC_factor_car, na.rm = T), d=3)

stargazer(model1, model2, model3, out="../output/tables/comparisons_GHG.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3)),
          dep.var.labels = c("eating beef vs. two servings of pasta","eletricity produced by nuclear power vs. wind turbines",  "commuting by car vs. food waste"),
          dep.var.caption = c("Does this activity emits fare more GHG than this other one?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Responsible party for CC
df$dummy_CC_responsible_each <- (df$CC_responsible_each == TRUE) # TODO: these lines are useless, you are just duplicating the variables
df$dummy_CC_responsible_rich <- (df$CC_responsible_rich == TRUE) # Same everyone where you have ... <- ... == TRUE
df$dummy_CC_responsible_govts <- (df$CC_responsible_govts == TRUE)
df$dummy_CC_responsible_companies <- (df$CC_responsible_companies == TRUE)
df$dummy_CC_responsible_past <- (df$CC_responsible_past == TRUE)
df$dummy_CC_responsible_foreign <- (df$CC_responsible_foreign == TRUE)
df$dummy_CC_responsible_nature <- (df$CC_responsible_nature == TRUE)
df$dummy_CC_responsible_denial <- (df$CC_responsible_denial == TRUE)

model1 = lm(dummy_CC_responsible_each ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_CC_responsible_rich ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_CC_responsible_govts ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(dummy_CC_responsible_companies ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model5 = lm(dummy_CC_responsible_past ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model6 = lm(dummy_CC_responsible_foreign ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model7 = lm(dummy_CC_responsible_nature ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model8 = lm(dummy_CC_responsible_denial ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$dummy_CC_responsible_each, na.rm = T), d=3)
mean2 = round(mean(df$dummy_CC_responsible_rich, na.rm = T), d=3)
mean3 = round(mean(df$dummy_CC_responsible_govts, na.rm = T), d=3)
mean4 = round(mean(df$dummy_CC_responsible_companies, na.rm = T), d=3)
mean5 = round(mean(df$dummy_CC_responsible_past, na.rm = T), d=3)
mean6 = round(mean(df$dummy_CC_responsible_foreign, na.rm = T), d=3)
mean7 = round(mean(df$dummy_CC_responsible_nature, na.rm = T), d=3)
mean8 = round(mean(df$dummy_CC_responsible_denial, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, out="../output/tables/responsible_GHG.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4, mean5, mean6, mean7, mean8)),
          dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "Climate change is not a reality"),
          dep.var.caption = c("Which of the following is predominantly responsible for CC?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Possible to halt CC
df$CC_stoppable_no_infl <-- (df$CC_stoppable == "No influence")
df$CC_stoppable_adapt <-- (df$CC_stoppable == "Better to adapt")
df$CC_stoppable_should <-- (df$CC_stoppable == "Should but not happening")
df$CC_stoppable_policies <-- (df$CC_stoppable == "Policies & awareness will")
df$CC_stoppable_progress <-- (df$CC_stoppable == "Progress will suffice")

model1 = lm(CC_stoppable_no_infl ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(CC_stoppable_adapt ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(CC_stoppable_should ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(CC_stoppable_policies ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model5 = lm(CC_stoppable_progress ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)


mean1 = round(mean(df$CC_stoppable_no_infl, na.rm = T), d=3)
mean2 = round(mean(df$CC_stoppable_adapt, na.rm = T), d=3)
mean3 = round(mean(df$CC_stoppable_should, na.rm = T), d=3)
mean4 = round(mean(df$CC_stoppable_policies, na.rm = T), d=3)
mean5 = round(mean(df$CC_stoppable_progress, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, model5, out="../output/tables/CC_stoppable.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4, mean5)),
          dep.var.labels = c("Human have no noticeable influence","Better live with CC than try to halt it",  "Should stop emmissions, but not going to happen", "Ambitious policies and awareness will succeed", "Technologies and habits will suffice"),
          dep.var.caption = c("Can humanity halt CC?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Talks about CC
df$CC_talks_never <-- (df$CC_talks == -1)
df$CC_talks_yearly <-- (df$CC_talks == 0)
df$CC_talks_monthly <-- (df$CC_talks == 1)


model1 = lm(CC_talks_never ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(CC_talks_yearly ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(CC_talks_monthly ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)


mean1 = round(mean(df$CC_talks_never, na.rm = T), d=3)
mean2 = round(mean(df$CC_talks_yearly, na.rm = T), d=3)
mean3 = round(mean(df$CC_talks_monthly, na.rm = T), d=3)

stargazer(model1, model2, model3, out="../output/tables/CC_talks.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3)),
          dep.var.labels = c("Never","Yearly",  "Monthly"),
          dep.var.caption = c("How often do you talk about CC?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Generations most affected
df$dummy_CC_affected_1960 <-- (df$CC_affected_1960 == TRUE)
df$dummy_CC_affected_1990 <-- (df$CC_affected_1990 == TRUE)
df$dummy_CC_affected_2020 <-- (df$CC_affected_2020 == TRUE)
df$dummy_CC_affected_2050 <-- (df$CC_affected_2050 == TRUE)
df$dummy_CC_affected_none <-- (df$CC_affected_none == TRUE)

model1 = lm(dummy_CC_affected_1960 ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_CC_affected_1990 ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_CC_affected_2020 ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(dummy_CC_affected_2050 ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model5 = lm(dummy_CC_affected_none ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)


mean1 = round(mean(df$dummy_CC_affected_1960, na.rm = T), d=3)
mean2 = round(mean(df$dummy_CC_affected_1990, na.rm = T), d=3)
mean3 = round(mean(df$dummy_CC_affected_2020, na.rm = T), d=3)
mean4 = round(mean(df$dummy_CC_affected_2050, na.rm = T), d=3)
mean5 = round(mean(df$dummy_CC_affected_none, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, model5, out="../output/tables/CC_affected.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4, mean5)),
          dep.var.labels = c("Born in 1960s", "Born in 1990s", "Born in 2020s", "Born in 2050s", "None of them"),
          dep.var.caption = c("Which generations will be seriously affected by CC?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Sustainable lifestyle
df$dummy_change_lifestyle <-- (df$change_lifestyle == "Yes")

model1 = lm(dummy_CC_affected_1960 ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
mean1 = round(mean(df$dummy_CC_affected_1960, na.rm = T), d=3)

stargazer(model1, out="../output/tables/change_lifestyle.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1)),
          dep.var.labels = c("Willing to change lifestyle"),
          dep.var.caption = c("Scenario: world consensus to fight CC and wider green transports and energy available"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Willing to change lifestyle
df$dummy_change_condition_policies <-- (df$change_condition_policies == TRUE)
df$dummy_change_condition_income_factor <-- (df$change_condition_income_factor == TRUE)
df$dummy_change_condition_all <-- (df$change_condition_all == TRUE)
df$dummy_change_condition_no_rich <-- (df$change_condition_no_rich == TRUE)
df$dummy_change_condition_no_selfish <-- (df$change_condition_no_selfish == TRUE)
df$dummy_change_condition_no_denial <-- (df$change_condition_no_denial == TRUE)
df$dummy_change_condition_already <-- (df$change_condition_already == TRUE)
df$dummy_change_condition_try <-- (df$change_condition_try == TRUE)

model1 = lm(dummy_change_condition_policies ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_change_condition_income_factor ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_change_condition_all ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(dummy_change_condition_no_rich ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model5 = lm(dummy_change_condition_no_selfish ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model6 = lm(dummy_change_condition_no_denial ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model7 = lm(dummy_change_condition_already ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model8 = lm(dummy_change_condition_try ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$dummy_change_condition_policies, na.rm = T), d=3)
mean2 = round(mean(df$dummy_change_condition_income_factor, na.rm = T), d=3)
mean3 = round(mean(df$dummy_change_condition_all, na.rm = T), d=3)
mean4 = round(mean(df$dummy_change_condition_no_rich, na.rm = T), d=3)
mean5 = round(mean(df$dummy_change_condition_no_selfish, na.rm = T), d=3)
mean6 = round(mean(df$dummy_change_condition_no_denial, na.rm = T), d=3)
mean7 = round(mean(df$dummy_change_condition_already, na.rm = T), d=3)
mean8 = round(mean(df$dummy_change_condition_try, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, out="../output/tables/change_condition.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4, mean5, mean6, mean7, mean8)),
          dep.var.labels = c("Yes, if policies in the good direction","Yes, if financial means",  "Yes, if everyone does the same", "No, only rich should", "No, would affect me more than living with CC", "No, CC not a real problem", "Lifestyle already sustainable", "Trying, but trouble to change"),
          dep.var.caption = c("Would you be willing to change your lifestyle?"),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Effect of policies
df$dummy_effect_policies_opportunity <- (df$effect_policies_opportunity == TRUE)
df$dummy_effect_policies_cost <- (df$effect_policies_cost == TRUE)
df$dummy_effect_policies_lifestyle <- (df$effect_policies_lifestyle == TRUE)

model1 = lm(dummy_effect_policies_opportunity ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_effect_policies_cost ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_effect_policies_lifestyle ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$dummy_effect_policies_opportunity, na.rm = T), d=3)
mean2 = round(mean(df$dummy_effect_policies_cost, na.rm = T), d=3)
mean3 = round(mean(df$dummy_effect_policies_lifestyle, na.rm = T), d=3)

stargazer(model1, model2, model3, out="../output/tables/effect_policies.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3)),
          dep.var.labels = c("be an opportunity for our economy and improve our lifestyle","be costly, but we would maintain our lifestyle", "would require deep change in our lifestyle"),
          dep.var.caption = c("The policies aimed at halting CC would "),
          multicolumn = F, float = F, keep.stat = c("n")
)

## Issues to address to halt CC
df$dummy_kaya_techno <-- (df$kaya_techno == TRUE)
df$dummy_kaya_waste <-- (df$kaya_waste == TRUE)
df$dummy_kaya_wealth <-- (df$kaya_wealth == TRUE)
df$dummy_kaya_overconsumption <-- (df$kaya_overconsumption == TRUE)
df$dummy_kaya_overpopulation <-- (df$kaya_overpopulation == TRUE)
df$dummy_kaya_none <-- (df$kaya_none == TRUE)
#df$dummy_kaya_other_choice <-- (df$kaya_other_choice == TRUE)

model1 = lm(dummy_kaya_techno ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model2 = lm(dummy_kaya_waste ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model3 = lm(dummy_kaya_wealth ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model4 = lm(dummy_kaya_overconsumption ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model5 = lm(dummy_kaya_overpopulation ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
model6 = lm(dummy_kaya_none ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)
#model7 = lm(dummy_kaya_other_choice ~ race + gender + children + college + employment_agg + income_factor + age_agg + pol_agg + treatment_agg, data=df)

mean1 = round(mean(df$dummy_kaya_techno, na.rm = T), d=3)
mean2 = round(mean(df$dummy_kaya_waste, na.rm = T), d=3)
mean3 = round(mean(df$dummy_kaya_wealth, na.rm = T), d=3)
mean4 = round(mean(df$dummy_kaya_overconsumption, na.rm = T), d=3)
mean5 = round(mean(df$dummy_kaya_overpopulation, na.rm = T), d=3)
mean6 = round(mean(df$dummy_kaya_none, na.rm = T), d=3)
#mean7 = round(mean(df$dummy_kaya_other_choice, na.rm = T), d=3)

stargazer(model1, model2, model3, model4, model5, model6, out="../output/tables/kaya.tex", header=F,
          covariate.labels = cov_lab, add.lines =list(c("Mean", mean1, mean2, mean3, mean4, mean5, mean6)),
          dep.var.labels = c("Use of technologies that emit GHG", "Level of waste", "High standards of living", "Overconsumption", "Overpopulation", "None of them"),
          dep.var.caption = c("Which issues need to be addressed to halt CC?"),
          multicolumn = F, float = F, keep.stat = c("n")
)