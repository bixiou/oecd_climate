# Examples
# Be careful with the arguments (df, miss...)

e <- readRDS("../data/US_pilot_reg_210115.rds")

#source(".Rprofile")

# In case you want to redefine the labels, it need not be automatized, e.g.:
# labels_responsible <- c("Each of us", "The rich", "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "CC doesn't exist")
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 

# (CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
# save_plotly(CC_exists_US)
# # Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F

## Pre-treatment ##

## 1. Demographics
e$gender_factor <- relevel(relevel(as.factor(e$gender), "Other"), "Female")
(gender_US <- barres(vars = "gender_factor", df = e, miss = F, labels="Gender"))
save_plotly(gender_US, width= 470, height=140)

(age_US <- barres(vars = "age_agg", df = e, miss=F, rev = F, labels="Age"))
save_plotly(age_US, width= 470, height=140) # TODO: true age, 35 cutoff

(region_US <- barres(vars = "region", df = e, miss=F, labels="Region"))
save_plotly(region_US, width= 560, height=140)

(race_US <- barres(vars = variables_race[c(1:4)], df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
save_plotly(race_US, width= 330, height=190)

(speak_US <- barres(vars = "speaks_well", df = e, miss=F, labels="How well do you speak english?"))
save_plotly(speak_US, width= 750, height=140)

(education_US <- barres(vars = "education", df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
save_plotly(education_US, width= 880, height=140)

(employment_status_US <- barres(vars = "employment_agg", df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 630, height=140)

## Problem with this one
(hit_by_covid_US <- barres(vars = "hit_by_covid", df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 420, height=140)

(income_US <- barres(vars = "income", df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
save_plotly(income_US, width= 510, height=140)

(wealth_US <- barres(vars = "wealth", df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
save_plotly(wealth_US, width= 480, height=140)

## 2. HH composition and energy characteristics

(nb_children_US <- barres(vars = "nb_children", df = e, miss=F, rev_color = T, rev = F, labels="Number of children"))
save_plotly(nb_children_US, width= 560, height=140)

(heating_US <- barres(vars = "heating", df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
save_plotly(heating_US, width= 525, height=140)

### /!\ Var. cont. TODO
(flights_US <- barres(vars = "flights", df = e, miss=F, labels="How many round-trip flights did you take between 2015 and 2019?"))
save_plotly(flights_US, width= 1194, height=140)

(km_driven_US <- barres(vars = "km_driven", df = e, miss=F, labels="How many km have your household driven in 2019?"))
save_plotly(km_driven_US, width= 1194, height=140) # TODO

(frequency_beef_US <- barres(vars = "frequency_beef", df = e, miss=F, rev = F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 530, height=140)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, df = e, rev = F, miss = T, labels=labels_transport))
save_plotly(transport_US, width= 750, height=235) 

(transport_available_US <- barres(vars = "transport_available", df = e, miss=T, labels="Is public transport available near you live?"))
save_plotly(transport_available_US, width= 850, height=140)

## 3. Trust, perceptions of institutions, etc.

### Should we append the 3 graphs below?
(trust_people_US <- barres(vars = "trust_people", df = e, miss=T, rev_color = T, rev=F, labels="Trust other people"))
save_plotly(trust_people_US, width= 815, height=140)

(trust_govt_US <- barres(vars = "trust_govt", df = e, miss=T, rev_color = T, rev=F, labels="Trust the government<br>  to do what is right"))
save_plotly(trust_govt_US, width= 900, height=140)

(trust_public_spending_US <- barres(vars = "trust_public_spending", df = e, miss=T, labels="Authorities spend revenue in a sensible way"))
save_plotly(trust_public_spending_US, width= 1300, height=140)

(statist_US <- barres(vars = "statist", df = e, miss=T, rev_color = T, rev=F, labels="Pro government intervention"))
save_plotly(statist_US, width= 530, height=140)

(inequality_problem_US <- barres(vars = "inequality_problem", df = e, miss=T, labels="Is inequality a serious problem?"))
save_plotly(inequality_problem_US, width= 1120, height=140)

(future_gdp_US <- barres(vars = "future_gdp", df = e, miss=T, labels="Where do you see the world in 100 years?"))
save_plotly(future_gdp_US, width= 770, height=140)

(envi_US <- barres(vars = "envi", df = e, miss=T, labels="Views on environment"))
save_plotly(envi_US, width= 950, height=140)

## 4. Climate Change (attitudes and risks)

(CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
save_plotly(CC_exists_US,width= 680, height=140)

(CC_dynamics_US <- barres(vars = "CC_dynamics", df = e, miss = T, labels="If we halve global GHG emissions,<br>temperature will..."))
save_plotly(CC_dynamics_US, width= 770, height=140)

labels_CC_factor <- c("Eating one beef steak emits <br> far more than eating two servings of pasta", "Electricity from nuclear power emits <br> far more than electricity produced by wind turbines", "Commuting by car emits far more than food waste")
(CC_factor_US <- barres(vars = variables_CC_factor, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_CC_factor, hover=labels_CC_factor))
save_plotly(CC_factor_US, width= 942, height=200) 


labels_responsible <- c()
for (v in variables_CC_responsible) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_responsible[8] <- "None of the above, <br>climate change is not a reality"
(responsible_US <- barres(vars = variables_CC_responsible, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_responsible, hover=labels_responsible))
save_plotly(responsible_US, width= 575, height=320) 

(CC_stoppable_US <- barres(vars = "CC_stoppable", df = e, miss=T, rev = F, labels="Can humanity stop emitting GHG?"))
save_plotly(CC_stoppable_US, width= 1200, height=140)

(CC_talks_US <- barres(vars = "CC_talks", df = e, miss=T, labels="How often do you <br> talk about climate change?"))
save_plotly(CC_talks_US, width= 540, height=140)

labels_CC_affected <- c("People born in the 1960s", "People born in the 1990s", "People born in the 2020", "People born in the 2050s", "None", "PNR")
(CC_affected_US <- barres(vars = variables_CC_affected, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_CC_affected, hover=labels_CC_affected))
save_plotly(CC_affected_US, width= 470, height=300) 

(CC_affected_min_US <- barres(vars = "CC_affected_min", df = e, rev = F, rev_color = T, miss = T, labels="First US generation seriously affected by CC"))
save_plotly(CC_affected_min_US, width= 750, height=140) 

(change_lifestyle_US <- barres(vars = "change_lifestyle", df = e, miss=T, thin=F, labels="Willing to adopt <br> a sustainable lifestyle"))
save_plotly(change_lifestyle_US, width= 570, height=110)

labels_change_condition <- c("Yes, if policies went in this direction", "Yes, if financial means", "Yes, if everyone did the same", "No, only richest should", "No, changing lifestyle would<br> affect me more than CC", "No, CC not a real problem", "Already have sustainable lifestyle", "I'm trying, but troubles to change", "PNR")
(change_condition_US <- barres(vars = variables_change_condition, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_change_condition, hover=labels_change_condition))
save_plotly(change_condition_US, width= 500, height=350) 

labels_effect_policies <- c("Opportunity for our economy", "Costly, but could maintain our lifestyle", "Require deep change of lifestyle", "PNR")
(effect_policies_US <- barres(vars = variables_effect_policies, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_effect_policies, hover=labels_effect_policies))
save_plotly(effect_policies_US, width= 570, height=200) 

labels_kaya <- c("Technologies that emit GHG", "Level of waste", "High-standards of living", "Overconsumption", "Overpopulation", "None of the above", "Other", "PNR", "")
(kaya_US <- barres(vars = variables_kaya, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_kaya, hover=labels_kaya))
save_plotly(kaya_US, width= 540, height=330) 

## 5. International burden-sharing

labels_scale <- c()
for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_scale[5] <- "PNR"
(scale_US <- barres(vars = variables_scale, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_scale, hover=labels_scale))
save_plotly(scale_US, width= 260, height=250) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_burden_sharing[3] <- "Countries should pay in proportion to their past emissions <br> (from 1990 onwards)"
labels_burden_sharing[4] <- "The richest countries should pay it all, so that<br> the poorest countries do not have to pay anything"
labels_burden_sharing[5] <- "The richest countries should pay even more, <br> vulnerable countries would then receive money instead of paying"
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=T, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1150, height=325) 

(equal_quota_US <- barres(vars = "equal_quota", df = e, miss=T, labels="Should the U.S. take measures <br> to fight climate change?"))
save_plotly(equal_quota_US, width= 930, height=140)

(country_should_act_condition_US <- barres(vars = "country_should_act_condition", df = e, miss=F, labels="You think the US should take measures against CC.<br> How does this depend on what other countries do?"))
save_plotly(country_should_act_condition_US, width= 870, height=140)

labels_pro <- c("Global democratic assembly<br>on climate change", "Global tax on carbon <br> to fund a global basic income", "Global tax on top 1% <br> to finance low-income countries")
(pro_US <- barres(vars = variables_pro, df = e, miss = T, labels=labels_pro, legend=c("Yes ", "No", "PNR")))
save_plotly(pro_US, width= 510, height=250) # TODO: Yes No
