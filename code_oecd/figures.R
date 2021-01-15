# Examples
# Be careful with the arguments (df, miss...)

Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

e <- readRDS("../data/US_pilot_reg_210115.rds")

#source(".Rprofile")

# In case you want to redefine the labels, it need not be automatized, e.g.:
# labels_responsible <- c("Each of us", "The rich", "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "CC doesn't exist")
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 


## Pre-treatment ##

## 1. Demographics
(gender_US <- barres(vars = "gender", df = e, miss = F, labels="What is your gender?"))
save_plotly(gender_US, width= 1194, height=224)

(age_US <- barres(vars = "age_agg", df = e, miss=F, labels="What is your age?"))
save_plotly(age_US, width= 1194, height=224)

(region_US <- barres(vars = "region", df = e, miss=F, labels="In which region do you live?"))
save_plotly(region_US, width= 1194, height=224)
 
(race_US <- barres(vars = "race_white_only", df = e, miss=F, labels="What ethnicity do you identify with?"))
save_plotly(race_US, width= 1194, height=224)

(speak_US <- barres(vars = "speaks_well", df = e, miss=F, labels="How well do you speak english?"))
save_plotly(speak_US, width= 1194, height=224)

(education_US <- barres(vars = "education", df = e, miss=F, labels="What is your highest level  <br> of education completed?"))
save_plotly(education_US, width= 1194, height=224)

(employment_status_US <- barres(vars = "employment_status", df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 1194, height=224)

## Problem with this one
(hit_by_covid_US <- barres(vars = "hit_by_covid", df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 1194, height=224)

(income_US <- barres(vars = "income", df = e, miss=F, labels="What was your HH's annual income in 2019?"))
save_plotly(income_US, width= 1194, height=224)

(wealth_US <- barres(vars = "wealth", df = e, miss=F, labels="What is the estimated wealth of your HH"))
save_plotly(wealth_US, width= 1194, height=224)

## 2. HH composition and energy characteristics

(nb_children_US <- barres(vars = "nb_children", df = e, miss=F, labels="How many children do you have"))
save_plotly(nb_children_US, width= 1194, height=224)

(heating_US <- barres(vars = "heating", df = e, miss=T, labels="What is the main way you heat your home?"))
save_plotly(heating_US, width= 1194, height=224)

### /!\ Var. cont.
(flights_US <- barres(vars = "flights", df = e, miss=F, labels="How many round-trip flights did you take between 2015 and 2019?"))
save_plotly(flights_US, width= 1194, height=224)

(km_driven_US <- barres(vars = "km_driven", df = e, miss=F, labels="How many km have your household driven in 2019?"))
save_plotly(km_driven_US, width= 1194, height=224)

(frequency_beef_US <- barres(vars = "frequency_beef", df = e, miss=F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 1194, height=224)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_transport))
save_plotly(transport_US, width= 942, height=311) 

(transport_available_US <- barres(vars = "transport_available", df = e, miss=T, labels="Is public transport available near you live?"))
save_plotly(transport_available_US, width= 1194, height=224)

## 3. Trust, perceptions of institutions, etc.

### Should we append the 3 graphs below?
(trust_people_US <- barres(vars = "trust_people", df = e, miss=T, labels="Trust other people"))
save_plotly(trust_people_US, width= 1194, height=224)

(trust_govt_US <- barres(vars = "trust_govt", df = e, miss=T, labels="Trust the government to do <br> what is right"))
save_plotly(trust_govt_US, width= 1194, height=224)

(trust_public_spending_US <- barres(vars = "trust_public_spending", df = e, miss=T, labels="Authorities spend revenue in a sensible way"))
save_plotly(trust_public_spending_US, width= 1194, height=224)

(statist_US <- barres(vars = "statist", df = e, miss=T, labels="Government intervention"))
save_plotly(statist_US, width= 1194, height=224)

(inequality_problem_US <- barres(vars = "inequality_problem", df = e, miss=T, labels="Is inequality a serious problem?"))
save_plotly(inequality_problem_US, width= 1194, height=224)

(future_gdp_US <- barres(vars = "future_gdp", df = e, miss=T, labels="Where do you see the world in 100y.?"))
save_plotly(future_gdp_US, width= 1194, height=224)

(envi_US <- barres(vars = "envi", df = e, miss=T, labels="Views on environment"))
save_plotly(envi_US, width= 1194, height=224)

## 4. Climate Change (attitudes and risks)

(CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
save_plotly(CC_exists_US,width= 1194, height=224)

(CC_dynamics_US <- barres(vars = "CC_dynamics", df = e, miss = T, labels="If we halve global GHG emissions…"))
save_plotly(CC_dynamics_US, width= 1194, height=224)

labels_CC_factor <- c("Eating one beef steak emits <br> far more than eating two servings of pasta", "Electricity from nuclear power emits <br> far more than electricity produced by wind turbines", "Commuting by car emits far more than food waste")
(CC_factor_US <- barres(vars = variables_CC_factor, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_CC_factor, hover=labels_CC_factor))
save_plotly(CC_factor_US, width= 942, height=311) 


labels_responsible <- c()
for (v in variables_CC_responsible) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(responsible_US <- barres(vars = variables_CC_responsible, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_responsible, hover=labels_responsible))
save_plotly(responsible_US, width= 942, height=311) 

(CC_stoppable_US <- barres(vars = "CC_stoppable", df = e, miss=T, labels="Stop emitting GHG"))
save_plotly(CC_stoppable_US, width= 1194, height=224)

(CC_talks_US <- barres(vars = "CC_talks", df = e, miss=T, labels="How often do you <br> talk about climate change?"))
save_plotly(CC_talks_US, width= 1194, height=224)

labels_CC_affected <- c("People born in the 1960s", "People born in the 1990s", "People born in the 2020", "People born in the 2050s", "None", "PNR")
(CC_affected_US <- barres(vars = variables_CC_affected, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_CC_affected, hover=labels_CC_affected))
save_plotly(CC_affected_US, width= 942, height=311) 

(change_lifestyle_US <- barres(vars = "change_lifestyle", df = e, miss=T, labels="Willing to adopt <br> a sustainable lifestyle?"))
save_plotly(change_lifestyle_US, width= 1194, height=224)

labels_change_condition <- c("Yes, if policies went in this direction", "Yes, if financial means", "Yes, if everyone did the same", "No, only richest should", "No, changing lifestyle would affect me more than CC", "No, CC not a real problem", "Already have sustainable lifestyle", "I'm trying, but troubles to change", "PNR")
(change_condition_US <- barres(vars = variables_change_condition, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_change_condition, hover=labels_change_condition))
save_plotly(change_condition_US, width= 942, height=311) 

labels_effect_policies <- c("Opportunity for our economy", "Costly, but could maintain our lifestyle", "Require deep change of lifestyle", "PNR")
(effect_policies_US <- barres(vars = variables_effect_policies, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_effect_policies, hover=labels_effect_policies))
save_plotly(effect_policies_US, width= 942, height=311) 

labels_kaya <- c("Technologies that emit GHG", "Level of waste", "High-standards of living", "Overconsumption", "Overpopulation", "None of the above", "Other", "PNR", "")
(kaya_US <- barres(vars = variables_kaya, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_kaya, hover=labels_kaya))
save_plotly(kaya_US, width= 942, height=311) 

## 5. International burden-sharing

labels_scale <- c()
for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(scale_US <- barres(vars = variables_scale, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_scale, hover=labels_scale))
save_plotly(scale_US, width= 942, height=311) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=T, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1194, height=311) 

<<<<<<< Updated upstream
(CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
save_plotly(CC_exists_US)
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 

=======
(equal_quota_US <- barres(vars = "equal_quota", df = e, miss=T, labels="Should the U.S. take measures <br> to fight climate change?"))
save_plotly(equal_quota_US, width= 1194, height=224)

(country_should_act_condition_US <- barres(vars = "country_should_act_condition", df = e, miss=F, labels="How does this depend <br> on what other countries do?"))
save_plotly(country_should_act_condition_US, width= 1194, height=224)

variables_pro <<- names(e)[grepl('^pro_', names(e))]
labels_pro <- c("Would approve global assembly", " Would approve global tax on carbon <br> to fund a global basic income", "Would approve global tax on top 1% <br> to finance low-income countries")
(pro_US <- barres(vars = variables_pro, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_pro))
save_plotly(pro_US, width= 1194, height=224) 
>>>>>>> Stashed changes
