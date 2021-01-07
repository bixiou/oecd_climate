##### Metadata #####
decrit("finished", data = e)
decrit("excluded", data = e)
decrit("language", data = e)
decrit("urban_category", data = e) 
decrit("treatment_policy", data = e)
decrit("treatment_climate", data = e)
decrit("variant_flight_quota", data = e)
any(duplicated(e$PSID))


##### Durations ######
decrit("duration", data = e) # median 19 min / mean 23.5
decrit("duration_politics_field", data = e) # median 20s / mean 1 min
decrit("duration_CC_field", data = e) # median 20s / mean < 1 min
decrit("duration_policies_field", data = e) # median 20s / mean < 1 min
decrit("duration_burden_sharing", data = e) # 1-2 min
decrit("duration_treatment_climate", data = e) # TODO
decrit("duration_treatment_policy", data = e)
decrit("duration_tax_transfers", data = e) # < 1 min
decrit("duration_policies", data = e) # 1 min


##### Socio-demographics ####
decrit("gender", data = e)
decrit("age", data = e)
decrit("region", data = e)
decrit("zipcode", data = e)
decrit("urbanity", data = e) # 35% rural
decrit("urban_category", data = e)
for (v in variables_race) print(decrit(v, data = e)) # 4% black, 6% hispanic, 3% PNR or other
decrit("speaks_well", data = e)
decrit("education", data = e) # few low education
decrit("employment_status", data = e) # TODO
decrit("hit_by_covid", data = e)
decrit("income", data = e)
for (v in variables_home) print(decrit(v, data = e))
decrit("wealth", data = e) # TODO label
decrit("nb_children", data = e)
decrit("hh_children", data = e)
decrit("hh_adults", data = e)
decrit("heating", data = e) # 44% Elec TODO check
decrit("km_driven", data = e)
decrit("flights", data = e)
decrit("frequency_beef", data = e) # 7/23/54/17%
for (v in variables_transport) print(decrit(v, data = e)) # available but using car


##### Trust, perceptions of institutions, inequality, and the future ######
decrit("trust_people", data = e)
decrit("trust_govt", data = e)
decrit("trust_public_spending", data = e)
decrit("statist", data = e)
decrit("inequality_problem", data = e)
decrit("future_gdp", data = e) 
decrit("envi", data = e)


##### Essay: politics ######
decrit("politics_field", data = e)


##### Essay: climate change ######
decrit("CC_field", data = e)


##### Essay: climate policies ######
decrit("policies_field", data = e) # TODO length


##### Climate change (attitudes and risks) ######
decrit("CC_exists", data = e) # 50% anthropogenic
decrit("CC_dynamics", data = e) # 47% good
for (v in variables_CC_factor) print(decrit(v, data = e)) # 40-70% true
for (v in variables_CC_responsible) print(decrit(v, data = e))
decrit("CC_stoppable", data = e) 
decrit("CC_talks", data = e) 
for (v in variables_CC_affected) print(decrit(v, data = e)) # less in 2050 than 2020!
decrit("change_lifestyle", data = e) # 60%
for (v in variables_change_condition) print(decrit(v, data = e))
for (v in variables_effect_policies) print(decrit(v, data = e))
for (v in variables_kaya) print(decrit(v, data = e))


##### International burden-sharing ######
for (v in variables_scale) print(decrit(v, data = e))
for (v in variables_burden_sharing) print(decrit(v, data = e)) # Weird that last line > second to last
decrit("equal_quota", data = e) # 49% No, not individual level. TODO: Change?
decrit("country_should_act", data = e)
decrit("country_should_act_condition", data = e) # TODO order
decrit("pro_global_assembly", data = e)
decrit("pro_global_tax", data = e)
decrit("pro_tax_1p", data = e)


##### Treatment effects #####
end_formula <- paste(paste(variables_main_controls, collapse = ' + '), " + treatment_climate * treatment_policy")
summary(lm(standard_exists=="Yes" ~ treatment_climate * treatment_policy, data = e)) # /!\ effect although we expect none
summary(lm(standard_support=="Yes" ~ treatment_climate * treatment_policy, data = e))
summary(lm(investments_support=="Yes" ~ treatment_climate * treatment_policy, data = e))
summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy, data = e)) # /!\ negative effect of climate video
summary(lm(tax_transfers_support=="Yes" ~ treatment_climate, data = e))
summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
summary(lm(policies_trust ~ treatment_climate * treatment_policy, data = e))
summary(lm(policies_effective ~ treatment_climate * treatment_policy, data = e))
summary(lm(policies_employment ~ treatment_climate * treatment_policy, data = e))
summary(lm(policies_side_effects ~ treatment_climate * treatment_policy, data = e))
summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
summary(lm(policies_incidence ~ treatment_climate * treatment_policy, data = e))
summary(lm(as.formula(paste("standard_exists=='Yes' ~", end_formula)), data = e)) 
summary(lm(as.formula(paste("standard_support=='Yes' ~", end_formula)), data = e))
summary(lm(as.formula(paste("investments_support=='Yes' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~", end_formula)), data = e)) 
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ treatment_climate +", paste(variables_main_controls, collapse = ' + '))), data = e)) 
summary(lm(as.formula(paste("policies_support ~", end_formula)), data = e)) 
summary(lm(as.formula(paste("policies_trust ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_effective ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_employment ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_side_effects ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_self ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_incidence ~", end_formula)), data = e))
# Correlations of support with motives
summary(lm(policies_support ~ policies_trust + policies_effective + policies_employment + policies_side_effects + 
             policies_self + policies_incidence + treatment_climate * treatment_policy, data = e))
summary(lm(standard_support=='Yes' ~ standard_employment + standard_side_effects + 
             standard_incidence_self + treatment_climate * treatment_policy, data = e)) # bug with standard_effective + standard_trust : colinearity?
summary(lm(investments_support=='Yes' ~ investments_employment + investments_side_effects + 
            investments_incidence_self + treatment_climate * treatment_policy, data = e))
summary(lm(tax_transfers_support=='Yes' ~ tax_transfers_employment + tax_transfers_side_effects + 
             tax_transfers_incidence_self + treatment_climate * treatment_policy, data = e))
summary(lm(as.formula(paste("policies_support ~ policies_trust + policies_effective + policies_employment + policies_side_effects + standard_incidence_self +", end_formula)), data = e)) # negative effect of policy_treatment, indicating that effect is mediating by a motive
summary(lm(as.formula(paste("standard_support=='Yes' ~ standard_employment + standard_side_effects + standard_incidence_self +", end_formula)), data = e))
summary(lm(as.formula(paste("investments_support=='Yes' ~ investments_employment + investments_side_effects + investments_incidence_self +", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ tax_transfers_employment + tax_transfers_side_effects + tax_transfers_incidence_self +", end_formula)), data = e))


##### Preference 1: emission limit for cars ######
decrit("standard_exists", data = e)
decrit("standard_trust", data = e)
decrit("standard_effective", data = e)
decrit("standard_employment", data = e)
decrit("standard_side_effects", data = e)
for (v in variables_standard_incidence) print(decrit(v, data = e))
decrit("standard_support", data = e)


##### Preference 2: green infrastructure program ######
decrit("investments_trust", data = e)
decrit("investments_effective", data = e)
decrit("investments_employment", data = e)
decrit("investments_side_effects", data = e)
for (v in variables_investments_incidence) print(decrit(v, data = e))
decrit("investments_support", data = e)


##### Preference 3: carbon tax with cash transfers ######
decrit("tax_transfers_trust", data = e)
decrit("tax_transfers_effective", data = e)
decrit("tax_transfers_employment", data = e)
decrit("tax_transfers_side_effects", data = e)
for (v in variables_tax_transfers_incidence) print(decrit(v, data = e))
decrit("tax_transfers_support", data = e)


##### Preferences on climate policies ######
for (v in variables_policy) print(decrit(v, data = e))
for (v in variables_tax) print(decrit(v, data = e)) 


##### Preference for bans vs. incentives ######
decrit("insulation_compulsory", data = e)
for (v in variables_flight_quota) print(decrit(v, data = e))
for (v in variables_beef) print(decrit(v, data = e))
decrit("ban_incentives", data = e)


##### WTP ######
decrit("wtp", data = e)


##### Poltical views and media consumption ######
decrit("interest_politics", data = e)
decrit("member_environmental_orga", data = e)
decrit("relative_environmentalist", data = e)
decrit("left_right", data = e)
decrit("Left_right", data = e)
for (v in variables_political_identity) print(decrit(v, data = e))
decrit("media", data = e)
decrit("vote_participation", data = e)
decrit("vote", data = e)
decrit("vote_other", data = e)


##### Feedback ######
decrit("survey_biased", data = e) # 40% Pro-envi biased. TODO: Change?
decrit("comment_field", data = e) # Most don't leave a comment, many "Good survey", many (but less) critics that it's pro-envi biased, a few critics that some options are missing (but no example)
