# Nb: condition is > 0. Might be different for other variables
# Return a df with mean and CI for each sub-group
heterogeneity_mean_CI <- function(variable_name, heterogeneity_group, df=e, weights = "weight"){
  # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
  mean_sd <- as.data.frame(sapply(split(df, df[[heterogeneity_group]]), function(x) c(wtd.mean(x[[variable_name]] > 0, w = x[[weights]], na.rm=T)*100, sqrt(wtd.var(x[[variable_name]] > 0, w = x[[weights]], na.rm=T))/sqrt(sum(x[[weights]]))*100)))
  # Get low and high bounds of CI. For the moment only 10% CI
  mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-1.645*x[2], x[1]+1.645*x[2]))))
  mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
  mean_sd$policy <- variable_name
  
  return(mean_sd)
}

# e <- us
e <- fr
# e <- dk
variables_list <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
#variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")

# Apply to get df with info on each variable
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

## Plot creation
#mean_sd <- subset(mean_sd, political_affiliation %in% c("Democrat", "Republican"))
policies_label <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

# 120 130 vert
support_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
#  scale_color_brewer(breaks = unique(mean_sd$left_right), direction = +1, palette="RdYlBu")
#  scale_color_hue(breaks = unique(mean_sd$left_right), direction = -1, palette="RdBu") 
support_by_political_FR
# /!\ Need to be saved manually
#save_plotly(support_by_political_US, width= 800, height=400)


## Dépensent bcp en énergie
## Rural
## Droite

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

support_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
support_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

support_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
support_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#ABD9E9"))
support_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
support_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
support_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
support_by_heating_expenses_FR


### WILLINGNESS PART


#Set-up
variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")

policies_label <- c("WTP", "Limit flying", "Limit driving", "Have electric car", "Limit heating or cooling home", "Limit beef consumption")

# Graphs
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

willing_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
willing_by_political_FR

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

willing_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
willing_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

willing_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
willing_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not Polluting", "Polluting"),values = c("#FDAE61", "#ABD9E9"))
willing_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
willing_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
willing_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
willing_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
willing_by_incomes_FR

### ALL POSITIVE ANSWERS PART

# heterogeneity_mean_CI_01 <- function(variable_name, heterogeneity_group, df=e, weights = "weight"){
#   # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
#   mean_sd <- as.data.frame(sapply(split(df, df[[heterogeneity_group]]), function(x) c(wtd.mean(x[[variable_name]], w = x[[weights]], na.rm=T)*100, sqrt(wtd.var(x[[variable_name]], w = x[[weights]], na.rm=T))/sqrt(sum(x[[weights]]))*100)))
#   # Get low and high bounds of CI. For the moment only 10% CI
#   mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-1.645*x[2], x[1]+1.645*x[2]))))
#   mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
#   mean_sd$policy <- variable_name
#   
#   return(mean_sd)
# }
# mean_sd <- bind_rows((lapply(variables_list_index, heterogeneity_mean_CI_01,
#                                           heterogeneity_group = "left_right", df=e, weights = "weight")))
# variables_list_index <- c("index_knowledge" ,"index_affected")

#Set-up
variables_list <- c("CC_anthropogenic","willing_limit_driving","wtp",
                    "standard_support","standard_public_transport_support",
                    "investments_support","tax_transfers_support",
                    "beef_ban_intensive_support","insulation_support",
                    "tax_1p_support")

policies_label <- c("Climate change is anthropogenic", "Willing to limit driving",
                    "Willing to Pay for climate action", "Support ban on combustion engine",
                    "Support ban on combustion engine \n (public transport made available)",
                    "Support green investments program", "Support carbon tax with cash transfer",
                    "Support ban on intensive cattle farming", "Support mandatory insulation",
                    "Support global wealth tax to fund LDCs")
  
# Graphs
mean_sd <- bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

positive_all_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
positive_all_by_political_FR

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

positive_all_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
positive_all_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

positive_all_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
positive_all_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not Polluting", "Polluting"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_income_FR

# Age
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "age", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_age_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = age, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(5, "RdBu")))
positive_all_by_age_FR

# Diploma
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "diploma", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_diploma_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = diploma, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_diploma_FR

# Female
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "female", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_female_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = female, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Male", "Female"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_female_FR

# Yellow Vests
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "Gilets_jaunes_agg", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$Gilets_jaunes_agg <- factor(mean_sd$Gilets_jaunes_agg, levels = c("oppose", "NSP", "comprend", "soutient", "est_dedans"))


positive_all_by_yellow_vests_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = Gilets_jaunes_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Oppose", "Understand", "Support", "Participate", "PNR"),values = c(brewer.pal(4, "RdBu")), na.value = "grey70")
positive_all_by_yellow_vests_FR

# CC anthropogenic
e$CC_anthropogenic_dummy <- e$CC_anthropogenic > 0
mean_sd <- bind_rows((lapply(variables_list[2:10], heterogeneity_mean_CI, 
                             heterogeneity_group = "CC_anthropogenic_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list[2:10], labels = policies_label[2:10]))

positive_all_by_CC_anthropogenic_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = CC_anthropogenic_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("CC is not anthropogenic", "CC is anthropogenic"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_CC_anthropogenic_FR

mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

#Children
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_children_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = children, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("No child", "child(ren) at home"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_children_FR

#Employment
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "employment_status", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_employment_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = employment_status, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(7, "RdBu")))
positive_all_by_employment_FR
